location = "~/github"
path= "/ml-lab/scripts/"
setwd(paste(location, path, sep=""))

library(tm) 
library(maptpx)
library(irlba)
library(Matrix)


source("../utilities/convert_to_string.R")
data = read.csv("../data/multiple_choice_training_set.tsv", sep="\t")


#separate dataset into 4x as many rows, with dummy variable for "correct" or "incorrect", as well as "negation"
dfa = data.frame(question=data$question, answer=data$answerA, correct=(data$correctAnswer == 'A'))
dfb = data.frame(question=data$question, answer=data$answerB, correct=(data$correctAnswer == 'B'))
dfc = data.frame(question=data$question, answer=data$answerC, correct=(data$correctAnswer == 'C'))
dfd = data.frame(question=data$question, answer=data$answerD, correct=(data$correctAnswer == 'D'))
df = rbind(dfa, dfb, dfc, dfd)

df$answer = convert_to_string(df$answer)
df$question = convert_to_string(df$question)

#indentify questions with a negation
df$negation = FALSE
df[grep(" not ", df$question),"negation"] = TRUE
df[grep(" except ", df$question),"negation"] = TRUE
head(df[df$negation,]$question)

#build topic model for questions 
src = VectorSource(df$question)
questions = Corpus(src)
## tm_map just maps some function to every document in the corpus
questions <- tm_map(questions, content_transformer(tolower)) ## make everything lowercase
#questions <- tm_map(questions, content_transformer(removeNumbers)) ## remove numbers
questions <- tm_map(questions, content_transformer(removePunctuation)) ## remove punctuation
## remove stopword.  be careful with this: one's stopwords are anothers keywords.
questions <- tm_map(questions, content_transformer(removeWords), stopwords("SMART"))

# you could also do stemming; I don't bother here.
questions <- tm_map(questions, content_transformer(stripWhitespace)) ## remove excess white-space

qtm <- DocumentTermMatrix(questions)
class(qtm)
## You can inspect them:
inspect(qtm[1:5,1:8])
## find words with greater than a min count
findFreqTerms(qtm,100)
findFreqTerms(qtm,10)
## or grab words whose count correlates with given words
findAssocs(qtm, "rock", .2) 

## Finally, drop those terms that only occur in one or two questions
#qtm <- removeSparseTerms(qtm, 0.995)

qtm_mat = as(qtm, "Matrix")
summary(head(qtm_mat))
qpc <- qtm_mat %*% irlba(qtm_mat, nv=10, nu=0)$v 
qpc[1,]

## look at the big rotations... 
classpca$rotation[order(abs(classpca$rotation[,1]),decreasing=TRUE),1][1:10]
classpca$rotation[order(abs(classpca$rotation[,2]),decreasing=TRUE),2][1:10]

## Plot the first two PCs..
plot(classpca$x[,1:2], col=0, xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n")
text(x=classpca$x[,1], y=classpca$x[,2], labels=rownames(dtm))


#we've now categorized questions into topics.  Let's then take our answers, categorize
#them into the same topics, and run a lasso on the answer topics interacted with the
#questions topics and "negation" to logistically predict "correct"

src = VectorSource(df$answer)
answers = Corpus(src)
## tm_map just maps some function to every document in the corpus
answers <- tm_map(answers, content_transformer(tolower)) ## make everything lowercase
#answers <- tm_map(answers, content_transformer(removeNumbers)) ## remove numbers
answers <- tm_map(answers, content_transformer(removePunctuation)) ## remove punctuation
## remove stopword.  be careful with this: one's stopwords are anothers keywords.
answers <- tm_map(answers, content_transformer(removeWords), stopwords("SMART"))

# you could also do stemming; I don't bother here.
answers <- tm_map(answers, content_transformer(stripWhitespace)) ## remove excess white-space

atm <- DocumentTermMatrix(answers)

correct_model <- glm(df$correct ~  df$negation * qpc * apc, family="binomial")


tpc <- topics(qtm, K=2:10) # it chooses 2 topics only!  this is simple class ;-)
summary(tpc)
# If you follow through with the 2 topic model below, it'll tell you that 
# class one was a single topic and every other class was another.
# I think a 3 topic model is more interesting here.
tpc <- topics(qtm,K=7)
summary(tpc, 10) #10 is number of top terms to print

## the topic-term probabilities ('theta'); each column is a topic
## we can use these to rank terms by probability within topics
rownames(tpc$theta)[order(tpc$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpc$theta)[order(tpc$theta[,2], decreasing=TRUE)[1:10]]
## topic 1 looks like regression modelling, topic 2 is all else on data.

## plot the lectures another way (do them in order)
whichtopic <- 3 # change this to see which classes are in each
par(srt=-30, xpd=NA) ## rotate stings, and allow words outside plot
plot(tpc$omega[,whichtopic], type="l", col=8, xlab="", xlim=c(0.5,12),
     xaxt="n", ylab=sprintf("topic %d weight",whichtopic), bty="n")


## cl=NULL instead implies a serial run. 
cl <- makeCluster(detectCores())
## small nlambda for a fast example
fits <- mnlm(cl, we8thereRatings, 
             we8thereCounts, bins=5,nlambda=10)
stopCluster(cl) # usually a good idea

## plot fits for a few individual terms
terms <- c("first date","chicken wing",
           "ate here", "good food",
           "food fabul","terribl servic")
par(mfrow=c(3,2))
for(j in terms)
{ 	plot(fits[[j]]); mtext(j,font=2,line=2) }

## extract coefficients
B <- coef(fits)
mean(B[-1,]==0) # sparsity in loadings
## some big loadings on `overall'
B[2,order(B[2,])[1:10]]
B[2,order(-B[2,])[1:10]]

## do MNIR projection onto factors
z <- srproj(B,we8thereCounts) 

## fit a fwd model to the factors
summary(fwd <- lm(we8thereRatings$Overall ~ z)) 

## truncate the fwd predictions to our known range
fwd$fitted[fwd$fitted<1] <- 1
fwd$fitted[fwd$fitted>5] <- 5
## plot the fitted rating by true rating
par(mfrow=c(1,1))
plot(fwd$fitted ~ factor(we8thereRatings$Overall), 
     varwidth=TRUE, col="lightslategrey")



#TODO: Pull in topic information from Wikipedia to train on
#TODO: google question, pick the answer that best matches the top result

