library(nnet)
NUM_SOLDIERS = 100
CASTLE_VALUES = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
DEBUG = FALSE


Dropbox = "~/github"
path= "/ml-lab/data/"
setwd(paste(Dropbox,path, sep=""))
strategies = read.csv(paste(Dropbox,path,"castle-solutions.csv",sep=""))
strategies <- strategies[,-11]
#grep("jakewalker56", strategies[,11])
strategies[735,]

fast_evaluate <- function(s){
  result <- rep(0,nrow(s))
  #this turns out to be ~ the same speed as using for loops
  sapply(seq(from=1, to=nrow(s)-1, by=1), function(i) {
    sapply(seq(from=i+1, to=nrow(s), by=1), function(j) {
      winner = (s[i,] > s[j,]) - (s[i,] < s[j,])
      tally = sum(winner * CASTLE_VALUES)
      if(tally > 0) {
        result[i] <<- result[i] + 1
      } else if(tally < 0) {
        result[j] <<- result[j] + 1
      } else {
        result[j] <<- result[j] + 0.5
        result[i] <<- result[i] + 0.5
      }
    })
  })
  return(result)
}

evaluate <- function(x1, x2) {
  winners = sapply(seq(from=1,to=10,by=1), function(i){
    if(x1[i] > x2[i]){
      return(1)
    } else if (x1[i] < x2[i]) {
      return(0)
    } else {
      return(0.5)
    }
  })
  result = winners * CASTLE_VALUES
  if(sum(result)> sum(CASTLE_VALUES) / 2) {
    return(1)
  } else if(sum(result) < sum(CASTLE_VALUES) / 2) {
    return(0)
  } else {
    return(0.5)
  }
}

s = strategies

#discard invalid strategies
discard <- c(NULL)
for(i in 1:nrow(s)) {
  if(sum(s[i,]) != NUM_SOLDIERS){
    print(paste("discarding strategy ", i, " (", sum(strategies[i,]), " soldiers)", sep=""))
    discard <- c(discard, i)
  }
}
s <- s[-discard,]
rownames(s) <- seq(from=1, to=nrow(s), 1)

#Score everyone against everyone else, just for fun
#578.567 seconds = ~10 min
system.time(results <- fast_evaluate(s))
sapply(seq(from=1, to=nrow(s), by=1), function(i){
  return(paste(list(s[i,]), ": ", results[i]))
})
print(paste("winning strategy: ", list(s[which.is.max(results),])))

#look at the top 100 winning strategies
#winningest strategy is a good baseline for everything else (1137 wins)
n=100
p = n
print(paste("top ", n, " strategies:", sep=""))
for(i in tail(order(results), n)) {
  print(paste("strategy ", i, " (#", p, "): ", list(s[i,]), "; score = ", results[i], sep=""))
  p <- p - 1
}

#utility function for genetic algorithm
colMaxZero <- function(data) {
  return(sapply(c(data), function(a){
    return(max(a, 0))
  }))
}

#utility function for genetic algorithm
colMinSoldiers <- function(data) {
  return(sapply(c(data), function(a){
    return(min(a, NUM_SOLDIERS))
  }))
}

#genetic algorithm to explore the possibility space and try to find local optimas
genetic_find <- function(seed, iterations = 100, decay = 0.95, explore=.8, dropout=0.4){
  current_best = seed
  current_best_result <- sum(sapply(seq(1, nrow(s), 1), function(i){
      return(evaluate(current_best, s[i,]))
    }
  ))
  current_explore = explore
  for(i in seq(from=1, to=iterations)){
    print(paste("Current best (", current_best_result,", iteration ", i,"): ", list(current_best), sep=""))
    perturbed <- colMinSoldiers(colMaxZero(current_best + (runif(length(CASTLE_VALUES), 0, 1) > dropout) * rnorm(n=length(CASTLE_VALUES), mean = 0, sd = current_explore) * (10 + current_best)))
    
    #normalize
    perturbed <- round(NUM_SOLDIERS * perturbed / (sum(perturbed)), 0)
    while(sum(perturbed) != NUM_SOLDIERS) {
      r = sample(1:10, 1)
      if(sum(perturbed) < NUM_SOLDIERS) {
        perturbed[r] <- min(perturbed[r] + 1, NUM_SOLDIERS)
      } else {
        perturbed[r] <- max(perturbed[r] - 1, 0)
      }
    }
    print(paste("Evaluating perturbation ", list(perturbed), sep=""))
    perturbed_result <- sum(sapply(seq(1, nrow(s), 1), function(i){
        return(evaluate(perturbed, s[i,]))
      }
    ))
    print(paste("Perturbed Result: ", perturbed_result))
    if(perturbed_result > current_best_result) {
      current_best <- perturbed
      current_best_result <- perturbed_result
    }
    current_explore <- current_explore * decay
  }
  return(current_best)
}


#using various seed values, the resulting strategy is what the genetic algorithm ended up spitting out
seed = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
mystrategy = c(6, 2, 2, 12, 16, 3, 2, 31, 4, 22)
#4, 5, 8, 10
sum(sapply(seq(1, nrow(s), 1), function(i){ return(evaluate(mystrategy, s[i,]))}))

seed = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
mystrategy = c(0, 3, 2, 12, 3, 22, 2, 31, 3, 22)
#4, 6, 8, 10
sum(sapply(seq(1, nrow(s), 1), function(i){ return(evaluate(mystrategy, s[i,]))}))

seed = c(1, 1, 11, 12, 1, 19, 24, 29, 1, 1)
mystrategy = c(0, 1, 7, 8, 2, 22, 22, 31, 4, 3)
#3, 4, 6, 7, 8
sum(sapply(seq(1, nrow(s), 1), function(i){ return(evaluate(mystrategy, s[i,]))}))

seed = c(3, 5, 8, 10, 13, 1, 26, 30, 2, 2)
mystrategy = c(0, 6, 8, 9, 12, 1, 27, 31, 2, 4)
#2, 3, 4, 5, 7, 8
sum(sapply(seq(1, nrow(s), 1), function(i){ return(evaluate(mystrategy, s[i,]))}))

seed = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
mystrategy = c(0, 5, 7, 9, 16, 22, 2, 2, 33, 4)
#2, 3, 4, 5, 6, 9
sum(sapply(seq(1, nrow(s), 1), function(i){ return(evaluate(mystrategy, s[i,]))}))


system.time(best <- genetic_find(seed, decay=0.995, explore=0.9, dropout=0.6, iterations=1000))

