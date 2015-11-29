source("~/github/projects/RScripts/logging.R")
source("~/github/projects/RScripts/naref.R")

trimnumerics<- function(d){
  for(di in colnames(d)) {
    if(!is.numeric(d[,di])){
      #drop the column    
      d[,di] <- NULL
    }
  }
  return(d)
}

getpngfile <- function(i) {
  #returns a name for the ith png file of the doc
  filearray = strsplit(logfile, "\\.")
  return(path.expand(paste(paste(filearray[[1]][-length(filearray[[1]])], collapse="."), "_", i, ".png", sep="")))
}

sudoanalyze = function(y, x, debug=FALSE, ...) {
  setuplog("~/github/projects/data/sudoanalyze.html", debug)
  #sudoanalyze is my own personal auto-analysis tool
 
  i = 1
  headsize = 20
  n = 30
  
  printf("<div>")
  printf("<h1>##RESPONSE SUMMARY##</h1>")
  yn <- naref(y)
  for(yi in colnames(yn)) {
    printf("<div>", ...)
    printf("<table>", ...)
    printf(paste("<tr><th colspan=", length(head(names(summary(sort(yn[,yi]))))),">",yi,"</th></tr>", sep=""), ...)
    printf("<tr><td>", ...)
    printf(paste(strsplit(head(names(summary(sort(yn[,yi])))),"\t"), collapse="</td><td>"), ncolumns=600, ...)
    printf("</td></tr>", ...)
    printf("<tr><td>", ...)
    printf(paste(head(summary(sort(yn[,yi]))), collapse="</td><td>"), ncolumns=600, ...)
    printf("</td></tr>", ...)
    printf("</table>", ...)
    
    #draw plots
    img = getpngfile(i)
    i = i+1
    png(filename=img, width = 960, height = 480, units = "px")
    if(is.factor(yn[,yi])) {
        img=""
        #TODO: what to plot here if y is a factor?
    } else if (is.numeric(yn[,yi])) {
      #set any NAs to 0's
      yn[,yi][is.na(yn[,yi])] <- 0
      diff = max(yn[,yi]) - min(yn[,yi])
      if (diff == 0) {
        diff = .01
      }
      sequence = seq(min(yn[,yi]) - (1/n + 0.01)*diff, max(yn[,yi]) + (1/n + 0.01)*diff, l=n)
      printf(sequence, debug=TRUE, ...)
      hist(yn[,yi], col="grey", xlab=yi, main=paste(yi,"histogram", sep=" "), ylab="Frequency", 
           breaks=sequence)
    }
    dev.off()
    printf(paste("<img src='", img,"'>", sep=""), ...)
    printf("</div>", ...)
  
    #newline
    printf("<br>", ...)
    
  }    
  printf("<h1>##END RESPONSE SUMMARY##</h1>", ...)
  printf("</div>", ...)
  printf("<div>", ...)
  printf("<h1>##CONTROL SUMMARY##</h1>", ...)
  xn <- naref(x)
  
  for(xi in colnames(xn)) {
    #print summary
    printf("<div>", ...)
    printf("<table>", ...)
    printf(paste("<tr><th colspan=", length(head(names(summary(sort(xn[,xi]))), n=headsize)),">",xi,"</th></tr>", sep=""), ...)
    printf("<tr><td>", ...)
    printf(paste(strsplit(head(names(summary(sort(xn[,xi]))),n=headsize),"\t"), collapse="</td><td>"), ncolumns=600, ...)
    printf("</td></tr>", ...)
    printf("<tr><td>", ...)
    printf(paste(head(summary(sort(xn[,xi])), n=headsize), collapse="</td><td>"), ncolumns=600, ...)
    printf("</td></tr>", ...)
    printf("</table>", ...)
    
    #draw plots
    img = getpngfile(i)
    i = i+1
    png(filename=img, width = 960, height = 480, units = "px")
    if(is.factor(xn[,xi])) {
      if (length(unique(factor(xn[,xi]))) < 100 && length(unique(factor(xn[,xi]))) > 1 ) {
        boxplot(y[!is.na(xn[,xi]),] ~ xn[,xi][!is.na(xn[,xi])], xlab=xi, ylab=colnames(y)[1], main=paste(xi,"factor boxplot", sep=" "))
      } else {
        img=""
        #TODO: what to plot here if # factors is huge?
      }
    } else if (is.numeric(xn[,xi])) {
      #set any NAs to 0's
      xn[,xi][is.na(xn[,xi])] <- 0
      printf(typeof((xn[,xi])), debug=TRUE, ...)
      printf(min(xn[,xi]), debug=TRUE, ...)
      printf(max(xn[,xi]), debug=TRUE, ...)
      diff = max(xn[,xi]) - min(xn[,xi])
      if (diff == 0) {
        diff = .01
      }
      sequence = seq(min(xn[,xi]) - (1/n + 0.01)*diff, max(xn[,xi]) + (1/n + 0.01)*diff, l=n)
      printf(sequence, debug=TRUE, ...)
      hist(xn[,xi], col="grey", xlab=xi, main=paste(xi,"histogram", sep=" "), ylab="Frequency", 
         breaks=sequence)
    }
    dev.off()
    printf(paste("<img src='", img,"'>", sep=""), ...)
    printf("</div>", ...)
    #newline
    printf("<br>", ...)
  }   
  printf("<h1>##END CONTROL SUMMARY##</h1>")
  printf("</div>")
  
  #now lets classify some things!
  printf("<div>", ...)
  printf("<h1>##CLASSIFICATION##</h1>", ...)
  
  grp = kmeans(x=trimnumerics(xn), centers=5, nstart=5)
  printclassification(grp)
  printf("<h1>##END CLASSIFICATION##</h1>", ...)
  printf("</div>", ...)
  
  #just print to console
  printf("done!", console=TRUE, file=FALSE)
}