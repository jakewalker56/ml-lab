setdebug <- function(x) {
  if(x == TRUE)
  {
    globaldebug <<- TRUE
  }
  else
  {
    globaldebug <<- FALSE
  }
}
setfile <- function(file) {
  print(paste("saving log to", file, sep=" "))
  logfile <<- file
}
clearfile <- function() {
  print("clearing log file")
  write("", file = logfile, append = FALSE, sep = "")
}
printclassification <- function(content, ...){
  printf("<table>", ...)
  printf("<tr>", ...)
  for (i in 1:length(colnames(content$centers))){
    printf(paste("<th>", colnames(content$centers)[i], "</th>"), ...)
  }
  printf("</tr>", ...)
  for (i in 1:length(content$centers[,1])){
    printf("<tr>", ...)
    for (j in 1:length(content$centers[1,])){
      printf(paste("<td>", content$centers[i,j], "</td>"), ...)
    }
    printf("</tr>", ...)
  }
  printf("</table>", ...)
}
printf <- function(content, file=TRUE, debug=FALSE, console=TRUE, ...) {
  #if debug is false OR we want to print debug strings
  if(!debug || (debug && globaldebug) ) {
    if(file && !debug){
      if(is.list(content)){
        #we can't print lists right now, so we redirect output
        sink(file=logfile, append=TRUE)
          print(content, ...)
        sink()
      }
      else {
        write(content, file = logfile, append=TRUE, sep = " ", ...)
      }
    }
    if(console || debug) {  
      print(content)
    }
  }
}
setuplog <- function(filename, debug) {
  setdebug(debug)
  setfile(filename)
  clearfile()
}