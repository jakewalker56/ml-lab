convert_to_numeric <- function (d){
  for(di in colnames(d)) {
    if(!is.numeric(d[,di])){
      #d[,di] <- as(levels(d[,di]),"numeric")[d[,di]]
      d[,di] <- as(d[,di],"numeric")
    }
  }
  return(d)
}