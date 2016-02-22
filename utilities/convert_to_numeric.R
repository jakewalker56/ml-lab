atomic_convert_to_numeric <- function(d){
  if(!is.numeric(d)){
    if(is.factor(d)) {
      #if the value is NA, set it to 0
      d[is.na(d)] <- "0"
      #if factor, convert it to its char representation before converting to numeric
      d <- as(d, "character")
    }
    d <- as(d,"numeric")
  } else {
    #this is numeric, but it could still have NA's
    d[is.na(d)] <- 0
  }
  return(d)
}
convert_to_numeric <- function (d){
  if(is.atomic(d)) {
    return(atomic_convert_to_numeric(d))
  } else {
    for(di in colnames(d)) {
      #not sure if you can have a frame with a frame inside it, but I guess we'll support it with a recursive call?
      #note that this will break if convert_to_numeric doesn't simplify to an atomic eventually
      d[,di] <- convert_to_numeric(d[,di])
    }
 }
  return(d)
}