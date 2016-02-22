atomic_convert_to_string <- function(d){
  if(!is.character(d)){
      d <- as(d, "character")
  }
  return(d)
}
convert_to_string <- function (d){
  if(is.atomic(d)) {
    return(atomic_convert_to_string(d))
  } else {
    for(di in colnames(d)) {
      #not sure if you can have a frame with a frame inside it, but I guess we'll support it with a recursive call?
      #note that this will break if convert_to_numeric doesn't simplify to an atomic eventually
      d[,di] <- convert_to_string(d[,di])
    }
  }
  return(d)
}