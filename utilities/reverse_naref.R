## reverse_naref: replace (NA) with "NA"

reverse_xnaref <- function(x){
  if(is.factor(x))
    if(is.na(levels(x)[1]))
      levels(x)[is.na(levels(x))] <- "NA"
  return(x) }

reverse_naref <- function(DF){
  if(is.null(dim(DF))) return(reverse_xnaref(DF))
  if(!is.data.frame(DF)) 
    stop("You need to give me a data.frame or a factor")
  DF <- lapply(DF, reverse_xnaref)
  return(as.data.frame(DF))
}