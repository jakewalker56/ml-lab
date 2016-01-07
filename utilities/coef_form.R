coef_formula <- function(coefs) {
  myFormula = ""
  for(i in 1:length(coefs)) {
    if(names(coefs)[i] != "(Intercept)") {
      myFormula = paste(myFormula, names(coefs)[i], sep="")
      myFormula = paste(myFormula, "*", sep="")
    }
    myFormula = paste(myFormula, coefs[i], sep="")
    if(i != length(coefs)) {
      myFormula = paste(myFormula, " + ", sep="")
    }
  }
  return(myFormula)
}