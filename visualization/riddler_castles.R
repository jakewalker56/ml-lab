library(nnet)
NUM_SOLDIERS = 100
CASTLE_VALUES = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
DEBUG = FALSE

evaluate <- function(x1, x2) {
  if(length(x1) != length(CASTLE_VALUES)){
    stop("x1 is not the same length as castles")
  }
  if(length(x2) != length(CASTLE_VALUES)){
    stop("x2 is not the same length as castles")
  }
  if(sum(x1) != NUM_SOLDIERS){
    stop("x1 does not have the correct number of soldiers allocated")
  }
  if(sum(x2) != NUM_SOLDIERS){
    stop("x2 does not have the correct number of soldiers allocated")
  }
  ?lapply
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
    if(DEBUG){
      print(paste("Strategy 1 wins: ", sum(result), "-", sum(CASTLE_VALUES) - sum(result)))
    }
    return(1)
  } else if(sum(result) < sum(CASTLE_VALUES) / 2) {
    if(DEBUG){
      print(paste("Strategy 2 wins: ", sum(result), "-", sum(CASTLE_VALUES) - sum(result)))
    }
    return(0)
  } else {
    if(DEBUG){
      print(paste("Tie Game: ", sum(result), "-", sum(CASTLE_VALUES) - sum(result)))
    }
    return(0.5)
  }
}
strategies = list(
  c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
  c(25, 25, 25, 25, 0, 0, 0, 0, 0, 0),
  c(24, 24, 23, 23, 1, 1, 1, 1, 1, 1),
  c(0, 11, 11, 12, 11, 11, 11, 11, 11, 11)
)

results = sapply(strategies, function(strategy_1){
  return(sum(sapply(strategies, function(strategy_2){
    return(evaluate(strategy_1, strategy_2))
  }
  #ignore the tie with yourself
  )) - 0.5)
})

sapply(seq(from=1, to=length(strategies), by=1), function(i){
  return(paste(strategies[i], ": ", results[i]))
})
print(paste("winning strategy: ", strategies[which.is.max(results)]))
