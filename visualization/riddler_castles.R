NUM_SOLDIERS = 100
CASTLE_VALUES = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

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
    print(paste("Strategy 1 wins: ", sum(result), "-", sum(CASTLE_VALUES) - sum(result)))
  } else if(sum(result) < sum(CASTLE_VALUES) / 2) {
    print(paste("Strategy 2 wins: ", sum(result), "-", sum(CASTLE_VALUES) - sum(result)))
  } else {
    print(paste("Tie Game: ", sum(result), "-", sum(CASTLE_VALUES) - sum(result)))
  }
}

strategy_1 = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
strategy_2 = c(25, 20, 15, 9, 6, 5, 5, 5, 5, 5)

evaluate(strategy_1, strategy_2)
