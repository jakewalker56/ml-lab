kurt = function(x) {
  x = na.omit(x)
  x.bar = mean(x)
  N = length(x)
  sum((x - x.bar)^4)/((N - 1) * sd(x)^4)
}