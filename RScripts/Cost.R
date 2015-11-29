# c is a cost matrix of the form:
#                            actual
#                         a              b        c
#               a        100            100       ...
#   prediction  !a       200             0        ...
# 

# p is a probability matrix of the form:
#                            Probability
#                         a              b        c
#               a        0.2            0.8       ...
# ...which will be normalized to a total of 1.0


predict_a <- function(c, p) {
  #don't pass in negative values, n00b
  if(min(p) < 0)
    p <- p + abs(min(p))
  if(min(c) < 0)
    return(FALSE)
  
  #normalize p
  if(sum(p) == 0)
    return(FALSE)
  p <- p/sum(p)
  
  #cost of predicting a:
  #cost_p_a = p(a) * cost(a | p_a) + p(b) * cost(b | p_a) ...
  cost_p_a = 0
  for (i in 1:length(c[1,])) {
    cost_p_a = cost_p_a + p[i] * c[1,i]
  }
  #cost of predicting !a:
  #cost_p_!a = p(a) * cost(a | p_!a) + p(b) * cost(b | p_!a) ...
  cost_p_not_a = 0
  for (i in 1:length(c[1,])) {
    cost_p_not_a = cost_p_not_a + p[i] * c[2,i]
  }
  #if cost_p_a > cost_p_!a, we should predict a
  return(cost_p_a < cost_p_not_a)
}