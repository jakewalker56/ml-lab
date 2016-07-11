#decision rule
generate_number <- function(chances = 2, decision_rule = 0.5) {
  chance = 1
  val = runif(1)
  while(chance < chances){
    if(val < decision_rule){
      val = runif(1)
    }
    chance = chance + 1
  } 
  return(val)
}

calc_win_rates <- function(his_rules, my_rules, iterations, his_chances = 2, my_chances = 3){
  win_rates <- matrix(vector(), nrow=length(his_rules), ncol=length(my_rules))
  his_iter = 0
  for(his_rule in his_rules){
    his_iter = his_iter + 1
    my_iter = 1
    for(my_rule in my_rules){
      winner = vector()
      for(i in 1:iterations) {    
        #simulate his number
        his_number = generate_number(chances = his_chances, decision_rule = his_rule)
        #simulate my number
        my_number = generate_number(chances = my_chances, decision_rule = my_rule)
        winner = c(winner, his_number < my_number)
      }
      win_percent = sum(winner)/length(winner)
      win_rates[his_iter, my_iter] = win_percent
      my_iter = my_iter + 1
      
      if((my_iter - 1 + (his_iter - 1) * length(my_rules)) %% round(length(my_rules)*length(his_rules)/10) == 0 | length(my_rules)*length(his_rules) < 10) {
        print(paste(round(100*(my_iter - 1 + (his_iter - 1) * length(my_rules)) / (length(my_rules)*length(his_rules))),"% complete", sep=""))
      }
    }
  }
  return(win_rates)
}
win_rates = calc_win_rates(
  his_rules = seq(from=0, to=1, length.out=101),
  my_rules = seq(from=0, to=1, length.out=101),
  iterations = 1000,
  his_chances = 2,
  my_chances = 2
  )

plot(his_rules, apply(win_rates, 1, max), ylab="best I can do")
plot(his_rules, apply(win_rates, 1, which.max)/100, ylab="my optimal decision rule")
plot(his_rules, apply(win_rates, 1, mean), ylab="Average win rate with random rule")

plot(win_rates[66,])

his_rules = c(0.66)
my_rules = c(0.6)
win_rates_2 = calc_win_rates(
  his_rules = his_rules,
  my_rules = my_rules,
  iterations = 50000,
  his_chances = 2,
  my_chances = 2
)
plot(seq(from=0.55, to=0.7, length.out=10), apply(win_rates_2, 1, max), ylab="best I can do")
plot(seq(from=0.55, to=0.7, length.out=10), apply(win_rates_2, 1, which.max), ylab="my optimal decision rule")
plot(seq(from=0.55, to=0.7, length.out=10), apply(win_rates_2, 1, mean), ylab="Average win rate with random rule")
plot(my_rules, win_rates_2[1,])
win_rates_2
expected value = (1 - rule) * (max + rule) / 2 + max/2

pdf(x)
if x < rule
pdf(x) =
  x*rule
if x > rule
pdf(x) 
  x * rule + (x - rule) * (1 - rule)

cdf = integral under curve

Now imagine we want to find ruleprime, with the same pdf/cdf functions
but a different rule. To find the odds of winning, we would want 
to find:

integral from 0 to 1 of pdfprime(x)*cdf(x)

now to maximize this, we would take the derivative and set it equal to zero

assume rule < ruleprime
3 regions
rule > x
  x*rule * x * ruleprime
rule < x < ruleprime
  (x * rule + (x - rule) * (1 - rule)) * x * ruleprime
rule < ruleprime < x
  (x * rule + (x - rule) * (1 - rule)) * (x * ruleprime + (x - ruleprime) * (1 - ruleprime))

sum these and it has to = 0 to find the ruleprime that maximizes

x*rule*x*ruleprime + (x*rule*x*ruleprime) +