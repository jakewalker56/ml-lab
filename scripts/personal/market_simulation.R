terminal_value <- function (up_count, down_count){
  result = 1
  if(up_count == 0){
    result = 0.3
  }
  return(result)
}

get_price <- function(agents, up_count, down_count, lookahead, allow_leverage, allow_short){
  if(lookahead == 0)
  {
    return(terminal_value(up_count, down_count))
  }
  
  #recursively figure out up state value, down state value
  price_up = get_price(agents, up_count + 1, down_count, lookahead-1, allow_leverage, allow_short)
  price_down = get_price(agents, up_count, down_count + 1, lookahead-1, allow_leverage, allow_short)
  
 
  print(paste("price up:", price_up))
  print(paste("price down:", price_down))
  if(abs(price_up / price_down - 1.0) < .001) {
    #prices are too close together; everyone is going to want the same thing,
    #and our optimization process is going to fail.  Just return the mean of the two.
    
    #this often happens if price_up == price_down
    return((price_up + price_down)/2.0)
  } 
  #each person has own willingness to pay
  wtp <- price_up * agents$probability + price_down *(1 - agents$probability)  
  
  #make sure agents are sorted
  agents = agents[order(agents$probability),]
  
  #initial price seed
  p = mean(wtp)
  MAX = 100
  for(j in 1:MAX) {
    #supply is total numer of underlyings in the market
    supply = sum(agents$underlying)
    
    #demand is total amount owned by people who want it
    demand = 0
    
    #price must be set so supply == demand
    #if we iterate without finding an answer for 100 times, just bail.
    for(i in nrow(agents):1) {
      #start at the most optimistic, assign them to demand
      
      #if this agent's willingess to pay is lower than the price
      #he is going to sell everything, so he does not contribute to demand
      if(wtp[i] < p){
        break
      }
      
      #now this unit will buy as much as possible, so demand is current amount
      #plus amount purchased
      if(allow_leverage){
        demand = demand + agents[i,]$underlying + (agents[i,]$capital + agents[i,]$underlying * price_down)/p
      } else {
        demand = demand + agents[i,]$underlying + agents[i,]$capital / p
      }
    }
    print(paste("demand:", demand, "supply:", supply, "price:", p))
    #check if demand ~= supply
    if(abs(demand/supply - 1.0) < .001) {
      #good enough
      break
    } else {
      #adjust p depending on how badly we missed
      print("adjusting price...")
      p = p * ((i + nrow(agents))/nrow(agents) + demand/supply) / ((i + nrow(agents))/nrow(agents) + 1)
    }
  }
  print("willingness to pay:")
  print(summary(wtp))
  return(p)
}

invest <- function(agents, lookahead, allow_leverage, allow_short){
  #we know supply == demand, so change the capital/underlying allocation
  #of the agents according to price
  
  price_up = get_price(agents, 1, 0, lookahead-1, allow_leverage, allow_short)
  price_down = get_price(agents, 0, 1, lookahead-1, allow_leverage, allow_short)
  
  current_price = get_price(agents, 0, 0, lookahead, allow_leverage, allow_short)
  
  #each person has own willingness to pay
  wtp <- price_up * agents$probability + price_down *(1 - agents$probability)  
  print(summary(wtp))
  remaining_supply = sum(agents$underlying)
  
  for(i in nrow(agents):1) {
    #start at the most optimistic, assign them to demand
    #if this agent's willingess to pay is lower than the price, bail
    if(wtp[i] < current_price){
      if(remaining_supply != 0) {
        #this is the unlucky person on the margin.  He has to keep some underlying
        agents[i,]$capital = agents[i,]$capital - current_price * (remaining_supply - agents[i,]$underlying)
        agents[i,]$underlying = remaining_supply
        remaining_supply = 0
      } else {
        #these people sold their underlying this period
        agents[i,]$capital = agents[i,]$capital + agents[i,]$underlying * current_price
        agents[i,]$underlying = 0
      }
    }
    else {
      if(allow_leverage){
        #buy as much as you can
        agents[i,]$underlying = agents[i,]$underlying + (agents[i,]$capital + agents[i,]$underlying * price_down)/current_price
        agents[i,]$capital = -agents[i,]$underlying * price_down
        remaining_supply = remaining_supply - agents[i,]$underlying
      } else {
        agents[i,]$underlying = agents[i,]$underlying + agents[i,]$capital/current_price
        agents[i,]$capital = 0
        remaining_supply = remaining_supply - agents[i,]$underlying
      }
    }
  }
  return(agents)
}

count = 100
agents = data.frame(
  probability=seq(from=0 + 1/count,to=1-1/count,length.out=count-1),
  capital=1,
  underlying=1
)

summary(agents)

iterations = 1
price_lookahead = 2
allow_leverage = FALSE
allow_short = FALSE
p = get_price(agents, 0, 0, price_lookahead, allow_leverage, allow_short)
print(p)
a = invest(agents, price_lookahead, FALSE, FALSE)
summary(a)
a
mean(a$underlying)
mean(a$capital)

# for(i in 1:iterations){
#   
#   #price is set by supply and demand
#   p = get_price(agents, 0, 0, price_lookahead, allow_leverage, allow_short)
#   print(p)
#   
#   #bankrupt agents leave the market
#   agents = agents[(agents$capital + p * agents$underlying) > 0,]
#   #note that if an agent goes bankrupt, they can take losses with them,
#   #and capital goes up in the market
#   
#   #agents choose investment based on their own beliefs
#   agents = invest(agents, price, allow_leverage, allow_short)
# }