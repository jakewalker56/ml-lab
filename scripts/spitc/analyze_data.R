#Windows
Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/SPITC/PE Activity - Financial Info/ceadata"
setwd(paste(Dropbox, path, sep=""))

bid = read.csv("bidsummary_extended_population.csv")

block = "E"
#try a simple forward stepwise including all blocks
#Note: this is going to be very innacurate, because errors in C
#(auctioned as REA) will dominate all other errors.  Better to do
#a per-block regression, explored in detail below
full = lm(pwb_amount ~ pop * Block * income + gulf, data=rerun)
null <- lm(pwb_amount ~ 1, data=bid)
fwd <- step(null, scope=formula(full), direction="forward", k=log(length(bid$pop)))
summary(fwd)



################################
#log transforms

#note: don't run this on C.  Not enough datapoints, the reverse
#stepwise function fails
block_ar = vector()
KCSM_pop_ar = vector()
KCSM_income_ar = vector()
lwr_ar = vector()
upr_ar = vector()
val_ar = vector()
channelcost_ar = vector()
lm_ar = vector()
for(block in c("A", "B", "E"))
{
  #log transforms
  block_data = bid[bid[,"Block"] == block,]
  reg_data = data.frame(pwb_amount = log(block_data$pwb_amount))
  reg_data$pop = block_data$pop
  reg_data$income = block_data$income
  reg_data$logpop = log(block_data$pop + 1)
  reg_data$logincome = log(block_data$income + 1)
  
  full <- lm((pwb_amount) ~ .^4 + 1, data=reg_data, y=TRUE)
  rev <- step(full, scope=formula(null), direction="backward", k=log(length(reg_data$pop)))
  summary(rev)
  
  #cald r-squared in non-log space
  ssr = sum((exp(predict(rev)) - mean(block_data$pwb_amount))^2)
  sse = sum((exp(predict(rev)) - block_data$pwb_amount)^2)
  r2 = ssr/(ssr + sse)
  r2
  
  #visualize the data
  # hist(block_data$pwb_amount)
  # hist(reg_data$pwb_amount)
  # plot(resid(rev))
  # hist(resid(rev))
  
  #test the KCSM example
  #License BRED-20130726AEV
  # 
  # It appears tv stations aren't given broadcasting rights over geographic
  # regions- rather, they are given broadcast rights to antanae locations,
  # directions, and and average power usage.  How do we convert this into
  # population + income?  We can get income from CMA, but pop....?
  
  
  
  #Equivalent CMAs ~ 7, 27, 175, 123, 111 - http://wireless.fcc.gov/auctions/data/maps/CMA.pdf
  #- service contour map from http://transition.fcc.gov/fcc-bin/tvq?call=KCSM
  KCSM_pop = 0
  
  if(block == "E" || block == "A")
  {
    #block E, A
    
    # KCSM TV broadcasts 24 hours a day, and our 500 kilowatt broadcast 
    # signal has a coverage area that includes San Mateo, San Francisco, 
    # Santa Clara, Santa Cruz, Alameda, Contra Costa, Marin, Solano, 
    # Sonoma and Napa counties
    path= "/SPITC/PE Activity - Financial Info/Stations by State"
    setwd(paste(Dropbox, path, sep=""))
    
    county =  read.csv("Zipcode_database.csv")
    tempcounty = county[county["state"] == "CA",]
    KCSM_pop = 0
    
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "San Mateo County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "San Francisco County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Santa Cruz County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Alameda County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Contra Costa County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Marin County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Solano County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Sanonoma County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Napa County",]$estimated_population)
    
    #KCSM website claims ~6M reachable auidience- comment this depending on
    #which estimate you want to use (~6M seems more reasonable based)
    #on CMA info for block B
    KCSM_pop = 6000000
    
    KCSM_income = block_data[block_data["EA"] == 163,]$income
  
    #what 6MHz of block A/E actually went for 
    #(EA is too large of an area, but still useful)
    channelcost = block_data[block_data["EA"] == 163,]$pwb_amount * KCSM_pop/block_data[block_data["EA"] == 163,]$pop
    if(block=="A")
    {
      #block a was auctioned as 12 MHz
      channelcost = channelcost / 2
    }
  }
  if(block =="B")
  {
    #block B
    KCSM_pop = 0
    
    KCSM_pop = block_data[block_data["CMA"] == 7,]$pop + sum(block_data[block_data["CMA"] == 27 | block_data["CMA"] == 175 | block_data["CMA"] == 126,]$pop / 2)
    KCSM_income = sum(block_data[block_data["CMA"] == 7 | block_data["CMA"] == 27 | block_data["CMA"] == 175 | block_data["CMA"] == 126,]$income) / 4
  
    #what 6MHz of block B actually went for
    channelcost = (block_data[block_data["CMA"] == 7,]$pwb_amount  + sum(block_data[block_data["CMA"] == 27 | block_data["CMA"] == 175 | block_data["CMA"] == 126,]$pwb_amount / 2)) / 2 
  }
  
  KCSM = data.frame(pop = KCSM_pop)
  KCSM$income = KCSM_income
  KCSM$logpop = log(KCSM$pop + 1)
  KCSM$logincome = log(KCSM$income + 1)
  
  #predict the value based on this regression
  if(block == "E")
  {
    #confidence interval
    prediction = predict(rev, newdata=KCSM, interval="prediction", se.fit=TRUE)
    lwr = exp(matrix(prediction$fit[,2]))
    upr = exp(prediction$fit[,3])
    val =exp(matrix(prediction$fit[,1]))
  }
  if(block == "B" || block == "A")
  {
    prediction = predict(rev, newdata=KCSM, interval="prediction", se.fit=TRUE)
    lwr = exp(matrix(prediction$fit[,2]))/2
    upr = exp(prediction$fit[,3])/2
    val =exp(matrix(prediction$fit[,1]))/2
  }
  block_ar = c(block_ar, block)
  channelcost_ar = c(channelcost_ar, channelcost)
  KCSM_pop_ar = c(KCSM_pop_ar, KCSM_pop)
  KCSM_income_ar = c(KCSM_income_ar, KCSM_income)
  lwr_ar = c(lwr_ar, lwr)
  upr_ar = c(upr_ar, upr)
  val_ar = c(val_ar, val)
}
fmt = ","
block_ar
format(KCSM_pop_ar,big.mark=fmt,scientific=F)
format(KCSM_income_ar,big.mark=fmt,scientific=F)
format(lwr_ar,big.mark=fmt,scientific=F)
format(upr_ar,big.mark=fmt,scientific=F)
format(val_ar,big.mark=fmt,scientific=F)
format(channelcost_ar,big.mark=fmt,scientific=F)





################################
#non-log transforms
block = "E"
block_ar = vector()
KCSM_pop_ar = vector()
KCSM_income_ar = vector()
lwr_ar = vector()
upr_ar = vector()
val_ar = vector()
channelcost_ar = vector()
r2_ar = vector()
for(block in c("A", "B", "E"))
{
  #log transforms
  block_data = bid[bid[,"Block"] == block,]
  reg_data = data.frame(pwb_amount = (block_data$pwb_amount))
  reg_data$pop = block_data$pop
  reg_data$income = block_data$income
  reg_data$logpop = log(block_data$pop + 1)
  reg_data$logincome = log(block_data$income + 1)
  
  full <- lm((pwb_amount) ~ .^4 + 1, data=reg_data, y=TRUE)
  rev <- step(full, scope=formula(null), direction="backward", k=log(length(reg_data$pop)))
  summary(rev)
  
  #cald r-squared in non-log space
  ssr = sum(((predict(rev)) - mean(block_data$pwb_amount))^2)
  sse = sum(((predict(rev)) - block_data$pwb_amount)^2)
  r2 = ssr/(ssr + sse)
  r2_ar = c(r2_ar, r2)
  
  #visualize the data
  # hist(block_data$pwb_amount)
  # hist(reg_data$pwb_amount)
   plot(resid(rev))
  # hist(resid(rev))
  
  #test the KCSM example
  #License BRED-20130726AEV
  # 
  # It appears tv stations aren't given broadcasting rights over geographic
  # regions- rather, they are given broadcast rights to antanae locations,
  # directions, and and average power usage.  How do we convert this into
  # population + income?  We can get income from CMA, but pop....?
  
  
  
  #Equivalent CMAs ~ 7, 27, 175, 123, 111 - http://wireless.fcc.gov/auctions/data/maps/CMA.pdf
  #- service contour map from http://transition.fcc.gov/fcc-bin/tvq?call=KCSM
  KCSM_pop = 0
  
  if(block == "E" || block == "A")
  {
    #block E, A
    
    # KCSM TV broadcasts 24 hours a day, and our 500 kilowatt broadcast 
    # signal has a coverage area that includes San Mateo, San Francisco, 
    # Santa Clara, Santa Cruz, Alameda, Contra Costa, Marin, Solano, 
    # Sonoma and Napa counties
    path= "/SPITC/PE Activity - Financial Info/Stations by State"
    setwd(paste(Dropbox, path, sep=""))
    
    county =  read.csv("Zipcode_database.csv")
    tempcounty = county[county["state"] == "CA",]
    KCSM_pop = 0
    
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "San Mateo County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "San Francisco County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Santa Cruz County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Alameda County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Contra Costa County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Marin County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Solano County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Sanonoma County",]$estimated_population)
    KCSM_pop = KCSM_pop + sum(tempcounty[tempcounty["county"] == "Napa County",]$estimated_population)
    
    #KCSM website claims ~6M reachable auidience- comment this depending on
    #which estimate you want to use (~6M seems more reasonable based)
    #on CMA info for block B
    KCSM_pop = 6000000
    
    KCSM_income = block_data[block_data["EA"] == 163,]$income
    
    #what 6MHz of block A/E actually went for 
    #(EA is too large of an area, but still useful)
    channelcost = block_data[block_data["EA"] == 163,]$pwb_amount * KCSM_pop/block_data[block_data["EA"] == 163,]$pop
    if(block=="A")
    {
      #block a was auctioned as 12 MHz
      channelcost = channelcost / 2
    }
  }
  if(block =="B")
  {
    #block B
    KCSM_pop = 0
    
    KCSM_pop = block_data[block_data["CMA"] == 7,]$pop + sum(block_data[block_data["CMA"] == 27 | block_data["CMA"] == 175 | block_data["CMA"] == 126,]$pop / 2)
    KCSM_income = sum(block_data[block_data["CMA"] == 7 | block_data["CMA"] == 27 | block_data["CMA"] == 175 | block_data["CMA"] == 126,]$income) / 4
    
    #what 6MHz of block B actually went for
    channelcost = (block_data[block_data["CMA"] == 7,]$pwb_amount  + sum(block_data[block_data["CMA"] == 27 | block_data["CMA"] == 175 | block_data["CMA"] == 126,]$pwb_amount / 2)) / 2 
  }
  
  KCSM = data.frame(pop = KCSM_pop)
  KCSM$income = KCSM_income
  KCSM$logpop = log(KCSM$pop + 1)
  KCSM$logincome = log(KCSM$income + 1)
  
  #predict the value based on this regression
  if(block == "E")
  {
    #confidence interval
    prediction = predict(rev, newdata=KCSM, interval="prediction", se.fit=TRUE)
    lwr = (matrix(prediction$fit[,2]))
    upr = (prediction$fit[,3])
    val =(matrix(prediction$fit[,1]))
  }
  if(block == "B" || block == "A")
  {
    prediction = predict(rev, newdata=KCSM, interval="prediction", se.fit=TRUE)
    lwr = (matrix(prediction$fit[,2]))/2
    upr = (prediction$fit[,3])/2
    val = (matrix(prediction$fit[,1]))/2
  }
  block_ar = c(block_ar, block)
  channelcost_ar = c(channelcost_ar, channelcost)
  KCSM_pop_ar = c(KCSM_pop_ar, KCSM_pop)
  KCSM_income_ar = c(KCSM_income_ar, KCSM_income)
  lwr_ar = c(lwr_ar, lwr)
  upr_ar = c(upr_ar, upr)
  val_ar = c(val_ar, val)
}
fmt = ","
block_ar
r2_ar
format(KCSM_pop_ar,big.mark=fmt,scientific=F)
format(KCSM_income_ar,big.mark=fmt,scientific=F)
format(lwr_ar,big.mark=fmt,scientific=F)
format(upr_ar,big.mark=fmt,scientific=F)
format(val_ar,big.mark=fmt,scientific=F)
format(channelcost_ar,big.mark=fmt,scientific=F)

