#Windows
Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/SPITC/PE Activity - Financial Info/ceadata"
setwd(paste(Dropbox, path, sep=""))
bid = read.csv("bidsummary_extended.csv")
eapop = read.csv("easummary_GDP.csv")

summary(bid)
summary(eapop)

bid
eapop

bidex = c();
bidexincome = c();

for(i in 1:length(bid$EA))
{
  total = 0
  totalincome = 0
  if(bid$EA[i] != 0)
  {
    for(j in 1:length(eapop$EA))
    {
      if(eapop$EA[j] == bid$EA[i])
      {
        total = total + eapop$X2KPOPULATION[j]
        #have to do this stupid trick because R 
        #doesn't have 64-bit integer support..?
        z =  eapop$X2KPOPULATION[j]/total
        totalincome = totalincome * (1-z) + eapop$Income[j] * z
      }
    }
  }
  else if(bid$REA[i] !=0)
  {
    for(j in 1:length(eapop$REA))
    {
      if(eapop$REA[j] == bid$REA[i])
      {
        total = total + eapop$X2KPOPULATION[j]
        #have to do this stupid trick because R 
        #doesn't have 64-bit integer support...
        z =  eapop$X2KPOPULATION[j]/total
        totalincome = totalincome * (1-z) + eapop$Income[j] * z
      }
    }
  }
  else if (bid$CMA[i] !=0)
  {
    for(j in 1:length(eapop$CMA))
    {
      if(eapop$CMA[j] == bid$CMA[i])
      {
        total = total + eapop$X2KPOPULATION[j]
        #have to do this stupid trick because R 
        #doesn't have 64-bit integer support...
        z =  eapop$X2KPOPULATION[j]/total
        totalincome = totalincome * (1-z) + eapop$Income[j] * z
      }
    }
  }
  bidex[i] = total
  bidexincome[i] = totalincome
}

bid$pop = bidex
bid$income = bidexincome

isgulf = c()
for(i in 1:length(bid$REA))
{
  #special case gulf of mexico
  if(bid$REA[i] == 12 || bid$EA[i] == 176)
  {
    isgulf[i] = 1
  }
  else
  {
    isgulf[i] = 0
  }
}

bid$gulf = isgulf

reg = lm(pwb_amount ~ pop * Block * income, data=bid)
summary(reg)


write.csv(bid, "bidsummary_extended_population.csv")

#I manually removed the D block after processing, since it's not data we're 
#interested in.  Turns out, it didn't have a huge effect