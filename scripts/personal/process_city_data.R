location = "~/github"
path= "/ml-lab/scripts/"
setwd(paste(location, path, sep=""))

library(stringr)
library(reshape2)

source("../utilities/convert_to_numeric.R")

#please don't use scientific notation for these graphs
options("scipen"=100, "digits"=10)

#SOURCE: http://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.html
#SOURCE: http://tigerweb.geo.census.gov/tigerweb/
#SOURCE: http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_pums_csv_2014&prodType=document


#"Santa Cruz", c(8701), "California"
cities = c("Boulder", "Denver", "Salt Lake City", "Seattle", "Evanston", "Portland")
pum_codes = list(c(801), c(812, 813, 814, 816), c(35001, 35003, 35004), c(11601, 11602, 11603, 11604, 11605), c(3421), c(1301, 1314, 1305))
states = c("Colorado", "Colorado", "Utah", "Washington", "Illinois", "Oregon")
i=6
aggregated_data = NULL
for(i in 1:length(cities)){
  state_pop = read.csv(paste("../data/", states[i], "_pums.csv",sep=""), sep=",")
  state_housing = read.csv(paste("../data/", states[i], "_housing.csv",sep=""), sep=",")
  pop = state_pop[state_pop$PUMA %in% pum_codes[i][[1]],]
  housing = state_housing[state_housing$PUMA %in% pum_codes[i][[1]],]
  row = data.frame(city=cities[i],state=states[i])
  #pop_weights = pop$PWGTP
  #pop_ages = pop$AGEP
  
  ########Commute Times#######
  
  #0-5, 5-10, 10-15, 15-20, 20-25, 25-30, 30-35, 35-40
  #sum the weights of all people in this range divided by the weights of all people
  pop$JWMNP = convert_to_numeric(pop$JWMNP)
  max_time = 60
  time_gap = 5
  names = c()
  for(j in seq(0,max_time - time_gap,time_gap)){
    row[paste("commute_", j, "_", j+time_gap,sep="")] = sum(pop[pop$JWMNP > j & pop$JWMNP <= (j+time_gap),]$PWGTP)/sum(pop[pop$JWMNP > 0,]$PWGTP)
    names = c(names, paste(j+1,"-", j+time_gap,sep=""))
  }
  row["commute_more"] = sum(pop[pop$JWMNP > max_time,]$PWGTP)/sum(pop[pop$JWMNP > 0,]$PWGTP)
  names = c(names, paste(max_time + 1, "+", sep=""))
  commute_cols = grep("commute",colnames(row))
  commute_names = names
  png(filename=paste("../output/", cities[i],"_commute_times.png",sep=""), width = 700, height = 300, units = "px")
  barplot(melt(row[1,commute_cols])$value,
          names=names, 
          xlab="# of Minutes in typical commute", 
          ylab="% of respondants",
          ylim=c(0,0.2),
          main=paste(cities[i],"Commute Times"))
  #Conclusion #1: people are liars.  There just HAPPEN to be peaks at 30 minutes, 45 minutes, and 1 hour?  Dirty rotten liars.
  dev.off()
  
  ########Work Start Times#######
  
  #Time of arrival at work - hour and minute (five minute increments starting at midnight)
  #boulder_pop$JWAP
  pop$JWAP = convert_to_numeric(pop$JWAP)
  pop$JWAP
  #start at 5am
  start_time = 12*5
  #end at 10am
  max_time = 12*10
  time_gap = 3
  names = c()
  for(j in seq(start_time,max_time - time_gap,time_gap)){
    row[paste("work_arrival_", floor(j/12), "_", str_pad((5*((j) %% 12)), 2, pad = "0"),sep="")] = sum(pop[pop$JWAP >= j & pop$JWAP < (j+time_gap),]$PWGTP)/sum(pop[pop$JWAP > 0,]$PWGTP)
    names = c(names, paste(floor(j/12), ":", str_pad((5*((j) %% 12)), 2, pad = "0"),sep=""))
  }
  work_arrival_cols = grep("work_arrival_",colnames(row))
  work_arrival_names = names
  png(filename=paste("../output/", cities[i],"_work_arrival_times.png",sep=""), width = 900, height = 300, units = "px")
  barplot(melt(row[1,work_arrival_cols])$value,
          names=names, 
          xlab="Work Arrival Time", 
          ylab="% of respondants",
          ylim=c(0,0.09),
          main=paste(cities[i],"Work Arrival Times"))
  dev.off()
  
  #######Marital Status########
  
  pop$MAR = factor(pop$MAR)
  levels(pop$MAR) <- list(Married="1", Widowed="2", Divorced="3", Separated="4", Never_Married="5")
  names = c()
  for(j in levels(pop$MAR)){
    row[paste(j, "_rate",sep="")] = sum(pop[pop$MAR == j & pop$AGEP > 18,]$PWGTP)/sum(pop[pop$AGEP > 18,]$PWGTP)
    names = c(names, paste(j))
  }
  marriage_rate_cols = grep(paste(levels(pop$MAR),collapse="|"),colnames(row))
  marriage_rate_names = names
  png(filename=paste("../output/", cities[i],"_marriage_rates.png",sep=""), width = 700, height = 400, units = "px")
  barplot(melt(row[,marriage_rate_cols])$value,
          names=names, 
          xlab="Marital Status", 
          ylab="% of respondants",
          ylim=c(0,0.7),
          main=paste(cities[i],"Over-18 Marriage Statistics"))
  dev.off()
  
  ######## Marriage Length #########
  
  #year last married
  pop$MARHYP = convert_to_numeric(pop$MARHYP)
  pop$MARLEN = 0
  pop[pop$MARHYP > 0,]$MARLEN = 2016 - pop[pop$MARHYP > 0,]$MARHYP
  max_time = 24
  time_gap = 4
  names = c()
  for(j in seq(0,max_time - time_gap,time_gap)){
    row[paste("marriage_length_", j, "_", j+time_gap,sep="")] = sum(pop[pop$MARLEN > j & pop$MARLEN <= (j+time_gap),]$PWGTP)/sum(pop[pop$MARLEN > 0,]$PWGTP)
    names = c(names, paste(j+1,"-", j+time_gap,sep=""))
  }
  row["marriage_length_more"] = sum(pop[pop$MARLEN > max_time,]$PWGTP)/sum(pop[pop$MARLEN > 0,]$PWGTP)
  names = c(names, paste(max_time + 1, "+", sep=""))
  marriage_length_cols = grep("marriage_length_",colnames(row))
  marriage_length_names = names
  png(filename=paste("../output/", cities[i],"_marriage_length.png",sep=""), width = 700, height = 300, units = "px")
  barplot(melt(row[1,marriage_length_cols])$value,
          names=names, 
          xlab="Years of Marriage", 
          ylab="% of respondants",
          ylim=c(0,0.5),
          main=paste(cities[i],"Marriage Length"))
  dev.off()
  
  ######### Total Age ##########
  
  pop$AGEP = convert_to_numeric(pop$AGEP)
  max_time = 64
  time_gap = 4
  names = c()
  for(j in seq(0,max_time - time_gap,time_gap)){
    row[paste("individual_age_", j, "_", j+time_gap,sep="")] = sum(pop[pop$AGEP > j & pop$AGEP <= (j+time_gap),]$PWGTP)/sum(pop[pop$AGEP > 0,]$PWGTP)
    names = c(names, paste(j+1,"-", j+time_gap,sep=""))
  }
  row["individual_age_more"] = sum(pop[pop$AGEP > max_time,]$PWGTP)/sum(pop[pop$AGEP > 0,]$PWGTP)
  names = c(names, paste(max_time + 1, "+", sep=""))
  age_cols = grep("individual_age_",colnames(row))
  age_names = names
  png(filename=paste("../output/", cities[i],"_age.png",sep=""), width = 700, height = 300, units = "px")
  barplot(melt(row[1,age_cols])$value,
          names=names, 
          xlab="Age", 
          ylab="% of respondants",
          ylim=c(0,0.1),
          main=paste(cities[i],"Age"))
  dev.off()
  
  ######### Education Level ##########
  
  pop$SCHL = convert_to_numeric(pop$SCHL)
  #21 .Bachelor's degree
  #22 .Master's degree
  #23 .Professional degree beyond a bachelor's degree
  #24 .Doctorate degree
  
  pop$SCHL = factor(pop$SCHL)
  levels(pop$SCHL) <- list(No_Degree="0", 
                           No_Degree="1", 
                           No_Degree="2", 
                           No_Degree="3", 
                           No_Degree="4", 
                           No_Degree="5", 
                           No_Degree="6", 
                           No_Degree="7", 
                           No_Degree="8", 
                           No_Degree="9", 
                           No_Degree="10", 
                           No_Degree="11", 
                           No_Degree="12", 
                           No_Degree="13", 
                           No_Degree="14", 
                           No_Degree="15", 
                           No_Degree="16", 
                           No_Degree="17", 
                           No_Degree="18", 
                           No_Degree="19", 
                           No_Degree="20", 
                           Bachelors="21", 
                           Masters="22",
                           Masters="23",
                           PhD="24")
  names = c()
  for(j in levels(pop$SCHL)){
    row[paste(j, "_rate",sep="")] = sum(pop[pop$SCHL == j & pop$AGEP > 21,]$PWGTP)/sum(pop[pop$AGEP > 21,]$PWGTP)
    names = c(names, paste(j))
  }
  degree_cols = grep(paste(levels(pop$SCHL),collapse="|"),colnames(row))
  degree_names = names
  png(filename=paste("../output/", cities[i],"_degree_rates.png",sep=""), width = 700, height = 400, units = "px")
  barplot(melt(row[,degree_cols])$value,
          names=names, 
          xlab="Degree Granted", 
          ylab="% of respondants",
          ylim=c(0,0.6),
          main=paste(cities[i],"Degree Statistics"))
  dev.off()  
  
  ########## Wages ############
  
  #wages last 12 months
  #pop$WAGP
  
  pop$WAGP = convert_to_numeric(pop$WAGP)
  max_time = 150000
  time_gap = 10000
  names = c()
  for(j in seq(0,max_time - time_gap,time_gap)){
    row[paste("wages_", j, "_", j+time_gap,sep="")] = sum(pop[pop$WAGP > j & pop$WAGP <= (j+time_gap),]$PWGTP)/sum(pop[pop$WAGP > 0,]$PWGTP)
    names = c(names, paste(j/1000,"K",sep=""))
  }
  row["wages_more"] = sum(pop[pop$WAGP > max_time,]$PWGTP)/sum(pop[pop$WAGP > 0,]$PWGTP)
  names = c(names, paste(max_time + 1, "+", sep=""))
  wage_cols = grep("wages_",colnames(row))
  wage_names = names
  png(filename=paste("../output/", cities[i],"_wages.png",sep=""), width = 700, height = 300, units = "px")
  barplot(melt(row[1,wage_cols])$value,
          names=names, 
          xlab="Wages", 
          ylab="% of respondants",
          ylim=c(0,0.25),
          main=paste(cities[i],"Wages"))
  dev.off()
  
  ######### Mobility ##########
  #Mobility status (lived here 1 year ago)
  #boulder_pop$MIG
  #b .N/A (less than 1 year old)
  #1 .Yes, same house (nonmovers)
  #2 .No, outside US and Puerto Rico
  #3 .No, different house in US or Puerto Rico
  pop$MIG = convert_to_numeric(pop$MIG)
  #if its not 1, set it to zero
  pop[pop$MIG != 1,]$MIG <- 0
  
  row["migration_percentage"] = 1 -  sum(pop[pop$MIG == 1,]$PWGTP)/sum(pop$PWGTP)
    
  
  ######### Indoor Plumbing ##########
  
  housing$PLM = convert_to_numeric(housing$PLM)
  #boulder_housing$PLM
  #b .N/A (GQ)
  #1 .Yes, has hot and cold running water, a flush toilet, and a bathtub or shower
  #2 .No
  
  housing$PLM = factor(housing$PLM)
  levels(housing$PLM) <- list(No="0", 
                           Yes="1", 
                           No="2"
                           )
  names = c()
  for(j in levels(housing$PLM)){
    row[paste(j, "_plumbing",sep="")] = sum(housing[housing$PLM == j,]$WGTP)/sum(housing$WGTP)
    names = c(names, paste(j))
  }
  plumbing_cols = grep(paste(paste(levels(housing$PLM),"_plumbing",sep=""),collapse="|"),colnames(row))
  plumbing_names = names
  plumbing_names
  row[,plumbing_cols]
  png(filename=paste("../output/", cities[i],"_indoor_plumbing_rates.png",sep=""), width = 700, height = 400, units = "px")
  barplot(melt(row[,plumbing_cols])$value,
          names=names, 
          xlab="Indoor Plumbing?", 
          ylab="% of respondants",
          ylim=c(0,0.9),
          main=paste(cities[i],"Indoor Plumbing Statistics"))
  dev.off()  
  
  ####### Number of Rooms #######
  
  #num rooms
  #housing$RMSP
  
  housing$RMSP = convert_to_numeric(housing$RMSP)
  max_time = 10
  time_gap = 1
  names = c()
  for(j in seq(1,max_time - time_gap,time_gap)){
    row[paste("rooms_", j,sep="")] = sum(housing[housing$RMSP > j & housing$RMSP <= (j+time_gap),]$WGTP)/sum(housing[housing$RMSP > 0,]$WGTP)
    names = c(names, paste(j,sep=""))
  }
  row["rooms_more"] = sum(housing[housing$RMSP > max_time,]$WGTP)/sum(housing[housing$RMSP > 0,]$WGTP)
  names = c(names, paste(max_time + 1, "+", sep=""))
  rooms_cols = grep("rooms_",colnames(row))
  rooms_names = names
  png(filename=paste("../output/", cities[i],"_rooms_per_household.png",sep=""), width = 700, height = 300, units = "px")
  barplot(melt(row[1,rooms_cols])$value,
          names=names, 
          xlab="# of rooms", 
          ylab="% of households",
          ylim=c(0,0.2),
          main=paste(cities[i],"Rooms Per Household"))
  dev.off()
  row["rooms_average"] = sum(housing[housing$RMSP > 0,]$WGTP * housing[housing$RMSP > 0,]$RMSP)/sum(housing[housing$RMSP > 0,]$WGTP)
  
  
  ########### Average Rent #############
  
  #rent 
  #boulder_housing$RNTP
  housing$RNTP = convert_to_numeric(housing$RNTP)
  max_time = 3200
  time_gap = 100
  names = c()
  names
  for(j in seq(0,max_time - time_gap,time_gap)){
    row[paste("rent_", j,sep="")] = sum(housing[housing$RNTP > j & housing$RNTP <= (j+time_gap),]$WGTP)/sum(housing[housing$RNTP > 0,]$WGTP)
    names = c(names, paste(j,sep=""))
  }
  row["rent_more"] = sum(housing[housing$RNTP > max_time,]$WGTP)/sum(housing[housing$RNTP > 0,]$WGTP)
  names = c(names, paste(max_time + 1, "+", sep=""))
  rent_cols = grep("rent_",colnames(row))
  rent_names = names
  png(filename=paste("../output/", cities[i],"_rent_per_household.png",sep=""), width = 700, height = 300, units = "px")
  row[1,rent_cols]
  barplot(melt(row[1,rent_cols])$value,
          names=names, 
          xlab="Monthly Rent", 
          ylab="% of households",
          ylim=c(0,0.2),
          main=paste(cities[i],"Monthly Rent"))
  dev.off()
  row["rent_average"] = sum(housing[housing$RNTP > 0,]$WGTP * housing[housing$RNTP > 0,]$RNTP)/sum(housing[housing$RNTP > 0,]$WGTP)
  row["rent_average"]
  
  if(is.null(aggregated_data)){
    aggregated_data = row
  } else {
    aggregated_data = rbind(aggregated_data, row)
  }
}

aggregated_data


png(filename=paste("../output/", "aggragate_commute_times_by_city.png",sep=""), width = 1200, height = 600, units = "px")
  barplot(as.matrix(aggregated_data[,commute_cols]),
          beside=TRUE,
          names = commute_names,
          main="Commute Times by City", ylab="Total",
          col=terrain.colors(nrow(aggregated_data)))
  legend(10*(length(cities) + 1) + 1, 0.2, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggragate_work_arrival_times_by_city.png",sep=""), width = 1200, height = 600, units = "px")
  barplot(as.matrix(aggregated_data[,work_arrival_cols]),
          beside=TRUE,
          names = work_arrival_names,
          main="Work Arrival Times by City", ylab="Total",
          col=terrain.colors(nrow(aggregated_data)))
  legend(17*(length(cities) + 1) + 1, 0.08, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggragate_marriage_rates_by_city.png",sep=""), width = 1200, height = 600, units = "px")
  barplot(as.matrix(aggregated_data[,marriage_rate_cols]),
          beside=TRUE,
          names=marriage_rate_names,
          main="Marriage Rates by City", ylab="Total",
          col=terrain.colors(nrow(aggregated_data)))
  legend(3*(length(cities) + 1) + 1, 0.5, cities, cex=1.0, 
         fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggragate_marriage_length_by_city.png",sep=""), width = 1200, height = 600, units = "px")
  barplot(as.matrix(aggregated_data[,marriage_length_cols]),
          beside=TRUE,
          names=marriage_length_names,
          main="Marriage Length by City", ylab="Total",
          col=terrain.colors(nrow(aggregated_data)))
  legend(1*(length(cities) + 1) + 1, 0.4, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggragate_age_by_city.png",sep=""), width = 1200, height = 600, units = "px")
  barplot(as.matrix(aggregated_data[,age_cols]),
          beside=TRUE,
          names=age_names,
          main="Age by City", ylab="Total",
          col=terrain.colors(nrow(aggregated_data)))
  legend(1*(length(cities) + 1) + 1, 0.15, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()


png(filename=paste("../output/", "aggragate_degree_rates_by_city.png",sep=""), width = 1200, height = 600, units = "px")
  barplot(as.matrix(aggregated_data[,degree_cols]),
          beside=TRUE,
          names=degree_names,
          main="Degree Rates by City", ylab="Total",
          col=terrain.colors(nrow(aggregated_data)))
  legend(3*(length(cities) + 1) + 1, 0.6, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggragate_migration_rates_by_city.png",sep=""), width = 1200, height = 600, units = "px")
barplot(as.matrix(aggregated_data[,"migration_percentage"]),
        beside=TRUE,
        names="Migration Percentage",
        main="Percentage of Residents Moved to Current House in the Last Year", ylab="Total",
        col=terrain.colors(nrow(aggregated_data)))
legend(0*(length(cities) + 1) + 1, 0.2, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggregate_plumbing_rates_by_city.png",sep=""), width = 1200, height = 600, units = "px")
  barplot(as.matrix(aggregated_data[,grep("No_plumbing", colnames(aggregated_data))]),
                                    #plumbing_cols]),
          beside=TRUE,
          #names=degree_names,
          names=c("% Houses with No Plumbing"),
          main="Plumbing Rates by City", ylab="Total",
          col=terrain.colors(nrow(aggregated_data)))
  legend(0.5*(length(cities) + 1) + 1, 0.015, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggregate_bedrooms_by_city.png",sep=""), width = 1200, height = 600, units = "px")
barplot(as.matrix(aggregated_data[,rooms_cols]),
        beside=TRUE,
        names=rooms_names,
        main="Average Rooms by City", ylab="Total",
        col=terrain.colors(nrow(aggregated_data)))
legend(0.5*(length(cities) + 1) + 1, 0.15, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()


png(filename=paste("../output/", "aggregate_rent_by_city.png",sep=""), width = 1200, height = 600, units = "px")
barplot(as.matrix(aggregated_data[,rent_cols]),
        beside=TRUE,
        names=rent_names,
        main="Rental Prices by City", ylab="Total",
        col=terrain.colors(nrow(aggregated_data)))
legend(20*(length(cities) + 1) + 1, 0.15, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggregate_rental_value_by_city.png",sep=""), width = 1200, height = 600, units = "px")
plot(aggregated_data$rooms_average, aggregated_data$rent_average, 
        main="Rental bang-for-your-buck by City", 
        ylab="Average monthly rent", xlab="Average # of rooms",
        col=terrain.colors(nrow(aggregated_data)),
        pch=15,
        cex=2.0
     )
legend(6.25, 1200, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()

png(filename=paste("../output/", "aggregate_wages_by_city.png",sep=""), width = 1200, height = 600, units = "px")
barplot(as.matrix(aggregated_data[,wage_cols]),
        beside=TRUE,
        names=wage_names,
        main="Wages by City", ylab="Total",
        col=terrain.colors(nrow(aggregated_data)))
legend(8*(length(cities) + 1) + 1, 0.2, cities, cex=1.0, 
       fill=terrain.colors(nrow(aggregated_data)))
dev.off()



#gender
#boulder_pop$SEX
#1 .Male
#2 .Female


#Migration recode - State or foreign country code
#boulder_pop$MIGSP 3

#housing weight
#boulder_housing$WGTP

#number of people in house
#boulder_housing$NP

#num bedrooms
#boulder_housing$BDSP

#Electricity (monthly cost)
#boulder_housing$ELEP
#bbb .N/A (GQ/vacant)
#001 .Included in rent or in condo fee
#002 .No charge or electricity not used
#003..999 .$3 to $999 (Rounded and top-coded)

#property value
#boulder_housing$VALP

#family income
#boulder_housing$FINCP

#number of persons in family
#boulder_housing$NPF
