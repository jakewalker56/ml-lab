location = "~/github"
path= "/ml-lab/scripts/"
setwd(paste(location, path, sep=""))


library(RPostgreSQL)
library(yaml)
library(doBy)
library(ggplot2)
library(maps)

source("../utilities/naref.R")
source("../utilities/convert_to_numeric.R")

# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")
config = yaml.load_file("../config/job_posting_analysis.yml")

db = config$db

con <- dbConnect(drv, dbname=db$name,host=db$host,port=db$port,user=db$user,password=db$password)

table = "job_postings"

#seems to be about 1.5 seconds per 1000 rows
#current count (Jan 13) = 223674
n = 10000

dbExistsTable(con, table)
#jobs <- dbReadTable(con, table)

#jobs_query <- dbSendQuery(con,paste("select * from ", table, " limit ", n))
#jobs <- fetch(jobs_query,n=n)

jobs_columns = c("id", "content", "salary_range_low", "salary_range_high", "updated_at", "title", "company_name", "latitude", "longitude", "source", "job_posted_time", "city", "state")
jobs <- dbGetQuery(con,paste("select ", paste(jobs_columns, collapse=", "), " from ", table, " limit ", n))
jobs <- naref(jobs)
jobs$latitude = convert_to_numeric(jobs$latitude)
jobs$longitude = convert_to_numeric(jobs$longitude)

common_titles = sort(table(jobs$title),decreasing=TRUE)[1:10]
jobs$adjusted_title <- as.character(jobs$title)
jobs[!(jobs$title %in% names(common_titles)),]$adjusted_title <- "Other"



# overwrite=TRUE will change both data and table structure
# When row.name=TRUE then column named row.names will be added to the table
#dbWriteTable(con, c("db_name","new_table_name"), value=myTable,overwrite=TRUE,row.names=FALSE)
#Append data to table:
#dbWriteTable(con, c("db_name","new_table_name"), value=myTable,append=TRUE, row.names=FALSE)

summary(jobs)

# Close PostgreSQL connection 
dbDisconnect(con)
dbUnloadDriver(drv)

################### Data Loaded, proceed with analysis #############

plot(sort(jobs$city))
plot(jobs$state)

num_states = length(unique(jobs$state))
display_states = 10
states = summaryBy(state ~ state, FUN=c(length), keep.names = FALSE, data=jobs)
states = states[states$state.length > states[order(states$state.length),][num_states-display_states,]$state.length,]
summary(states)
ggplot(states, aes(x = reorder(state, -state.length), y = state.length)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(name="State") +
  scale_y_continuous(name="New Job Postings")

num_cities = length(unique(jobs$city))
display_cities = 10
cities = summaryBy(city ~ city, FUN=c(length), keep.names = FALSE, data=jobs)
cities = cities[cities$city.length > cities[order(cities$city.length),][num_cities-display_cities,]$city.length,]
ggplot(cities, aes(x = reorder(city, -city.length), y = city.length)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name="City") +
  scale_y_continuous(name="New Job Postings")

states <- map_data("state")
#states <- subset(states, region %in% c( "illinois", "indiana", "iowa", "kentucky", "michigan", "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin" ) )
titles_to_show = sort(table(jobs$adjusted_title),decreasing=TRUE)
p <- ggplot()
p <- p + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point(data=jobs, aes(x=longitude, y=latitude, color=adjusted_title))
p <- p + scale_colour_discrete( name="Job Title",
                                breaks=names(titles_to_show[order(titles_to_show)]),
                                labels=paste(names(titles_to_show[order(titles_to_show)])," (", titles_to_show[order(titles_to_show)], ")", sep="")
                              )
p

p <- ggplot()
p <- p + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point(data=jobs, aes(x=longitude, y=latitude, color=search_term))
p

