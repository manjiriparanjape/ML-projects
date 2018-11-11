# UBER Demand Supply Gap Analysis

#Business Objective
#Identify root cause for driver cancellation and non-availability of cars for airport rides leading to potential revenue loss for UBER
#Recommend potential solutions for the problem

#Strategy
# Use EDA - Data Cleansing, Univariate and Bivariate Analysis to see how the various factors such as travel times, travel detsinations, 
# drivers impact the Demand Supply of UBER cabs throughout the day

setwd("~/Assignment-UBER")

remove (list=ls())

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

#DATA LOADING AND PRIMARY CHECK UP
url_uber <- "https://cdn.upgrad.com/UpGrad/temp/76b3b6a4-d87d-4e82-b1c3-3f6e10b9c076/Uber%20Request%20Data.csv"
download.file(url_uber, destfile="./uber.csv")

uber <- read.csv("uber.csv" , stringsAsFactors = FALSE)
colnames(uber)
str(uber)

# GET AN IDEA OF DATA PROVIDED
sum(duplicated(uber$Request.id)) # no duplicate data
length (unique(uber$Request.id)) #6745 records
length(unique(uber$Driver.id)) #301 driver ids

#***************************************************************************************************
# REQUEST AND DROP TIMESTAMP CLEANING : Transform request and drop timestamp in regular format
# Step 1: Change time format where %H-%M to %H-%M-%S

fun_padsec <- function(x){
  if ( ! is.na(x) & (x <19)) {
    x <- paste0(x, ":00")
    
  }
  return(x)
}

uber$Request.timestamp <- sapply (uber$Request.timestamp, fun_padsec )
uber$Drop.timestamp <- sapply (uber$Drop.timestamp, fun_padsec )

#Step 2 : Get all date times in consistent seperator format converting "/" to "-")
uber$Request.timestamp <- str_replace_all(uber$Request.timestamp, "/" , "-")
uber$Drop.timestamp <- str_replace_all(uber$Drop.timestamp, "/" , "-")

#Step 3: Convert all dates to POSIXCT format for easy manipulation by R libraries & numeric operations
uber$Request.timestamp <- as.POSIXct(uber$Request.timestamp , format = "%d-%m-%Y %H:%M:%S" )
uber$Drop.timestamp <- as.POSIXct(uber$Drop.timestamp , format = "%d-%m-%Y %H:%M:%S" )

#*******************************************************************************************************

#ADD NEW COLUMNS OF DATE, TIME , HOUR FROM REQUEST.DATE ,DAYOFWEEK FOR PLOTTING AND ANALYSIS
uber$req_date <- format(uber$Request.timestamp, format = "%Y-%m-%d")
uber$req_time <- format(uber$Request.timestamp, format = "%H-%M-%S")
uber$req_hour <- substr (uber$req_time,1,2)

uber$drop_date <- format(uber$Drop.timestamp, format = "%Y-%m-%d")
uber$drop_time <- format(uber$Drop.timestamp, format = "%H-%M-%S")
uber$drop_hour <- substr (uber$drop_time,1,2)

uber$dayofweek = format(uber$Request.timestamp, format = "%A")


#********************************************************************************************************
#ADD NEW VARIABLE TRAVEL TIME TO CALCULATE TIME TAKEN FOR AIRPORT RIDE
#For rides to airport calculate the drop.timestamp - request.timestamp

uber$travel_time <- round(difftime(uber$Drop.timestamp, uber$Request.timestamp , units = "mins"),0)
median(uber$travel_time , na.rm= TRUE) # 52 mins

#******************************************************************************************************
# CALCULATE UBER SUCCESS % TO HANDLE INCOMING REQUESTS

 success_ratio <- count(filter(uber, Status == "Trip Completed")) / length(uber$Status)
 #Overall success to cater incoming requests = 42%
 
 success_pkpoint_city <- count(filter(uber, (Status == "Trip Completed" & Pickup.point == "City"))) / count(filter(uber, Pickup.point == "City"))
 # Success for city to airport rides = 42.8%
 
 success_pkpoint_Airport <- count(filter(uber, (Status == "Trip Completed" & Pickup.point == "Airport"))) / count(filter(uber, Pickup.point == "Airport"))
 # Success for airport to city rides = 41%

 
 
#********************************************************************************************************
# CALCULATE IDLE TIME OF DRIVERS AT AIRPORT
# For drivers that take ride to airport, find the interval after which they get the next ride to the city
# Step 1 > Order the data set by Driver id and Request timestamp and filter for trips completed

summ <- uber[order(uber$Driver.id, uber$Request.timestamp),]
summ <- subset(uber, Status == 'Trip Completed' )


#Step 2 : Calculate idle time as 
# Sort data by driver id and Request timestamp, check for consective ride from city to airport and airport to city
# by same  driver id and for the same request date with airport drop hours between 5 and 9
lth <- nrow(summ)
summ$idle_time <- 0
str(summ)
summ$drop_hour <- as.numeric(summ$drop_hour)
summ$req_hour <- as.numeric(summ$req_hour)
for (n in 1:(lth-1) ){

  if ((summ$Driver.id[n+1] == summ$Driver.id[n]) & (summ$req_date[n+1] == summ$drop_date[n]) 
     & (summ$Pickup.point[n] == "City") & (summ$Pickup.point[n+1] == "Airport")
     & (summ$drop_hour[n] > 4) & (summ$drop_hour[n] <=10)) { 
    #summ$idle_time[n] = summ$req_time[n+1] - summ$drop_time[n]
    summ$idle_time[n+1] = round(difftime(summ$Request.timestamp[n+1] , summ$Drop.timestamp[n], units = "mins"))
  } 

}

# Removing not applicable rows 
# Value 0 is where driver id are not same or not same date commute while calculating idle time for drivers
# Considering 10 hour shift with 2 one hour rides, max wait time is considered 8 hrs, so only value till 640 min
# are considered for analysis of idle time
idle_time_vector <- summ$idle_time [summ$idle_time != 0 & summ$idle_time<640]

median(idle_time_vector) #242 mins

# Not sufficient data available to analyze idle time to see whether driver really waiting at airport or planned wait 
# where idle time more than a couple of hours.

#*************************************************************************************************************

# PLOT INFLOW VS OUTFLOW OF CARS AT AIRPORT
str(summ)

summ$req_hour <- as.numeric(summ$req_hour)
summ$drop_hour <- as.numeric(summ$drop_hour)

summ_inflow <- summ %>%  filter(Pickup.point == "City" &  Status == "Trip Completed" ) %>%  
             group_by(drop_hour) %>%
             summarise(avg_inflow = n()) %>% 
             arrange(drop_hour)

summ_outflow <- summ %>%  filter(Pickup.point == "Airport" & Status == "Trip Completed") %>%  
  group_by(req_hour) %>% 
  summarise(avg_outflow = n()) %>% 
  arrange(req_hour)

airport_flow <- merge (summ_inflow, summ_outflow, by.x = "drop_hour", by.y = "req_hour")

#***********************************************************************************************************
#EXPORT DATA TO EXCEL TO BE USED FOR TABLEAU PLOTS

library(openxlsx)
write.xlsx(uber, file = "uber_cleaned_1.xlsx", sheetName = "my_data", append = FALSE) # UBER CLEAN DATASET WITH DERIVED COLUMNS
write.xlsx(airport_flow, file = "airport_flow.xlsx", sheetName = "my_data1", append = FALSE) # HOUR WISE INFLOW AND OUTFLOW DATA
write.xlsx(summ, file = "summary.xlsx", sheetName = "my_data", append = FALSE) # SUBSET OF UBER DATA WITH IDLE TIME CALCULATIONS

# **********************************************************************************************************
#PLOT BASIC GRAPHS
#Frequency of cars not available - by date / by hour
# % of rides lost due to cars not available
# may need lubridate
#uber$Request.timestamp <- as.POSIXct(uber$Request.timestamp , format = "%d-%m-%Y %H:%M:%S" )
#uber$Drop.timestamp <- as.POSIXct(uber$Drop.timestamp , format = "%d-%m-%Y %H:%M:%S" ) 

# UBER  Request Fulfillment Stats
g1 <- ggplot (uber, aes (Status )) + geom_histogram(stat = "count")
g1 + ggtitle("UBER Overall Health", subtitle="Request Fulfilment") + xlab("Status") + ylab("Count of requests")

# UBER Demand To/ From Airport at Various Times of the Day
# Graph shows imbalance in demands from City / Airport at morning & evening peak hrs of 5-9 & 17-21 hrs
g2 <- ggplot(uber, aes(uber$req_hour, fill = factor(Pickup.point))) + geom_bar(position = "stack")
g2 + ggtitle("Demand", subtitle="From Airport and From City") + xlab("Hour of the Day") + ylab("Requests Received")
   
#Request Fulfillment by Hour Status For Various Times of the Day
#Total height of the bar is hourly demand. The colored bar is split of request fulfilment
g3 <- ggplot (uber, aes (uber$req_hour, fill= factor(Status) )) + geom_bar(position = "stack")
g3 + ggtitle("Demand VS Supply", subtitle="Demand=Total, Supply= Trip Completed") + xlab("Hour of the Day") + ylab("Requests Received")



 # UBER Morning Cancellations for Airport
# This graph shows the IQR of cancelletions to show the range of cancelletions
str(uber)
uber$drop_hour <- as.numeric(uber$drop_hour)
uber$req_hour <- as.numeric(uber$req_hour)

summ1 <- uber %>% 
        filter(Status == "Cancelled" & Pickup.point == "City" & req_hour > 4 & req_hour <=9  ) %>% 
        group_by(Driver.id) %>% 
        summarise(cnt = length(Driver.id)) %>% 
        arrange(desc(cnt))
g4 <- ggplot(summ1, aes(x= "", y= cnt) ) +geom_boxplot()
g4 + ggtitle("Morning Cancellations for Airport") + ylab("Total Cancellations")


#*************************************************************************************************************
# SUMMARY
#UBER has only about 42% of success rate in handling total incoming requests causing potential loss of revenue

#MOST PROBLEMATIC REQUESTS
#Cancellations at Morning 5-9 am for incoming requests to airport ride amount to 39% of total morning incoming requests
#No cars available at evening hours 5-9 pm for requests from airport amount to 56.4% of total evening incoming requests 
#There is inflow VS outflow imbalance of cabs at the airport causing queuing of cabs in the morning and no cars available
# in the evening

#***************************************************************************************************************


