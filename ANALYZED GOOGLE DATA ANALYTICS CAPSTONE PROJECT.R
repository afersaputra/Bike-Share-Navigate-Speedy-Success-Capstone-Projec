#Install Packages
install.packages('janitor')
install.packages('skimr')
install.packages('here')
install.packages('hablar')
install.packages('chron')
install.packages('DescTools')
install.packages('metR')

#Calling Library for data analyze
library(tidyverse)
library(janitor)
library(skimr)
library(here)
library(hablar)
library(readxl)
library(data.table)
library(chron)
library(readr)
library(lubridate)
library(magrittr)
library(DescTools)
library(metR)

#Importing Data
Data_01 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202101-divvy-tripdata.csv')
Data_02 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202102-divvy-tripdata.csv')
Data_03 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202103-divvy-tripdata.csv')
Data_04 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202104-divvy-tripdata.csv')
Data_05 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202105-divvy-tripdata.csv')
Data_06 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202106-divvy-tripdata.csv')
Data_07 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202107-divvy-tripdata.csv')
Data_08 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202108-divvy-tripdata.csv')
Data_09 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202109-divvy-tripdata.csv')
Data_10 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202110-divvy-tripdata.csv')
Data_11 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202111-divvy-tripdata.csv')
Data_12 <- read.csv('PROJECT/GOOGLE ANALYTICS CAPSTONE PROJECT/RAW DATA/202112-divvy-tripdata.csv')

#Making Column Names for Each Data Before Join Become a Data Frame
colnames(Data_01)
colnames(Data_02)
colnames(Data_03)
colnames(Data_04)
colnames(Data_05)
colnames(Data_06)
colnames(Data_07)
colnames(Data_08)
colnames(Data_09)
colnames(Data_10)
colnames(Data_11)
colnames(Data_12)

#Making Data Frame from All Data
All_tripdata <- bind_rows(Data_01, Data_02, Data_03, Data_04, Data_05,
                          Data_06, Data_07, Data_08, Data_09, Data_10,
                          Data_11, Data_12)


ncol(All_tripdata) #There are 13 columns
nrow(All_tripdata) #There are 5595063 rows

#Cleaning Data
colnames(All_tripdata) #To see list of the column
dim(All_tripdata) #To see dimension of All_tripdata Dataframe
head(All_tripdata) #To see first 6 rows of dataframe
str(All_tripdata) #To see list of column and data type

#Making Column Names Such as Date, Month, Day, Day of Week, and 
#Year of Each Ride
All_tripdata$date <- as.Date(All_tripdata$started_at) #The default format is yyyy-mm-dd
All_tripdata$month <- format(as.Date(All_tripdata$date), '%m')
All_tripdata$day <- format(as.Date(All_tripdata$date), '%d')
All_tripdata$day_of_week <- format(as.Date(All_tripdata$date), '%u')
All_tripdata$year <- format(as.Date(All_tripdata$date), '%Y')

#Making Column Names of Ride_Lenght in Minute and Second
All_tripdata$ride_lenght <- difftime(All_tripdata$ended_at, 
                                     All_tripdata$started_at)
All_tripdata$ride_lenght_min <- (difftime(All_tripdata$ended_at,
                                          All_tripdata$started_at))/60

str(All_tripdata)

#Convert Column Name Type of Ride Length, Month and Day Column into Numeric
All_tripdata$ride_lenght <- as.numeric(as.character(All_tripdata$ride_lenght))
All_tripdata$ride_lenght_min <- as.numeric(as.character(All_tripdata$ride_lenght_min))
All_tripdata$month <- as.numeric(All_tripdata$month)
All_tripdata$day <- as.numeric(All_tripdata$day)
is.numeric(All_tripdata$ride_lenght)
is.numeric(All_tripdata$ride_lenght_min)
is.numeric(All_tripdata$month)
is.numeric(All_tripdata$day)

#There are negative value in column ride lenght, may be this is 
#start_time and end_time were swapped for these rides, or the system 
#simply registered and recorded the rides incorrectly. 
#So, negative value rides must be excluded.

All_tripdata_Ver1 <- All_tripdata[!( All_tripdata$ride_length < 0),]
write.csv(All_tripdata, file = "All_tripdata.csv")
#Descriptive Analysis of ride lenght and weekday
#The summary of statistical data
str(All_tripdata_Ver1)
All_tripdata %>% summarise(max(ride_lenght_min), 
                                min(ride_lenght_min),
                                mean(ride_lenght_min))

#Plot 1: The number of ride vs. weekday
All_tripdata %>%
  group_by(day_of_week) %>%
  summarise(The_number_of_ride = n()) %>%
  ggplot(mapping = aes(x = day_of_week, y = The_number_of_ride)) + 
  geom_col()

All_tripdata %>%
  group_by(day_of_week) %>%
  summarise(The_number_of_ride = n())

#Making plot of the ride_length or average_duration in minutes for 
#every day of the week for members and casual riders 

#Plot 2: ride_length per day per rider type
All_tripdata %>%
  group_by(member_casual, day_of_week) %>%
  summarise(The_number_of_ride = n(),
            Average_of_duration = mean(ride_lenght_min)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = Average_of_duration, 
             fill = member_casual)) + geom_col(position = 'dodge')

All_tripdata %>%
  group_by(member_casual, day_of_week) %>%
  summarise(The_number_of_ride = n(),
            Average_of_duration = mean(ride_lenght_min)) %>%
  arrange(member_casual, day_of_week)

#Plot 3: The number of rides per day per rides for every rider types
All_tripdata %>%
  group_by(member_casual, day_of_week) %>%
  summarise(The_number_of_ride = n(),
            Average_of_duration = mean(ride_lenght_min)) %>%
  ggplot(aes(x = day_of_week, y = The_number_of_ride, 
             fill = member_casual)) +
  geom_col(position = 'dodge')

#Average ride length depending on rider type and number of 
#each rider type

#Plot 4: Average of Ride Lenght Min
All_tripdata %>%
  group_by(member_casual) %>%
  summarise(max(ride_lenght_min), min(ride_lenght_min),
            Average_ride_lenght_min = mean(ride_lenght_min)) %>%
  ggplot(aes(x = member_casual, y = Average_ride_lenght_min,
             fill = member_casual)) + geom_col() +
  scale_y_continuous(breaks = seq(0, 40, by = 5))

#Plot 5: Overall Rider Count Based on Rider Types
All_tripdata %>% 
  group_by(member_casual) %>%
  summarise(Ride_count = n()) %>%
  ggplot(aes(x = member_casual, y = Ride_count, fill = member_casual)) +
  geom_col()

All_tripdata %>% 
  group_by(member_casual) %>%
  summarise(Ride_count = n())

library(metR)
#Assigning function of season from library metR
All_tripdata$season <- season(All_tripdata$month)

#Plot 6: The Number of Rides by Weekday and Rider Types and Season
All_tripdata %>%
  group_by(season, day_of_week, member_casual) %>%
  summarise(The_number_of_ride = n(),
            Average_ride_lenght_min = mean(ride_lenght_min)) %>%
  ggplot() + geom_col(mapping = aes(x = day_of_week, y = The_number_of_ride,
                                    fill = member_casual),
                      position = 'dodge') +
  facet_wrap(~season) + 
  scale_y_continuous(breaks = seq (0, 400000, by = 50000))

#Plot 7: Ride Lenght by Weekday and Rider Type and Season
All_tripdata %>%
  group_by(season, day_of_week, member_casual) %>%
  summarise(The_number_of_ride = n(),
            Average_ride_lenght_min = mean(ride_lenght_min)) %>%
  ggplot() + geom_col(mapping = aes(x = day_of_week, y = Average_ride_lenght_min,
                                    fill = member_casual),
                      position = 'dodge') +
  facet_wrap(~season) + 
  scale_y_continuous(breaks = seq (0, 50, by = 10))

#Plot 8: The Number of Rides Along the Whole Year
All_tripdata %>%
  group_by(month, member_casual) %>%
  summarise(The_number_of_ride = n(),
             Average_ride_lenght_min = mean(ride_lenght_min)) %>%
  ggplot() + geom_col(mapping = aes(x = month, y = The_number_of_ride,
                                    fill = member_casual)) +
  scale_x_continuous(breaks = seq(1, 12, by = 1))