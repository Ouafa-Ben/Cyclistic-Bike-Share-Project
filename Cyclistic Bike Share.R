# **Google Cyclistic Case Study**

# **Introduction:**

#In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

#Until now, Cyclistic's marketing strategy relied on building general awareness and appealing to broad consumer segments.One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes,and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

#Cyclistic's finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers. The director of marketing, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

#Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members.

# **Phase1: ASK**

# **Identify the business task**

#In order to achieve the goal of helping the marketing team to convert casual riders into annual members, I have to analyze the Cyclistic historical bike trip data to answer the following questions: 
  
#1. How annual members and casual riders differ?
#2. Why casual riders would buy a membership?
  
# **Team and stakeholders**
  
#As a junior data analyst working in the marketing analyst team at Cyclistic I will be working with the following stakeholder:
  
#1. Lily Moreno: Director of marketing and my manager. She is responsible for the development of campaigns and initiative to promote the bike-share program.

#2. Cyclistic Executive team: They are  responsible on deciding whether to approve the recommended marketing program.

#3. Cyclistic marketing analytics team: a team of data analysts who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy.

# **Phase 2: Prepare**

#I downloaded the last 12 months Cyclistic trip data from [link](http://divvy-tripdata.s3.amazonaws.com/index.html). The data has been made available by 
# Motivate International Inc. under this license [license](http://ride.divvybikes.com/data-license-agreement).

install.packages("tidyverse")
library(tidyverse)
library(readr)
## upload data 
df_2021_04 <- read_csv("Project/202104-divvy-tripdata.csv")
df_2021_05 <- read.csv("Project/202105-divvy-tripdata.csv")
df_2021_06 <- read.csv("Project/202106-divvy-tripdata.csv")
df_2021_07 <- read.csv("Project/202107-divvy-tripdata.csv")
df_2021_08 <- read.csv("Project/202108-divvy-tripdata.csv")
df_2021_09 <- read.csv("Project/202109-divvy-tripdata.csv")
df_2021_10 <- read.csv("Project/202110-divvy-tripdata.csv")
df_2021_11 <- read.csv("Project/202111-divvy-tripdata.csv")
df_2021_12 <- read.csv("Project/202112-divvy-tripdata.csv")
df_2022_01 <- read.csv("Project/202201-divvy-tripdata.csv")
df_2022_02 <- read.csv("Project/202202-divvy-tripdata.csv")
df_2022_03 <- read.csv("Project/202203-divvy-tripdata.csv")

## understand my data

glimpse(df_2021_04)
glimpse(df_2021_05)
glimpse(df_2021_06)
glimpse(df_2021_07)
glimpse(df_2021_08)
glimpse(df_2021_09)
glimpse(df_2021_10)
glimpse(df_2021_11)
glimpse(df_2021_12)
glimpse(df_2022_01)
glimpse(df_2022_02)
glimpse(df_2022_03)

##Change started_at, ended_at to dttm
##Now change data type in all of them to Date

library(lubridate)
df_2021_05 <- mutate(df_2021_05, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2021_06 <- mutate(df_2021_06, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2021_07 <- mutate(df_2021_07, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2021_08 <- mutate(df_2021_08, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2021_09 <- mutate(df_2021_09, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2021_10 <- mutate(df_2021_10, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2021_11 <- mutate(df_2021_11, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2021_12 <- mutate(df_2021_12, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2022_01 <- mutate(df_2022_01, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2022_02 <- mutate(df_2022_02, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
df_2022_03 <- mutate(df_2022_03, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))


##Check again

glimpse(df_2021_04)
glimpse(df_2021_05)
glimpse(df_2021_06)
glimpse(df_2021_07)
glimpse(df_2021_08)
glimpse(df_2021_09)
glimpse(df_2021_10)
glimpse(df_2021_11)
glimpse(df_2021_12)
glimpse(df_2022_01)
glimpse(df_2022_02)
glimpse(df_2022_03)

#Bind all datasets into one

trips_list <- list(df_2021_04, df_2021_05, df_2021_06, df_2021_07, df_2021_08, df_2021_09, df_2021_10, df_2021_12, df_2021_11, df_2022_01, df_2022_02, df_2022_03)
trips_list <- bind_rows(trips_list)
glimpse(trips_list)

# Further examining to understand my data 

summarise(trips_list, unique(member_casual))
summarise(trips_list, unique(rideable_type))


# Check the number of NA values

sum(is.na(trips_list$start_station_name)) + sum(is.na(trips_list$end_station_name))
# NA values = 54230

#Replace empty values with NA values

trips_list <- trips_list %>% 
  mutate_if(is.character, list(~na_if(.,""))) 

#Check again number of NA values 

sum(is.na(trips_list$start_station_name)) + sum(is.na(trips_list$end_station_name))
# NA values = 1541623

# Remove all NA values

trips_list <- na.omit(trips_list)

#Final check for NA values 

sum(is.na(trips_list$start_station_name)) + sum(is.na(trips_list$end_station_name))
#NA values = 0

#Remove Duplicates

## Count unique rows
length(unique(trips_list$ride_id))

## Remove duplicates
trips_list<- trips_list %>% 
  distinct(.keep_all = TRUE)

## check for duplicates
length(unique(trips_list$ride_id))== nrow(trips_list)

## Add rides_length 
trips_list <- trips_list %>% 
  mutate(trips_list, rides_length = ended_at - started_at)

## convert ride_length to min
trips_list <- trips_list %>%
  mutate(rides_length = (hms::hms(seconds_to_period(rides_length))))
glimpse(trips_list)

# Check for negative rides_length

trips_list %>% 
  filter(ended_at < started_at) %>% 
  count()

# Remove negative rides_length

trips_list <- trips_list %>% 
  filter(rides_length>0)

# Final Check 

trips_list %>% 
  filter(ended_at < started_at) %>% 
  count()


## Rename Column names
trips_list<- rename(trips_list, bike_type = rideable_type)
trips_list <- rename(trips_list, user_type = member_casual)
colnames(trips_list)

## Add hour, day, month, year, date
trips_list<-trips_list %>% 
  mutate(date = date(trips_list$started_at))
trips_list <-trips_list %>%
  mutate(day = wday(trips_list$started_at, label = TRUE, abbr = TRUE))
trips_list <-trips_list %>%
  mutate(month = month(trips_list$started_at, label = TRUE, abbr = TRUE))
trips_list <-trips_list %>%
  mutate(year = year(trips_list$started_at))
trips_list <- trips_list %>% 
  mutate(hour = hour(trips_list$started_at))

glimpse(trips_list)


## Reorder days of the week
trips_list$day<- ordered(trips_list$day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Summary table

summary(trips_list)

# User type percentage 

trips_list %>% 
  group_by(user_type) %>%
  summarize(count = n(), percentage = ((count/nrow(trips_list)) *100)) %>%
  arrange(count)

#Visualising user types

ggplot(data = trips_list)+ geom_bar(mapping = aes(x = user_type, fill = user_type))+
  labs(title = 'User Type Count', x = 'User type', y = 'Count')

# Usage of different bike types

trips_list %>% 
  group_by(bike_type) %>%
  summarize(count = n(), percentage = ((count/nrow(trips_list)) *100)) %>%
  arrange(count)

# Summary of different bike types usage by users

trips_list %>% 
  group_by(user_type, bike_type) %>%
  summarize(count = n(), percentage = ((count/nrow(trips_list)) *100)) %>%
  arrange(count)

# Visualising the usage of bike types based on user type

ggplot(data = trips_list)+ geom_bar(mapping = aes(x = user_type, fill = user_type)) +
  facet_wrap(~bike_type)+ 
  labs(title = 'Types of bikes used by user type', x = 'User type', y = 'Count')


# User type rides per day 

trips_list %>% 
  group_by(day, user_type) %>%
  summarize(count = n(), percentage = ((count/nrow(trips_list)) *100)) %>%
  arrange(day)

# User type rides per day 

ggplot(trips_list)+ geom_bar(mapping = aes(x = day, fill = day))+
  facet_wrap(~user_type)+
  labs(title = "User type rides/day", x = "day_of_week")

# User type and rides/month

trips_list %>% 
  group_by(month, user_type) %>%
  summarize(count = n(), percentage = ((count/nrow(trips_list)) *100)) %>%
  arrange(month)

# User type and rides/month

ggplot(trips_list)+ geom_bar(mapping = aes(x = month, fill = month))+
  facet_wrap(~user_type)+
  labs(title = "User type and rides/month", x = "Months")

# Type bikes used per month

trips_list %>%
  group_by(month, bike_type, user_type) %>%
  summarize(count = n(), percentage = ((count/nrow(trips_list)) *100)) %>%
  arrange(month)

# Type bikes used per month

ggplot(trips_list)+ geom_bar(mapping = aes(x = month, fill = month))+
  facet_wrap(~bike_type)+
  labs(title = "Type of bikes used per month", x = "Months")

# Rush hours by user type 

trips_list %>% 
  group_by(hour, user_type) %>%
  summarize(count = n(), percentage = ((count/nrow(trips_list)) *100)) %>%
  arrange(hour)

# Rush hours by user type 

ggplot(trips_list)+ geom_bar(mapping = aes(x = hour, fill = hour))+
  facet_wrap(~user_type)+
  labs(title = "Rush hours by user type", x = "Time (hr)")

# Find mean, median, max and min trip duration

trips_list %>%
  summarize(avg_length = mean(rides_length)/60, 
            median_length = median(rides_length)/60, 
            max_length = max(rides_length)/3600, 
            min_length = min(rides_length))

#Note: I converted the units of Mean/Median to min, Maximum length to hours and Minimum length to seconds

# Find mean, median, max and min trip duration for each casual users 

trips_list %>%
  filter(user_type == "casual") %>%
  summarize(avg_length = mean(rides_length)/60, 
            median_length = median(rides_length)/60, 
            max_length = max(rides_length)/3600, 
            min_length = min(rides_length))

#Note: I converted the units of Mean/Median to min, Maximum length to hours and Minimum length to seconds

# Find mean, median, max and min trip duration for each member users 

trips_list %>%
  filter(user_type == "member") %>%
  summarise(avg_length = mean(rides_length)/60, 
            median_length = median(rides_length)/60, 
            max_length = max(rides_length)/3600,
            min_length = min(rides_length))


#Note: I converted the units of Mean/Median to min, Maximum length to hours and Minimum length to seconds

# Visualise User type and mean rides length per day 

trips_list %>%  
  group_by(user_type, day) %>% 
  summarise(mean_rides_length = mean(rides_length)) %>%
  ggplot(aes(x = day, y = mean_rides_length, fill =user_type)) +
  geom_col(position = 'dodge') + 
  labs(title = 'Average rides length by weekday and customer type', x = 'Day of week', y = 'Average rides length')

# Visualise User type and mean rides length per month

trips_list %>%  
  group_by(user_type, month) %>% 
  summarise(mean_rides_length = mean(rides_length)) %>%
  ggplot(aes(x = month, y = mean_rides_length, fill =user_type)) +
  geom_col(position = 'dodge') + 
  labs(title = 'Average rides length by month and customer type', x = 'Month', y = 'Average rides length')

# **Step 6: Act**
# **Conclusions**

#1. Member users are more than casual users by 12%. 
#2. Most classic bikes users are members and only casual users use docked bikes.
#3. Both casual and member users like to use classic bikes more than other bike types.
#4. Casual riders use the bike for 32 min on average while members use the service for 13 min.
#5. Casual riders use the bike more times during vacation months, especially in July and August.
#6. Members use the bike more during the week especially Wednesday and Tuesday and trip duration stays consistent during the week but increases during the weekend.
#7. Both customers use cyclistic bikes more around 16:00 and 18:00. 
#8. Casual users ride the bike for a longer duration during spring and the beginning of summer April, May, and June.

# **Recomendations**

#1. Create a new weekend membership for casual users and run the marketing campain on weekends, rush hours and summer holidays.
#2. Increase the number of docked bikes since they are used only by casual users this may encourge them to subscribe to the annual membership. 
#3. Collect data related to age range, sex, ethnicity, occupation, purpose of journey and address.
#4. Conduct a customer satisfaction survey. 

# **Sources:**

#* www.google.com

#* www.stackoverflow.com

#* www.r-bloggers.com

#* www.kaggle.com
