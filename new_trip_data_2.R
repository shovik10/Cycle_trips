d1<-read.csv("202004-divvy-tripdata.csv.csv")
d2<-read.csv("202005-divvy-tripdata.csv.csv")
colnames(d1)
colnames(d2)

(d1<-rename(d1,
            trip_id=ride_id,
            bikeid=rideable_type,
            start_time=started_at,
            end_time=ended_at,
            from_station_name=start_station_name,
            from_station_id=start_station_id,
            to_station_name=end_station_name,
            to_station_id=end_station_id,
            usertype=member_casual))

colnames(d1)
(d2<-rename(d2,
            trip_id=ride_id,
            bikeid=rideable_type,
            start_time=started_at,
            end_time=ended_at,
            from_station_name=start_station_name,
            from_station_id=start_station_id,
            to_station_name=end_station_name,
            to_station_id=end_station_id,
            usertype=member_casual))

colnames(d2)
View(d2)

str(d1)
str(d2)

all_trips<-bind_rows(d1,d2)

View(all_trips)
all_trips<-all_trips%>% select(-c(start_lat,end_lat,start_lng,end_lng))

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

all_trips$date<-as.Date(all_trips$start_time)

all_trips$day<-day(all_trips$date)

all_trips$weekday<-weekday(all_trips$date,labels= TRUE)
library(lubridate)

all_trips$weekday<-wday(all_trips$date)

all_trips$year<-strftime(all_trips$date, "%Y")

library(lubridate)
library(tidyverse)

View(all_trips)

getwd()

all_trips$ride_length<-difftime(all_trips$end_time,all_trips$start_time)

str(all_trips)

all_trips$ride_length<-as.numeric(as.character(all_trips$ride_length))

all_trips_v2<-all_trips[!all_trips$from_station_name=="Eckhart Park" | all_trips$ride_length<0,]

mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

#comparing members and casual users
aggregate(all_trips_v2$ride_length~all_trips_v2$usertype,FUN = mean)
aggregate(all_trips_v2$ride_length~all_trips_v2$usertype,FUN = median)
aggregate(all_trips_v2$ride_length~all_trips_v2$usertype,FUN = max)
aggregate(all_trips_v2$ride_length~all_trips_v2$usertype,FUN = min)

#seeing the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length~all_trips_v2$usertype+all_trips_v2$weekday,FUN = mean)

#fixing weekday in order
all_trips_v2$weekday<-ordered(all_trips_v2$weekday,levels=c("1","2","3","4","5","6","7"))

#now lets run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length~all_trips_v2$usertype+all_trips_v2$weekday, FUN = mean)

#analyzing ridership data by type and weekday
all_trips_v2%>%
  group_by(usertype,weekday)%>%
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length))%>%
  arrange(usertype,weekday)

#lets visualize the number of rides by rider type
all_trips_v2%>%
  mutate(weekday=wday(start_time,label = TRUE))%>%
  group_by(usertype,weekday)%>%
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length))%>%
  arrange(usertype,weekday)%>%
  ggplot(aes(x=weekday,y=number_of_rides,fill=usertype))+geom_col(position = "dodge")

#lets create a visualization for average duration 
all_trips_v2%>%
  mutate(weekday=wday(start_time,label = TRUE))%>%
  group_by(usertype,weekday)%>%
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length))%>%
  arrange(usertype,weekday)%>%
  ggplot(aes(x=weekday,y=average_duration,fill=usertype))+geom_col(position = "dodge")

  

  
  

