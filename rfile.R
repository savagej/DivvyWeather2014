# change this folder to wherever you put your cloned git repo
setwd("~/Downloads/The Data/")

# function for adding days that have no activity back in to data frames
add.missing.days <- function (arr,miss) {
  missingDays <- as.data.frame(matrix(c(days2014[miss],0),nrow=1,ncol=2))
  missingDays[,1] <- as.Date(as.POSIXct(missingDays[,1],origin="1970-01-01"))
  colnames(missingDays) <- colnames(arr)
  arr <- rbind(arr[1:(miss-1),],missingDays,arr[-(1:(miss-1)),])
  rownames(arr) <- as.character(c(1:nrow(arr)))
  return(arr)
}

#trips2014q1q2 <- read.csv("Divvy_Stations_Trips 2014/Divvy_Stations_Trips_2014_Q1Q2/Divvy_Trips_2014_Q1Q2.csv")
#headtrips2014q1q2$gender <- (as.numeric(trips2014q1q2$gender)-1)
#trips2014q3_07 <- read.csv("Divvy_Stations_Trips 2014/Divvy_Stations_Trips_2014_Q3Q4//Divvy_Trips_2014-Q3-07.csv")
#trips2014q3_0809 <- read.csv("Divvy_Stations_Trips 2014/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-0809.csv")
#trips2014q4 <- read.csv("Divvy_Stations_Trips 2014/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q4.csv")
###trips <- rbind(trips2014q4,trips2014q3_0809,trips2014q3_07,trips2014q1q2)
#trips$gender <- as.factor(trips$gender)
#trips$starttime <- as.POSIXct(strptime(trips$starttime,"%m/%d/%Y %H:%M"))
#trips$stoptime <- as.POSIXct(strptime(trips$stoptime,"%m/%d/%Y %H:%M"))
#rm(trips2014q1q2,trips2014q3_07,trips2014q3_0809,trips2014q4)

# load in previously created trips and weather data
# see forecast.R file for details on getting weather data
load("trips.RData")
load("weather2014.RData")
weather2014<-weather2014[1:365,] #only need actual days in 2014
weather2014$cloudCover[5] <- 0 #fix NA in cloud cover

# Make array of days in 2014
days2014 <- seq(as.POSIXct("2014/1/1"),as.POSIXct("2014/12/31"),"days")

# function for adding days that have no activity back in to data frames
add.missing.days <- function (arr,miss) {
  missingDays <- as.data.frame(matrix(c(days2014[miss],0),nrow=1,ncol=2))
  missingDays[,1] <- as.Date(as.POSIXct(missingDays[,1],origin="1970-01-01"))
  colnames(missingDays) <- colnames(arr)
  arr <- rbind(arr[1:(miss-1),],missingDays,arr[-(1:(miss-1)),])
  rownames(arr) <- as.character(c(1:nrow(arr)))
  return(arr)
}

# function for analysing all the trips into statistics for each day in 2014
analyse.trips <- function (tripData) {
  tripsPerDay <- aggregate(rep(1,length(tripData$starttime)) ~ cut(tripData$starttime,"day"),data = tripData, FUN = sum)
  tripTimePerDay <- aggregate( (tripData$stoptime - tripData$starttime) ~ cut(tripData$starttime,"day"), data=tripData,FUN=sum)
  colnames(tripsPerDay) <- c("days","counts")
  colnames(tripTimePerDay) <- c("days","totaltime")
  #add back in any days that are missing due to lack of activity
  tripsPerDay$days <- as.Date(tripsPerDay$days)
  tripTimePerDay$days <- as.Date(tripTimePerDay$days)
  for (var in 1:length(days2014)) {
    if (as.Date(days2014[var]) != tripsPerDay$days[var]) {
      tripsPerDay <- add.missing.days(tripsPerDay,var)
    }
    if (as.Date(days2014[var]) != tripTimePerDay$days[var]) {
      tripTimePerDay <- add.missing.days(tripTimePerDay,var)
    }
  }
  # make array of average trip length for each day
  averageTripLength <- tripTimePerDay$totaltime / tripsPerDay$counts
  averageTripLength[is.nan(averageTripLength)] <- 0
  result <- cbind(tripsPerDay,tripTimePerDay$totaltime,averageTripLength)
  colnames(result) <- c("days","numTrips","totalTripTime","averageTripLength")
  return(result)
}

# make various arrays of statistics of ridership for each day of the year for various rider types
all_trips <- analyse.trips(trips)
male_trips <- analyse.trips(trips[trips$gender==2,])
female_trips <- analyse.trips(trips[trips$gender==1,])
nonmember_trips <- analyse.trips(trips[trips$gender==0,])
member_trips <- analyse.trips(trips[(trips$gender==2|trips$gender==1),])


plot_fit <- function (weather,datum) {
  plot(weather,datum)
  lm.out <- lm(as.numeric(datum) ~ weather)
  lm2.out <- lm(as.numeric(datum) ~ poly(weather,2,raw=T))
  lm3.out <- lm(as.numeric(datum) ~ poly(weather,3,raw=T))
  lmexp.out <- lm(log(as.numeric(datum)+0.000001) ~ weather)
  points(weather, predict(lm.out), type="p", col="red", lwd=1)
  points(weather, predict(lm2.out), type="p", col="green", lwd=1)
  points(weather, predict(lm3.out), type="p", col="blue", lwd=1)
  points(weather, exp(predict(lmexp.out)), type="p", col="yellow", lwd=1)
}

#Figure1
plot(weather2014$time,all_trips$numTrips,type="l",col="blue",axes=F,ylab="",xlab="")
axis(labels=c(seq(from=0,to=18000,by=1000)),side=4,tck=-0.015,at=c(seq(from=0,to=18000,by=1000)))
mtext("Daily Number of Trips",side=4,line=2)
par(new=T)
plot(weather2014$time,(weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,type="l",col="red",xlab="Date",ylab="Average Daily Temperature")
legend("topleft",c("Temperature","Number of trips"),lty=1,col=c("red","blue"))

#Figure2
plot((weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,all_trips$numTrips,xlim=c(-40,100),ylim=c(0,18000),col="red",axes=F,xlab="Average Daily Temperature",ylab="Number of Trips per Day")
box()
axis(labels=c(seq(from=-40,to=100,by=10)),side=1,tck=-0.015,at=c(seq(from=-40,to=100,by=10)))
axis(labels=c(seq(from=0,to=18000,by=2000)),side=2,tck=-0.015,at=c(seq(from=0,to=18000,by=2000)))

#Figure3
plot((weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,member_trips$numTrips,xlim=c(-40,100),ylim=c(0,14000),col="green",axes=F,xlab="Average Daily Temperature",ylab="Number of Trips per Day")
par(new=T)
plot((weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,nonmember_trips$numTrips,xlim=c(-40,100),ylim=c(0,14000),col="blue",axes=F,xlab="",ylab="")
box()
axis(labels=c(seq(from=-40,to=100,by=10)),side=1,tck=-0.015,at=c(seq(from=-40,to=100,by=10)))
axis(labels=c(seq(from=0,to=14000,by=2000)),side=2,tck=-0.015,at=c(seq(from=0,to=14000,by=2000)))
legend("topleft",c("member","nonmember"),pch=1,col=c("green","blue"))

#Figure4
plot((weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,all_trips$averageTripLength,xlim=c(-40,100),ylim=c(0,30),col="red",axes=F,xlab="Average Daily Temperature",ylab="Average Trip Length per Day")
box()
axis(labels=c(seq(from=-40,to=100,by=10)),side=1,tck=-0.015,at=c(seq(from=-40,to=100,by=10)))
axis(labels=c(seq(from=0,to=60,by=5)),side=2,tck=-0.015,at=c(seq(from=0,to=60,by=5)))

#Figure5
plot((weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,member_trips$averageTripLength,xlim=c(-40,100),ylim=c(0,45),col="green",axes=F,xlab="Average Daily Temperature",ylab="Average Trip Length per Day")
par(new=T)
plot((weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,nonmember_trips$averageTripLength,xlim=c(-40,100),ylim=c(0,45),col="blue",axes=F,xlab="",ylab="")
box()
axis(labels=c(seq(from=-40,to=100,by=10)),side=1,tck=-0.015,at=c(seq(from=-40,to=100,by=10)))
axis(labels=c(seq(from=0,to=60,by=5)),side=2,tck=-0.015,at=c(seq(from=0,to=60,by=5)))
legend("topright",c("member","nonmember"),pch=1,col=c("green","blue"))

#Figure6
mem_line <- lm(as.numeric(member_trips$numTrips) ~ weather2014$precipProbability)
nonmem_line <- lm(as.numeric(nonmember_trips$numTrips) ~ weather2014$precipProbability)
plot(weather2014$precipProbability,member_trips$numTrips,xlim=c(0,1),ylim=c(0,12000),col="green",axes=F,xlab="Cloud Cover",ylab="Number of Trips Per Day")
par(new=T)
plot(weather2014$precipProbability,nonmember_trips$numTrips,xlim=c(0,1),ylim=c(0,12000),col="blue",axes=F,xlab="",ylab="")
abline(mem_line,col="green",lwd=4)
abline(nonmem_line,col="blue",lwd=4)
box()
axis(labels=c(seq(from=0,to=1,by=0.1)),side=1,tck=-0.015,at=c(seq(from=0,to=1,by=0.1)))
axis(labels=c(seq(from=0,to=12000,by=1000)),side=2,tck=-0.015,at=c(seq(from=0,to=12000,by=1000)))
legend("topright",c("member","nonmember"),pch=1,col=c("green","blue"))

#Figure7
mem_line <- lm(as.numeric(member_trips$numTrips) ~ weather2014$cloudCover)
nonmem_line <- lm(as.numeric(nonmember_trips$numTrips) ~ weather2014$cloudCover)
plot(weather2014$cloudCover,member_trips$numTrips,xlim=c(0,1),ylim=c(0,12000),col="green",axes=F,xlab="Cloud Cover",ylab="Number of Trips Per Day")
par(new=T)
plot(weather2014$cloudCover,nonmember_trips$numTrips,xlim=c(0,1),ylim=c(0,12000),col="blue",axes=F,xlab="",ylab="")
abline(mem_line,col="green",lwd=4)
abline(nonmem_line,col="blue",lwd=4)
box()
axis(labels=c(seq(from=0,to=1,by=0.1)),side=1,tck=-0.015,at=c(seq(from=0,to=1,by=0.1)))
axis(labels=c(seq(from=0,to=12000,by=1000)),side=2,tck=-0.015,at=c(seq(from=0,to=12000,by=1000)))
legend("topright",c("member","nonmember"),pch=1,col=c("green","blue"))

#Figure cloud vs precip
plot(weather2014$cloudCover,(weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2)
plot((weather2014$apparentTemperatureMin+weather2014$apparentTemperatureMax)/2,weather2014$cloudCover)
plot(weather2014$precipProbability,weather2014$cloudCover)

#Figure8
plot(weather2014$windSpeed,member_trips$numTrips,xlim=c(0,25),ylim=c(0,12000),col="green",axes=F,xlab="Wind Speed",ylab="Number of Trips Per Day")
par(new=T)
plot(weather2014$windSpeed,nonmember_trips$numTrips,xlim=c(0,25),ylim=c(0,12000),col="blue",axes=F,xlab="",ylab="")
box()
axis(labels=c(seq(from=0,to=25,by=5)),side=1,tck=-0.015,at=c(seq(from=0,to=25,by=5)))
axis(labels=c(seq(from=0,to=12000,by=1000)),side=2,tck=-0.015,at=c(seq(from=0,to=12000,by=1000)))
legend("topright",c("member","nonmember"),pch=1,col=c("green","blue"))
