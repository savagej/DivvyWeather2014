#install.packages("devtools")
library(devtools)
install_github("Rforecastio", "hrbrmstr")

library(Rforecastio)
#install.packages("ggplot2")
library(ggplot2)
library(plyr)

# You need to register on https://developer.forecast.io/ and get an api key.
# Then make a file that contains the key, in this case I use the file "~/.forecast.io"
fio.api.key= readLines("~/.forecast.io")

my.lat = 41.85
my.long = -87.65

fio.now <- fio.forecast(fio.api.key, my.lat, my.long)
my.date <- as.POSIXct("2014-06-03")
fio.date <- fio.forecast(fio.api.key, my.lat, my.long, my.date)
summary(fio.date)
str(fio.date$daily)


mostdays2014 <- seq(as.POSIXct("2014/1/2"), as.POSIXct("2015/1/1"), "days")
jan1st <- fio.forecast(fio.api.key,my.lat,my.long,as.POSIXct("2014/1/1"))
weather2014 <- jan1st$daily

rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

weather2014 <- jan1st$daily
for (days in mostdays2014) {
  fio.day <- fio.forecast(fio.api.key,my.lat,my.long,days)
  weather2014 <- rbind.all.columns(weather2014,fio.day$daily)
}
str(weather2014)
plot(weather2014$temperatureMin)
