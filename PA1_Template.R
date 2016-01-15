#Loading and preprocessing the data
library(knitr)
library(lattice)
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

#What is mean total number of steps taken per day?
library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)

#What is the average daily activity pattern?
library(ggplot2)
average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                     FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps, type = "l")) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

average[which.max(average$steps), ]
#Imputing missing values
missing <- is.na(data$steps)
table(missing)
#5-minute interval
fill.value <- function(steps, interval){
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (average[average$interval==interval, "steps"])
  return(filled)
}
filled.data <- data 
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval) 

total_steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps)
median(total_steps)

#Are there differences in activity patterns between weekdays and weekends?
week <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=week)
#Now, let's make a panel plot containing plots of average number of steps taken on weekdays and weekends.
average <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
