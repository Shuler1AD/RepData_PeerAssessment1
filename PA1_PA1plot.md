## Introduction

Reporducible Research: Peer Assessment 1
Loading and preprocessing the data

library(base)
library(knitr)
library(lattice)
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
What is mean total number of steps taken per day?

library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")


mean(total_steps, na.rm=TRUE)
## [1] 9354.23
median(total_steps, na.rm=TRUE)
## [1] 10395
What is the average daily activity pattern?

library(ggplot2)
average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                     FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps, type = "l")) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

average[which.max(average$steps), ]
##     interval    steps
## 104      835 206.1698
Imputing missing values

missing <- is.na(data$steps)
table(missing)
## missing
## FALSE  TRUE 
## 15264  2304
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
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

total_steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")


mean(total_steps)
## [1] 10766.19
median(total_steps)
## [1] 10766.19
Are there differences in activity patterns between weekdays and weekends?

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
Let’s make a panel plot containing plots of average number of steps taken on weekdays and weekends.

average <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
