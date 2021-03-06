Reproducible Research: Peer Assessment 1
========================================
by Anton Kuiper
github repo with RMarkdown source code:
https://github.com/antonkuiper/RepData_PeerAssessment1
date : 14 February 2015


## 1 Loading and preprocessing the data

```r
# Load packages
suppressMessages(library(dplyr))
library(knitr)
library(ggplot2)
# set working directory and download file
setwd("D:/Coursera_Reproducible Research/assignment1/data")
file <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(file,destfile = "D:/Coursera_Reproducible Research/assignment1/data/repdata-data-activity.zip" ,method="curl")
```

```
## Warning: running command 'curl
## "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" -o
## "D:/Coursera_Reproducible
## Research/assignment1/data/repdata-data-activity.zip"' had status 127
```

```
## Warning in download.file(file, destfile = "D:/Coursera_Reproducible
## Research/assignment1/data/repdata-data-activity.zip", : download had
## nonzero exit status
```

```r
dateDownloaded <- date()
dateDownloaded
```

```
## [1] "Sun Feb 15 20:59:59 2015"
```

```r
# prepare dataset
unzip("repdata-data-activity.zip")
activity <- read.csv("activity.csv")
activitydf <- tbl_df(activity)
```


```

## 2 What is the mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
steps.date_inclNA <- aggregate(steps ~ date, data=activity, FUN=sum)
hist(steps.date_inclNA$steps, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(steps.date_inclNA$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date_inclNA$steps)
```

```
## [1] 10765
```

## 3 What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# preprocessing data for plot
# use the dataset without the NAs
steps_by_interval <- aggregate(steps ~ interval, activitydf, mean)

# create a time series plot 
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# find row with max of steps
max_steps_row <- which.max(steps_by_interval$steps)

# find interval with this max
steps_by_interval[max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```


## 4 Imputing missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

I will use the means for the 5-minute intervals as fillers for missing
values.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
data_imputed <- activity
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- steps_by_interval[
      steps_by_interval$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}
# because in the dataset steps_by_interval has this steps_by_interval <- aggregate(steps ~ interval, activitydf, mean)
# each interval step has its own mean value, and that is inputed into the dataset!
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the mean and median total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data=data_imputed , FUN=sum)
hist(steps.date$steps, main="Histogram of total number of steps per day (NA value = mean of interval",      xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10766.19
```

The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.


##  5 Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("zaterdag", "zondag")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=activity,
                            subset=activity$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l", main=type)
}
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
