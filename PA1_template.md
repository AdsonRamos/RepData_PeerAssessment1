---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())

```r
activity <- read.csv("activity.csv")
```
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activityTotalStepsByDate <- aggregate(x = activity$steps, by = list(activity$date), sum, na.rm = TRUE)
names(activityTotalStepsByDate)[1] <- "Date"
names(activityTotalStepsByDate)[2] <- "Total.Steps"
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
head(activityTotalStepsByDate)
```

```
##         Date Total.Steps
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(activityTotalStepsByDate$Total.Steps, breaks = 10, main = "Total number of steps taken each day", xlab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(aggregate(x = activity$steps, by = list(activity$date), sum, na.rm = TRUE)[2][,1])
```

```
## [1] 9354.23
```

```r
median(aggregate(x = activity$steps, by = list(activity$date), sum, na.rm = TRUE)[2][,1])
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activityBySteps <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
plot(activityBySteps$interval, activityBySteps$steps, type = "l", main = "Time series plot of the average number of steps taken", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
activityBySteps[activityBySteps$steps == max(activityBySteps$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Replace all NAs with mean of 5-minute interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dataForFillNAs <- activity
for (i in activityBySteps$interval) {
    dataForFillNAs[dataForFillNAs$interval == i & is.na(dataForFillNAs$steps), ]$steps <- 
        activityBySteps$steps[activityBySteps$interval == i]
}
head(dataForFillNAs)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
sum(is.na(dataForFillNAs))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsFilled <- aggregate(steps ~ date, data = dataForFillNAs, sum, na.rm = TRUE)
hist(stepsFilled$steps, xlab = "Steps", main = "Total number of steps taken each day", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(stepsFilled$steps)
```

```
## [1] 10766.19
```

```r
median(stepsFilled$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
dayType <- vector()
days <- weekdays(ymd(dataForFillNAs$date))
for (i in 1:nrow(dataForFillNAs)) {
    if (days[i] == "sábado" | days[i] == "domingo") {
        dayType[i] <- "Weekend"
    } else {
        dayType[i] <- "Weekday"
    }
}
dataForFillNAs$dayType <- dayType
dataForFillNAs$dayType <- factor(dataForFillNAs$dayType)

stepsByDay <- aggregate(steps ~ interval + dayType, data = dataForFillNAs, mean)
names(stepsByDay) <- c("interval", "dayType", "steps")
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data

```r
library(lattice)
xyplot(steps ~ interval | dayType, stepsByDay, type = "l", layout = c(1, 2),
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->




