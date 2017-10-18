# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
options(scipen=1, digits=2)
library(dplyr)
library(ggplot2)

unzip("activity.zip")
data <- read.csv("activity.csv", as.is = TRUE)
data$date <- as.Date(data$date)
data.no.na <- data[!is.na(data$steps),]
```

## What is mean total number of steps taken per day?

```r
steps.by.date <- data.no.na %>% group_by(date) %>% summarise(total.steps = sum(steps))
ggplot(steps.by.date, aes(total.steps)) + geom_histogram() + ggtitle("Histogram of the Total Number of Steps Taken Each Day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

```r
steps.mean <- mean(steps.by.date$total.steps, na.rm = TRUE)
steps.median <- median(steps.by.date$total.steps, na.rm = TRUE)
```
Mean total number of steps taken per day is 10766.19.  
Median total number of steps taken per day is 10765.  

## What is the average daily activity pattern?


```r
steps.by.interval <- data.no.na %>% group_by(interval) %>% summarise(mean.steps = mean(steps))
ggplot(steps.by.interval, aes(interval, mean.steps)) + geom_line() + ggtitle("Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

```r
max.interval <- steps.by.interval$interval[which.max(steps.by.interval$mean.steps)]
```
The 5-minute interval 835, on average across all the days in the dataset, contains the maximum number of steps.

## Imputing missing values

Total number of missing values in the dataset is 2304.
We fill in the missing values by the mean for that 5-minute interval.


```r
na.idx <- is.na(data$steps)
data.na <- data[na.idx,]
data.na.joined <- data.na %>% left_join(steps.by.interval, by="interval")
data.na.filled <- data
data.na.filled[na.idx, "steps"] <- data.na.joined$mean.steps

steps.by.date.filled <- data.na.filled %>% group_by(date) %>% summarise(total.steps = sum(steps))
ggplot(steps.by.date.filled, aes(total.steps)) + geom_histogram() + ggtitle("Histogram of the Total Number of Steps Taken Each Day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

```r
steps.mean.filled <- mean(steps.by.date.filled$total.steps, na.rm = TRUE)
steps.median.filled <- median(steps.by.date.filled$total.steps, na.rm = TRUE)
```
Mean total number of steps taken per day is 10766.19.  
Median total number of steps taken per day is 10766.19.

Imputing missing data increases the median to be closer to the mean. It also increases the daily number of steps for those days with some NA steps originally.

## Are there differences in activity patterns between weekdays and weekends?


```r
data2 <- data.no.na
data2$dow <- weekdays(data2$date)
data2$weekday.or.weekend <- if_else(data2$dow %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
steps.by.interval.2 <- data2 %>% group_by(weekday.or.weekend, interval) %>% summarise(mean.steps=mean(steps))

ggplot(steps.by.interval.2, aes(interval, mean.steps)) + geom_line() + ggtitle("Average Daily Activity Pattern") + facet_grid(weekday.or.weekend ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

