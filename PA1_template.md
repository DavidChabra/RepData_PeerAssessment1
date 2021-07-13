---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

First we will load the data using read.csv()


```r
data <- read.csv("./activity.csv")
```

## What is mean total number of steps taken per day?

we will now group the data by date then summarize it so we get
total steps by date.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data <- group_by(data, date)
total_data <- summarise(data, total = sum(steps))
```

Now we will plot a histogram of the total number of steps per each day.


```r
hist(total_data$total, 
     xlab = "total steps per day", 
     main = "total steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Finally we will calculate the mean followed by the median of total steps.


```r
mean(total_data$total, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(total_data$total, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

First we will regroup Values that we can use the summarize function To take the mean in groups defined by which five minute interval they are in.


```r
data <- ungroup(data)
data <- group_by(data, interval)
interval_means <- summarise(data, mean = mean(steps, na.rm = TRUE))
```

We will now make the plot using interval_means.


```r
plot(interval_means$interval, 
     interval_means$mean, 
     type = "l", 
     xlab = "interval of day", 
     ylab = "mean steps", 
     main = "mean steps for interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Finally we will use the max() function to find which value for interval contains the highest number of average steps.


```r
filter(interval_means, mean == max(mean))$interval
```

```
## [1] 835
```

## Imputing missing values

First we will calculate the number of NA's within the data set.


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Next we will replace all NA's with the mean for their day. Some dates have no measurements so no mean, so we will replace those days with the mean of all other means.


```r
data <- ungroup(data)
data <- group_by(data, date)
date_mean <- summarise(data, mean = mean(steps))
date_mean$mean[is.na(date_mean$mean)] <- rep(mean(date_mean$mean,
                                                  na.rm = TRUE), 
                                        sum(is.na(date_mean$mean))
                                             )

test <- data$steps
new_data <- ungroup(data)

for(i in 1:nrow(data)) {
    if(is.na(test[i])) {
        new_data$steps[i] <- filter(date_mean,
                                    new_data$date[i] == date)[[2]]
    }
}
```

Finally we shall create a histogram of total steps for our new imputed data set. After this we shall compute the mean followed by the median.


```r
new_data <- group_by(new_data, date)
total_new_data <- summarise(new_data, total = sum(steps))

hist(total_new_data$total, 
     xlab = "total steps per day", 
     main = "total steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(total_new_data$total)
```

```
## [1] 10766.19
```

```r
median(total_new_data$total)
```

```
## [1] 10766.19
```

Overall the impact of the imputing seems to be relatively minor. The histogram appears quite similar and the mean and median values are closer together but have not changed that much. It seems to me that using the means as our method of imputing has centered the data slightly more, but otherwise has done very little.

## Are there differences in activity patterns between weekdays and weekends?

First we will add a new factor variable to our new data set new_data.


```r
new_data <- ungroup(new_data)
new_data$date <- as.Date(new_data$date)
week_or_weekend <- function(x) {
    if (x == "Saturday" | x == "Sunday") {
        output <- "weekend"
    } else {
        output <- "weekday"
    }
    output
}

new_data <- mutate(new_data, 
                   week_part = sapply(weekdays(new_data$date),
                                      week_or_weekend
                                      )
                   )
new_data$week_part <- as.factor(new_data$week_part)
```

Finally we will create a plot comparing the number of steps by intervals between weekends and days. In order to do that we will first create data for the mean measurements on each interval for weekends and weekdays.


```r
new_data <- group_by(new_data, week_part, interval)
means_nd <- summarise(new_data, mean = mean(steps))
```

```
## `summarise()` has grouped output by 'week_part'. You can override using the `.groups` argument.
```

```r
weekday <- filter(means_nd, week_part == "weekday")
weekend <- filter(means_nd, week_part == "weekend")

par(mfrow = c(2, 1))
plot(weekday$interval, 
     weekday$mean, 
     type = "l", 
     xlab = "interval of day", 
     ylab = "mean steps", 
     main = "weekday")

plot(weekend$interval, 
     weekend$mean, 
     type = "l", 
     xlab = "interval of day", 
     ylab = "mean steps", 
     main = "weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


