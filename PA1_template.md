---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
First we will unzip the file, download it and save it to the `data` variable. In order to make plotting easier, we'll convert the dates to a date format. Also we'll add the weekdays to the dataframe to make future plots easier to construct. Then we apply the `summary` function to it to get a quick overview of the data it contains. We will also download the ggplot2 package to plot the last graph. 


```r
library(ggplot2)
unzip("activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")
weekday <- weekdays(data$date)
data <- cbind(data,weekday)
summary(data)
```

```
##      steps             date               interval        weekday         
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
```

## What is mean total number of steps taken per day?
First we subset the data by date and steps, excluding the NA values, and sum them. Here's the histogram showing the data. 


```r
data_totalsteps <-  with(data, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(data_totalsteps) <- c("date", "steps")
hist(data_totalsteps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "lightgreen", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


We calculate the mean of the total number of steps taken per day as follows.


```r
mean(data_totalsteps$steps)
```

```
## [1] 9354.23
```
Then we obtain the median of the toal number of steps taken per day like this.


```r
median(data_totalsteps$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
We create the time series plot by first subsetting the data per interval and then calculating the mean. We make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
data_dailyactivity <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
names(data_dailyactivity) <- c("interval", "mean")
plot(data_dailyactivity$interval, data_dailyactivity$mean, type = "l", lwd=2, xlab = "Interval", ylab = "Average number of steps per intervals", main = "Average number of steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



Which 5-minute interval, on aversge across all the days in the dataset, contains the maximum number of steps? 

```r
data_dailyactivity[which.max(data_dailyactivity$mean), ]$interval
```

```
## [1] 835
```

## Imputing missing values
This is the total number of missing values in the dataset. 


```r
colSums(is.na(data))
```

```
##    steps     date interval  weekday 
##     2304        0        0        0
```
We will fill in the missing values in *steps* with the mean of the intervals subset we created earlier. First we match both datasets by the number of interval.


```r
imputing_steps <- data_dailyactivity$mean[match(data$interval, data_dailyactivity$interval)] 
```

Then we create a new dataset with the missing data filled in. 


```r
data_imputedsteps <- transform(data, steps=ifelse(is.na(data$steps), yes=imputing_steps, no=data$steps))
data_totalsteps_imputed <- aggregate(steps ~ date, data_imputedsteps, sum)
names(data_totalsteps_imputed) <- c("date", "dailysteps")
```

Now we can plot a new histogram. 


```r
hist(data_totalsteps_imputed$dailysteps, col = "lightgreen", xlab = "Total steps per day", main = "Total number of steps taken each day", breaks = seq(0,25000, by=2500), ylim = c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



We calculate the mean of the total number of steps taken each day as follows.


```r
mean(data_totalsteps_imputed$dailysteps)
```

```
## [1] 10766.19
```
Then we obtain the median of the toal number of steps taken each day like this.


```r
median(data_totalsteps_imputed$dailysteps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data$datetype <- sapply(data$date, function(x) {
        if(weekdays(x) == "sábado" | weekdays(x) == "domingo")
        {y <- "Weekend"} else
        {y <- "Weekday"}
        y
})
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
data_activitybydate <- aggregate(steps ~ interval+datetype, data, mean, na.rm=TRUE)
graph <- ggplot(data_activitybydate, aes(interval, steps, color=datetype)) + geom_line(show.legend = FALSE) + labs(title = "Average daily steps by type of date", x="Interval", y="Average number of steps") + facet_wrap(~datetype, ncol = 1, nrow = 2) 
print(graph)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->






