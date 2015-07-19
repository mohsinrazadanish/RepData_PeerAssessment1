# Reproducible Research: Peer Assessment 1
mohsinrazadanish  
19 Jul 2015  

## Basic settings

```r
echo = TRUE  # Always make code visible
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.1.3
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

## Loading and processing the data

```r
data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",", 
                   colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
ignore.na <- na.omit(data)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.
- Make a histogram of the total number of steps taken each day


```r
ggplot(ignore.na, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png) 

- Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:


```r
total.steps <- aggregate(ignore.na$steps, list(Date = ignore.na$date), FUN = "sum")$x
mean(total.steps)
```

```
## [1] 10766.19
```

Median total number of steps taken per day:


```r
median(total.steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average.number.of.steps <- aggregate(ignore.na$steps, list(interval = as.numeric(as.character(ignore.na$interval))), FUN = "mean")
names(average.number.of.steps)[2] <- "Mean_of_Steps"

ggplot(average.number.of.steps, aes(interval, Mean_of_Steps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average.number.of.steps[average.number.of.steps$Mean_of_Steps == max(average.number.of.steps$Mean_of_Steps), ]
```

```
##     interval Mean_of_Steps
## 104      835      206.1698
```

## Imputing missing values

- The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I used the mean for that 5-minute interval to fill each NA value in the steps column.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new.data <- data 
for (i in 1:nrow(new.data)) {
    if (is.na(new.data$steps[i])) {
        new.data$steps[i] <- average.number.of.steps[which(new.data$interval[i] == average.number.of.steps$interval), ]$Mean_of_Steps
    }
}

head(new.data)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(new.data))
```

```
## [1] 0
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
ggplot(new.data, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

![](PA1_files/figure-html/unnamed-chunk-10-1.png) 

- Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:


```r
new.total.steps <- aggregate(new.data$steps, 
                           list(Date = new.data$date), 
                           FUN = "sum")$x
new.mean <- mean(new.total.steps)
new.mean
```

```
## [1] 10766.19
```

Median total number of steps taken per day:


```r
new.median <- median(new.total.steps)
new.median
```

```
## [1] 10766.19
```

Compare them with the two before imputing missing data:


```r
old.mean <- mean(total.steps)
old.median <- median(total.steps)
new.mean - old.mean
```

```
## [1] 0
```

```r
new.median - old.median
```

```
## [1] 1.188679
```

So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

## Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(new.data)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
new.data$weekdays <- factor(format(new.data$date, "%A"))
levels(new.data$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(new.data$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(new.data$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(new.data$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

-Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
average.number.of.steps <- aggregate(new.data$steps, 
                      list(interval = as.numeric(as.character(new.data$interval)), 
                           weekdays = new.data$weekdays),
                      FUN = "mean")
names(average.number.of.steps)[3] <- "Mean_of_Steps"
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.3
```

```r
xyplot(average.number.of.steps$Mean_of_Steps ~ average.number.of.steps$interval | average.number.of.steps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_files/figure-html/unnamed-chunk-15-1.png) 
