---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
# read data into "steps"
steps <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
1. Total number of steps per day

```r
totalSum <- tapply(steps$steps, steps$date, sum, na.rm = TRUE)
print(totalSum)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0        126      11352      12116      13294      15420      11015 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##          0      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##      10139      15084      13452      10056      11829      10395       8821 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##      13460       8918       8355       2492       6778      10119      11458 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##       5018       9819      15414          0      10600      10571          0 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##      10439       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336          0         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##       8841       4472      12787      20427      21194      14478      11834 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##      11162      13646      10183       7047          0
```

2. Histogram of total number of steps per day

```r
hist(totalSum, 
     breaks = seq(min(totalSum), max(totalSum), length.out = 8),
     main = "Histogram of Total Number of Steps Per Day", 
     xlab = "Total Number of Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Mean and Median of the total number of steps per day

```r
summary(totalSum)[3:4]
```

```
##   Median     Mean 
## 10395.00  9354.23
```

## What is the average daily activity pattern?
1. Time series plot of the 5-minute interval and the average number of steps taken

```r
# Find average number of steps for each interval
timeSteps <- tapply(steps$steps, steps$interval, mean, na.rm = TRUE)

# plot the times series
plot(names(timeSteps), 
     timeSteps, type = "l", 
     main = "Time Series Plot of Average Number of Steps Taken", 
     xlab = "Time (Minutes)", 
     ylab = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. 5-minute interval with maximum number of averaged steps

```r
# Find the location and value of maximum averaged steps
maxTimeStep <- timeSteps[which(timeSteps == max(timeSteps))]
n_maxTimeStep <- names(maxTimeStep)

# print these values out
print(n_maxTimeStep)
```

```
## [1] "835"
```

```r
print(maxTimeStep)
```

```
##      835 
## 206.1698
```
The maximum average value is 206.1698113 at the interval of 835.


## Imputing missing values
1. Total number of missing values in the dataset

```r
sum(is.na(steps$steps))
```

```
## [1] 2304
```

2 + 3. Create new dataset by replacing the missing values with the mean for that 5-minute interval

```r
newSteps <- data.frame(steps)

# Using for loop, go through every vlaue of the steps column and look for NA.
# If the value is NA, replace it with the average for that 5-minute interval
# from the time series vector made earlier, "timeSteps".

for (i in 1:length(newSteps$steps)) {
        if (is.na(newSteps$steps[i])) {
                newSteps$steps[i] <- timeSteps[as.character(newSteps$interval[i])]
        }
}
```

4a. Histogram of total number of steps per day with new dataset

```r
# find sum of steps for each date
newTotalSum <- tapply(newSteps$steps, newSteps$date, sum)

# plot the histogram
hist(newTotalSum, 
     breaks = seq(min(newTotalSum), max(newTotalSum), length.out = 8),
     main = "History of Total Number of Steps Per Day with New Dataset", 
     xlab = "Total Number of Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

4b. Mean and Median of the total number of steps per day with new dataset

```r
summary(newTotalSum)[3:4]
```

```
##   Median     Mean 
## 10766.19 10766.19
```

Both of the median and means have increased when the NA values were filled appropriately.

## Are there differences in activity patterns between weekdays and weekends?
1. New variable in dataset with levels "weekday" and "weekend"

```r
# change class of the date column into Date
newSteps$date <- as.Date(newSteps$date)

# create new column that classifies weekends and weekdays
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
newSteps$Day.Type <- factor((weekdays(newSteps$date) %in% weekdays), 
                         levels=c(FALSE, TRUE), 
                         labels=c('weekend', 'weekday'))
```

2. Time series plot of the 5-minute interval and the average number of steps taken by weekday and weekend

```r
# Find number of steps for each interval accordring to weekday/weekend
timeSteps2 <- tapply(newSteps$steps, list(newSteps$interval, newSteps$Day.Type), mean, na.rm = TRUE)

#create dataframe for lattice plot
df <- data.frame(c(timeSteps2[,1], timeSteps2[,2]))
colnames(df) <- "steps"
df$interval <- as.numeric(c(rownames(timeSteps2), rownames(timeSteps2)))
df$day.type <- as.factor(c(rep("weekend", times = 288), rep("weekday", times = 288)))

# Plot data
library(lattice)
xyplot(steps ~ interval|day.type, data = df, layout = c(1,2), 
       xlab = "Interval", ylab = "Number of Steps", 
       main = "Time Series Plot of Average Number of Steps Taken", 
       type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
