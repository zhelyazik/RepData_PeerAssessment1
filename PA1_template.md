# Reproducible Research: Peer Assessment 1
Loading librarys and setting some settings

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
Sys.setlocale("LC_ALL","eng")
```

## Loading and preprocessing the data
setting __filePath__ and reading the table:

```r
filePath <- "activity.csv"
activity <- read.table(filePath, na.strings='NA', sep=',', header=T, 
                       colClasses=c("numeric","Date","numeric") )
```

## What is mean total number of steps taken per day?


```r
stepsPerDay <- tapply(activity$steps,activity$date, sum)

# histogram of the total number of steps taken each day
hist(stepsPerDay,col='red',main = "Histogram of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

__mean__ steps per day: 

```r
meanPerDay <- mean(stepsPerDay,na.rm=T)
meanPerDay
```

```
## [1] 10766.19
```
__median__ steps per day:

```r
medianPerDay <- median(stepsPerDay,na.rm=T)
medianPerDay
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
# creating time series 5-minute intervals
meanStepsPerInterval <- aggregate(steps ~ interval, data = activity,
                                  FUN = mean,
                                  na.rm = T)

#plotting 5-minute interval (x-axis) and the average number of steps taken,
#averaged across all days (y-axis)
plot(meanStepsPerInterval,
     type = "l",
     main = "Average interval activity due day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

5-minute interval contains the maximum number of steps:

```r
maxInterval <- meanStepsPerInterval$interval[match(max(
  meanStepsPerInterval$steps),meanStepsPerInterval$steps)]
maxInterval
```

```
## [1] 835
```

## Imputing missing values
number of rows with missing values:


```r
missing <- sum(!complete.cases(activity))
missing 
```

```
## [1] 2304
```

filling in all of the missing values in the dataset (using mean 5-minute intervals):

```r
activity.complete <- activity
na.steps<-is.na(activity.complete$steps)
activity.complete$steps[na.steps] <- meanStepsPerInterval$steps[
  match(activity.complete$interval[na.steps],meanStepsPerInterval$interval)]
```
Histogram of the total number of steps taken each day:

```r
# histogram of the total number of steps taken each day
stepsPerDay.complete <- tapply(activity.complete$steps,activity.complete$date, sum)
hist(stepsPerDay.complete, col='red')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

####Mean and median after filling missing value
new __mean__:

```r
meanPerDay.complete <- mean(stepsPerDay.complete,na.rm=T)
meanPerDay.complete
```

```
## [1] 10766.19
```
new __median__:

```r
medianPerDay.complete <- median(stepsPerDay.complete,na.rm=T)
medianPerDay.complete
```

```
## [1] 10766.19
```
The mean number of steps taken per day are the same, but median are different. Probably reason in filling missing value by mean of 5-minute intervals.

### Are there differences in activity patterns between weekdays and weekends?

Creating new factor variable __day__:

```r
activity$day <- "weekday"
activity$day[weekdays(activity$date) %in% c("Saturday","Sunday")] <- "weekend"
activity$day <- as.factor(activity$day) 
# grouping activity by day and interval
activityByDay <- group_by(activity, day,interval)
activityByDay <-summarise(activityByDay,steps=sum(steps,na.rm=T))
```

Plotting number of steps depending on day of week


```r
qplot(interval,steps,data=activityByDay,facets=day~.,geom="line")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

