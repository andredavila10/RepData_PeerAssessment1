Peer Research: Course Project 1
=================================

## Loading and processing data
First, we may unzip the file, read data and show its summary:

```r
unzip("./activity.zip")
data <- read.csv("./activity.csv")
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
stepsDay <- aggregate(steps ~ date, data, sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(stepsDay$steps, main = "Number of steps per day", xlab = "Total steps taken per day", col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
meanStepsDay <- mean(stepsDay$steps)
print(meanStepsDay)
```

```
## [1] 10766.19
```

```r
medStepsDay <- median(stepsDay$steps)
print(medStepsDay)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsInterval <- aggregate(steps~interval, data=data, mean, na.rm=TRUE)
plot(steps~interval, data=stepsInterval, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- stepsInterval[which.max(stepsInterval$steps),]$interval
print(maxSteps)
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totalNas <- sum(is.na(data$steps))
print(totalNas)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
A reasonable strategy may be filling missing values with the mean per interval:

```r
intervalMean <- function(interval){
    stepsInterval[stepsInterval$interval==interval,]$steps}
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
noNaData <- data
for(i in 1:nrow(noNaData)){
    if(is.na(noNaData[i,]$steps)){
        noNaData[i,]$steps <- intervalMean(noNaData[i,]$interval)
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
noNaSteps <- aggregate(steps ~ date, data=noNaData, sum)
hist(noNaSteps$steps, main="Number of steps per day (no Missing Values)", xlab = "Total steps taken per day", col = "darkGreen")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
noNaData$date <- as.Date(strptime(noNaData$date, format="%Y-%m-%d"))
noNaData$datetype <- sapply(noNaData$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y})
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
activityWeek <- aggregate(steps~interval + datetype, noNaData, mean, na.rm = TRUE)
library(ggplot2)
plot<- ggplot(activityWeek, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2) +
        theme_linedraw() +
        scale_color_manual(values = c("darkred", "steelblue"))

print(plot)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

