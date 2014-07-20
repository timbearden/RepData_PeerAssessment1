# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The data was organized in a pretty straightforward manner, so I didn't do any preprocessing, I just read the data in. 

```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
dailysteps <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
hist(dailysteps, main = "Total steps taken per day", xlab = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The mean and median number of daily steps, respectively:

```r
mean(dailysteps)
```

```
## [1] 9354
```

```r
median(dailysteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
walkpattern <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(names(walkpattern), walkpattern, type = "l", main = "Daily walking pattern", xlab = "time", ylab = "steps taken")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The most steps taken, on average, was in the 5 minute interval beginning at 8:35am

```r
walkpattern[(walkpattern == max(walkpattern))]
```

```
##   835 
## 206.2
```

## Imputing missing values

The number of NA values:

```r
checkNAs <- is.na(activity$steps)
length(checkNAs[(checkNAs == TRUE)])
```

```
## [1] 2304
```

To replace the missing values, I created an index for the rows which contained missing values. Looking through the index and comparing it to some of the data, I noticed that the NA values came in whole day bunches. Since this was the case, it was relatively easy to replace the data in those days with the average daily patterns which I computed for problem 2. 

```r
naindex <- which(is.na(activity)==TRUE) 
activity[naindex, "steps"] <- rep(walkpattern, times = 8)
```

The histogram for total steps taken with the NAs replaced:

```r
newdailysteps <- tapply(activity$steps, activity$date, sum)
hist(newdailysteps, main = "New average steps taken per day", xlab = "steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

## Are there differences in activity patterns between weekdays and weekends?


```r
activity$date <- as.POSIXlt(activity$date)
activity[, "day"] <- weekdays(activity$date)
activity[(activity$day != c("Saturday", "Sunday")), "day"] <- "Weekday"
activity[(activity$day == c("Saturday", "Sunday")), "day"] <- "Weekend"
activity$day <- as.factor(activity$day)
daymean <- tapply(activity[(activity$day == "Weekday"), "steps"], activity[(activity$day == "Weekday"), "interval"], mean)
endmean <- tapply(activity[(activity$day == "Weekend"), "steps"], activity[(activity$day == "Weekend"), "interval"], mean)
par(mfrow = c(2, 1), mar = c(6,4,2,4))
plot(names(daymean), daymean, type = "l", main = "Weekdays", xlab = "time", ylab = "steps")
plot(names(endmean), endmean, type = "l", main = "Weekends", xlab = "", ylab = "steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
