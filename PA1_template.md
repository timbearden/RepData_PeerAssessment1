# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
dailysteps <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
hist(dailysteps, main = "Average steps taken per day", xlab = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

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

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
walkpattern[(walkpattern == max(walkpattern))]
```

```
##   835 
## 206.2
```

## Imputing missing values


```r
checkNAs <- is.na(activity$steps)
length(checkNAs[(checkNAs == TRUE)])
```

```
## [1] 2304
```

```r
naindex <- which(is.na(activity)==TRUE) 
activity[naindex, "steps"] <- rep(walkpattern, times = 8)
newdailysteps <- tapply(activity$steps, activity$date, sum)
hist(newdailysteps, main = "New average steps taken per day", xlab = "steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

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

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
