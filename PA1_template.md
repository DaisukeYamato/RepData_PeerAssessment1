# Reproducible Research: Peer Assessment 1

## Loading libraries

```r
library(knitr)
library(lattice)
```


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?

```r
hist(rowsum(data$steps, data$date, na.rm = T), main = "Total number of steps taken per day", 
    xlab = "Total steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
step_total_mean = mean(rowsum(data$steps, data$date, na.rm = T))
step_total_median = median(rowsum(data$steps, data$date, na.rm = T))
print(sprintf("mean: %f", step_total_mean))
```

```
## [1] "mean: 9354.229508"
```

```r
print(sprintf("median: %d", step_total_median))
```

```
## [1] "median: 10395"
```

The mean and median total number of steps taken per day are 
9354.2295 and 10395 respectively.

## What is the average daily activity pattern?

```r
mean_omitna <- function(data) {
    mean(data, na.rm = T)
}
avg_step <- tapply(data$steps, data$interval, mean_omitna)
plot(as.integer(names(avg_step)), avg_step, type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
interval_max <- names(avg_step[avg_step == max(avg_step)])
avg_step[avg_step == max(avg_step)]
```

```
##   835 
## 206.2
```

835 has the maximum number of steps on average across all the days in the dataset.

## Imputing missing values

```r
n_na = sum(is.na(data$steps))
print(n_na)
```

```
## [1] 2304
```

```r

# NA filling by corresponding interval mean
mean_interval <- tapply(data$steps, data$interval, mean_naomit <- function(x) {
    mean(x, na.rm = T)
})
data.tmp <- data.frame(interval = as.integer(names(mean_interval)), mean_steps = mean_interval)
data.fill_na <- merge(data, data.tmp, by = "interval", sort = T)
data.fill_na <- data.fill_na[order(data.fill_na$date, data.fill_na$interval), 
    ]
data.fill_na[is.na(data.fill_na$steps), "steps"] <- data.fill_na[is.na(data.fill_na$steps), 
    "mean_steps"]
data.fill_na <- data.fill_na[, c("steps", "date", "interval")]
rownames(data.fill_na) <- seq(1, nrow(data.fill_na))

# create histogram & calculate mean/median
hist(rowsum(data.fill_na$steps, data.fill_na$date, na.rm = T), main = "Total number of steps taken per day", 
    xlab = "Total steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
step_total_mean = mean(rowsum(data.fill_na$steps, data.fill_na$date, na.rm = T))
step_total_median = median(rowsum(data.fill_na$steps, data.fill_na$date, na.rm = T))
print(sprintf("mean: %f", step_total_mean))
```

```
## [1] "mean: 10766.188679"
```

```r
print(sprintf("median: %f", step_total_median))
```

```
## [1] "median: 10766.188679"
```

The total number of missing value in the dataset is 2304.
In mean calculation, the NA-filling dataset slightly impacts 
the result compared to the row dataset.
However, the median is not affected so much.

## Are there differences in activity patterns between weekdays and weekends?

```r
type_weekdays <- function(weekdays) {
    tmp <- function(weekday) {
        x <- c("weekday", "weekend")
        fc <- factor(x, levels = c("weekday", "weekend"))
        if (weekday == "0" || weekday == "6") {
            return(fc[2])
        } else {
            return(fc[1])
        }
    }
    sapply(weekdays, tmp)
}
data.fill_na$weekdays <- as.POSIXlt(as.Date(data.fill_na$date, "%Y-%m-%d"))$wday
data.fill_na$day_type <- type_weekdays(data.fill_na$weekdays)
data.weekday <- subset(data.fill_na, data.fill_na$day_type == "weekday")
data.weekend <- subset(data.fill_na, data.fill_na$day_type == "weekend")
avg_step_weekday <- tapply(data.weekday$steps, data.weekday$interval, mean_omitna)
avg_step_weekend <- tapply(data.weekend$steps, data.weekend$interval, mean_omitna)
avg_step_data <- data.frame(steps = c(avg_step_weekday, avg_step_weekend), interval = c(names(avg_step_weekday), 
    names(avg_step_weekend)), day_type = c(rep(type_weekdays(1), length(avg_step_weekday)), 
    rep(type_weekdays(0), length(avg_step_weekend))))
par(mfrow = c(2, 1))
plot(as.integer(names(avg_step_weekday)), avg_step_weekday, type = "l", main = "weekday", 
    xlab = "interval", ylab = "mean of number of steps")
plot(as.integer(names(avg_step_weekend)), avg_step_weekend, type = "l", main = "weekend", 
    xlab = "interval", ylab = "mean of number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

