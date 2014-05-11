# Reproducible Research: Peer Assessment 1

## Loading libraries

```r
library(knitr)
```


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
# data_total_steps <-
# data.frame(total.steps=rowsum(data$steps,data$date,na.rm=T))
# data_total_steps$date <- rownames(data_total_steps) data.2 <-
# merge(data,data_total_steps,by='date')
step_sum <- function(data) {
    sum_steps <- c()
    current_date <- ""
    steps <- data[, "steps"]
    date <- data[, "date"]
    for (i in 1:nrow(data)) {
        if (current_date != date[i]) {
            current_date <- date[i]
            sum_steps[i] <- steps[i]
        } else {
            sum_steps[i] <- sum_steps[i - 1] + steps[i]
        }
    }
    return(sum_steps)
}
data$sum_step <- step_sum(data)
data$ave_step <- data$sum_step/(data$interval + 5)
## data.na_omit <- data[!is.na(data$steps),]
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
plot(x = data$interval, y = data$ave_step, type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
data[data$ave_step == max(data$ave_step, na.rm = T) & !is.na(data$sum_step), 
    ]
```

```
##       steps       date interval sum_step ave_step
## 16786   569 2012-11-28      645     7342     11.3
```


## Imputing missing values

```r
n_na = sum(is.na(data$steps))
```

The total number of missing value in the dataset is 2304.

## Are there differences in activity patterns between weekdays and weekends?
