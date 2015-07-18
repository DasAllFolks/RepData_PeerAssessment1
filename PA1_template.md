# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
data = read.csv('activity.csv')
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```


## What is mean total number of steps taken per day?

Per the assignment instructions, we will for now (naively) limit our analysis for this
problem to non-missing values:


```r
print(nrow(data))
```

```
## [1] 17568
```

```r
print(sum(is.na(data$steps)))
```

```
## [1] 2304
```

```r
cleaned <- data[!is.na(data$steps),]
print(nrow(cleaned))
```

```
## [1] 15264
```

```r
print(sum(is.na(cleaned$steps)))
```

```
## [1] 0
```

Find the total number of steps per day, excluding missing values:


```r
by_date <- split(cleaned, cleaned$date)
total_steps_per_day <- sapply(by_date, function (date) { sum(date$steps) })
```

Histogram of the results:


```r
hist(total_steps_per_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


## What is the average daily activity pattern?



## Imputing missing values

An interesting question to ask up front is how to deal with the issue of missing values in our data.

The assignment instructs that, "For this part of the assignment, you can ignore the missing values in the dataset."  However, it's probably wise to get an initial sense of exactly how frequent these missing values were and how they are distributed throughout the dataset:  a day which consists strictly of NA values could probably be ignored entirely from our analysis, whereas a day which requires a mix of NA and non-NA values might or might not be useful for our analysis depending on the percentage of values missing.


## Are there differences in activity patterns between weekdays and weekends?
