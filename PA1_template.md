---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

The following is the description of the dataset on the website of this assignment:

>This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data set variables are:
1. **steps**: Number of steps with 5-minute interval 
2. **date**: The date on which the measurement was taken in the YYYY-MM-DD format 
3. **interval**: Identifier of interval of data gathering

To read the dataset into a data frame the following script is used:


```r
dat = read.csv('activity.csv', header = T)
names(dat)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(dat)
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

Please note that the `names`, `str`, and `head` functions are called for data inspection.

## What is mean total number of steps taken per day?

To check the mean total number of steps taken per day we need to build a histogram with the data summary:


```r
library(data.table)
```

```
## Error in library(data.table): there is no package called 'data.table'
```

```r
dat_tbl = data.table(dat)
```

```
## Error in eval(expr, envir, enclos): não foi possível encontrar a função "data.table"
```

```r
# get the summary of steps per day
dat_tbl_summary = dat_tbl[, list(total_steps = sum(steps, na.rm = TRUE)), 
                          by = date]
```

```
## Error in eval(expr, envir, enclos): objeto 'dat_tbl' não encontrado
```

And with the summarized data we build a histogram:


```r
gen_hist = function(x, title){
    hist(x, 
         zbreaks = 20,
         main = title,
         xlab = 'Total Number of Steps', 
         col = 'grey',
          cex.main = .9)
        
        mean_value = round(mean(x), 1)
        median_value = round(median(x), 1)
        
        abline(v=mean_value, lwd = 3, col = 'blue')
        abline(v=median_value, lwd = 3, col = 'red')
        
        legend('topright', lty = 1, lwd = 3, col = c("blue", "red"),
               cex = .8, 
               legend = c(paste('Mean: ', mean_value),
               paste('Median: ', median_value)))
}

gen_hist(dat_tbl_summary$total_steps, 'Steps Taken Per Day')
```

```
## Error in hist(x, zbreaks = 20, main = title, xlab = "Total Number of Steps", : objeto 'dat_tbl_summary' não encontrado
```

## What is the average daily activity pattern?


```r
# summarize dataset by interval
dat_tbl_summary_intv = dat_tbl[, list(avg_steps = mean(steps, na.rm = T)), 
                          by = interval]
```

```
## Error in eval(expr, envir, enclos): objeto 'dat_tbl' não encontrado
```

```r
# plot the time series
with(dat_tbl_summary_intv, {
        plot(interval, avg_steps, type = 'l',
             main = 'Average Steps by Time Interval',
             xlab = '5 Minute Time Interval',
             ylab = 'Average Number of Steps')
        })
```

```
## Error in with(dat_tbl_summary_intv, {: objeto 'dat_tbl_summary_intv' não encontrado
```

```r
# get the interval with the max avg steps
max_steps = dat_tbl_summary_intv[which.max(avg_steps), ]
```

```
## Error in eval(expr, envir, enclos): objeto 'dat_tbl_summary_intv' não encontrado
```

```r
max_lab = paste('Maximum Of ', round(max_steps$avg_steps, 1), ' Steps \n On ', max_steps$interval, 'th Time Interval', sep = '')
```

```
## Error in paste("Maximum Of ", round(max_steps$avg_steps, 1), " Steps \n On ", : objeto 'max_steps' não encontrado
```

```r
points(max_steps$interval,  max_steps$avg_steps, col = 'red', lwd = 3, pch = 19)
```

```
## Error in points(max_steps$interval, max_steps$avg_steps, col = "red", : objeto 'max_steps' não encontrado
```

```r
legend("topright",
       legend = max_lab,
       text.col = 'red',
       bty = 'n'
       )
```

```
## Error in as.graphicsAnnot(legend): objeto 'max_lab' não encontrado
```

## Imputing missing values

The number of missing values is:


```r
    sum(is.na(dat$steps))
```

```
## [1] 2304
```

The strategy to fill the NA values


```r
setkey(dat_tbl, interval)
```

```
## Error in eval(expr, envir, enclos): não foi possível encontrar a função "setkey"
```

```r
setkey(dat_tbl_summary_intv, interval)
```

```
## Error in eval(expr, envir, enclos): não foi possível encontrar a função "setkey"
```

```r
NA_replace = function(x,y){
        if(is.na(x)){              
                return(y)
        }
        return(x)
}

dat_tbl_miss = dat_tbl[dat_tbl_summary_intv]
```

```
## Error in eval(expr, envir, enclos): objeto 'dat_tbl' não encontrado
```

```r
dat_tbl_miss$new_steps = mapply(NA_replace,dat_tbl_miss$steps, dat_tbl_miss$avg_steps)
```

```
## Error in mapply(NA_replace, dat_tbl_miss$steps, dat_tbl_miss$avg_steps): objeto 'dat_tbl_miss' não encontrado
```

```r
dat_tbl_summary_miss = dat_tbl_miss[, list(new_steps = sum(new_steps, na.rm = T)), by = date]
```

```
## Error in eval(expr, envir, enclos): objeto 'dat_tbl_miss' não encontrado
```

```r
# check the new data set
head(dat_tbl_summary_miss)
```

```
## Error in head(dat_tbl_summary_miss): objeto 'dat_tbl_summary_miss' não encontrado
```

The following script answer the question 4 of this topic.


```r
gen_hist(dat_tbl_summary$total_steps, 'Missing Values Removed')
```

```
## Error in hist(x, zbreaks = 20, main = title, xlab = "Total Number of Steps", : objeto 'dat_tbl_summary' não encontrado
```

```r
gen_hist(dat_tbl_summary_miss$new_steps, 'Missing Values Replaced With \n Mean For Interval')
```

```
## Error in hist(x, zbreaks = 20, main = title, xlab = "Total Number of Steps", : objeto 'dat_tbl_summary_miss' não encontrado
```

## Are there differences in activity patterns between weekdays and weekends?


```r
# Function to check if its weekday or not
weekpart = function(x){
        if(x %in% c('Saturday', 'Sunday')){
                return('Weekend')
        }
        return('Weekday')
}

dat_tbl_miss$dayname = weekdays(as.Date(dat_tbl_miss$date))
```

```
## Error in as.Date(dat_tbl_miss$date): objeto 'dat_tbl_miss' não encontrado
```

```r
dat_tbl_miss$daytype = as.factor(apply(as.matrix(dat_tbl_miss$dayname), 1, weekpart))
```

```
## Error in as.matrix(dat_tbl_miss$dayname): objeto 'dat_tbl_miss' não encontrado
```

```r
dat_tbl_summary_miss = dat_tbl_miss[, list(avg_steps = mean(new_steps, na.rm = T)), by = list(interval, daytype)]
```

```
## Error in eval(expr, envir, enclos): objeto 'dat_tbl_miss' não encontrado
```

```r
str(dat_tbl_summary_miss)
```

```
## Error in str(dat_tbl_summary_miss): objeto 'dat_tbl_summary_miss' não encontrado
```


```r
library(lattice)
xyplot(avg_steps~interval | daytype, data = dat_tbl_summary_miss,
      type = 'l',
      xlab = 'Interval',
      ylab = 'Number of Steps',
      layout = c(1,2))
```

```
## Error in eval(substitute(groups), data, environment(x)): objeto 'dat_tbl_summary_miss' não encontrado
```
