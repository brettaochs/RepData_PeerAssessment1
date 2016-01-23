# Reproducible Research: Peer Assessment 1
Brett A. Ochs  
Loading packages "dplyr" and "ggplot2" done with include="FALSE" to restrict package loading messages.

## Loading and preprocessing the data


Load data.frame with activity.csv data


```r
options(scipen = 5, digits = 1)
df.raw <- read.csv(unz("activity.zip", "activity.csv"))
summary(df.raw)
```

```
##      steps              date          interval   
##  Min.   :  0    2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0    2012-10-02:  288   1st Qu.: 589  
##  Median :  0    2012-10-03:  288   Median :1178  
##  Mean   : 37    2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12    2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806    2012-10-06:  288   Max.   :2355  
##  NA's   :2304   (Other)   :15840
```

## What is mean total number of steps taken per day?

Use dplyr summarize function to calculate the average steps per day without removing NA values. Then use ggplot2 histogram to plot frequencies of steps per day counts. NAs were removed from mean and median calculations.


```r
df.raw.summary <- tbl_df(df.raw) %>%
    group_by(date) %>%
    summarize(Avg.Steps = sum(steps, na.rm=TRUE))

ggplot(df.raw.summary, aes(x=Avg.Steps)) + 
    geom_histogram() + 
    geom_vline(xintercept=mean(df.raw.summary$Avg.Steps, na.rm=TRUE), linetype="longdash", colour="red", size=1) +
    geom_vline(xintercept=median(df.raw.summary$Avg.Steps, na.rm=TRUE), linetype="dotted", colour="blue", size=1)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

The mean number of steps per day of raw data is **9354.2** steps and is shown in red dashed line on above graph.

The median number of steps per day of raw data is **10395** steps and shown in blue dotted line on above graph.

## What is the average daily activity pattern?


```r
df.raw.time.summary <- df.raw %>%
    mutate(Time = str_pad(df.raw$interval, 4, pad="0"),
           DateTime = paste0("2015-03-11", " ", Time), # Fake date to use lubridate
           TrialInterval = ymd_hm(DateTime)) %>% 
    group_by(TrialInterval) %>%
    summarize(Reps = n(),
              Steps = mean(steps, na.rm=TRUE),
              Interval = unique(interval))
ggplot(df.raw.time.summary, aes(x=TrialInterval, y=Steps)) + geom_line() + 
    scale_x_datetime(labels=date_format("%I:%M %P"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

## Imputing missing values

There are a total of **$2304$** rows of data missing "steps" observations (i.e. steps = NA) out of **$17568$** total rows in dataset.


```r
df.imputed <- tbl_df(df.raw) %>%
    mutate(Imp.Steps = as.integer(ifelse(is.na(df.raw$steps) == TRUE, df.raw.time.summary$Steps[df.raw.time.summary$Interval %in% df.raw$interval], df.raw$steps)))
df.imputed.summary <- tbl_df(df.imputed) %>%
    group_by(date) %>%
    summarize(Avg.Steps = sum(Imp.Steps, na.rm=TRUE))
ggplot(df.imputed.summary, aes(x=Avg.Steps)) + 
    geom_histogram() + 
    geom_vline(xintercept=mean(df.imputed.summary$Avg.Steps, na.rm=TRUE), linetype="longdash", colour="red", size=1) +
    geom_vline(xintercept=median(df.imputed.summary$Avg.Steps, na.rm=TRUE), linetype="dotted", colour="blue", size=1)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
## Graph imputed dataset
df.imputed.time.summary <- df.imputed %>%
    mutate(Time = str_pad(df.imputed$interval, 4, pad="0"),
           DateTime = paste0("2015-03-11", " ", Time), # Fake date to use lubridate
           TrialInterval = ymd_hm(DateTime)) %>% 
    group_by(TrialInterval) %>%
    summarize(Reps = n(),
              Steps = mean(Imp.Steps, na.rm=TRUE),
              Interval = unique(interval))
ggplot(df.imputed.time.summary, aes(x=TrialInterval, y=Steps)) + geom_line() + 
    scale_x_datetime(labels=date_format("%I:%M %P"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)

The mean number of steps per day of imputed data is **10749.8** steps and is shown in red dashed line on above graph.

The median number of steps per day of imputed data is **10641** steps and shown in blue dotted line on above graph.

## Are there differences in activity patterns between weekdays and weekends?


```r
df.imputed.weekly <- df.imputed %>%
    mutate(day = wday(ymd(df.imputed$date), label=TRUE),
           Week.Class = ifelse(day %in% c("Sat", "Sun"), "Weekend", "Weekday"))
df.imputed.weekly.summary <- df.imputed.weekly %>%
    mutate(Time = str_pad(df.imputed$interval, 4, pad="0"),
           DateTime = paste0("2015-03-11", " ", Time), # Fake date to use lubridate
           TrialInterval = ymd_hm(DateTime)) %>% 
    group_by(TrialInterval, Week.Class) %>%
    summarize(Reps = n(),
              Steps = mean(Imp.Steps, na.rm=TRUE),
              Interval = unique(interval))
ggplot(df.imputed.weekly.summary, aes(x=TrialInterval, y=Steps)) + geom_line() + 
    scale_x_datetime(labels=date_format("%I:%M %P")) + facet_grid(Week.Class ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)
