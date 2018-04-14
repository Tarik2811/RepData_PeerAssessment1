---
title: "RepData_Peerassessment2"
author: "MD"
date: "April 14, 2018"
output:
  html_document:
      keep_md: true
---
##Q1- Total number of steps per day computation


##Q1- mean and median computaion 

```r
summary(SumStepsperday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
mean(SumStepsperday)
```

```
## [1] 10766.19
```

```r
median(SumStepsperday)
```

```
## [1] 10765
```

##Q1- Plot histogram (Total Daily Steps )

![](test123_files/figure-html/Daily Total steps-1.png)<!-- -->
## Q2-Daily average steps/interval


## Q2-Plot Daily average steps/interval


```r
#plot the average on each 5min Interval
with(Average, plot(interval, Avg, type="l", xlab="5 min interval", ylab="Daily Average steps", main="Daily Average steps per interval" ))
```

![](test123_files/figure-html/Daily Total steps Avg-1.png)<!-- -->
##Q2-5-min interval with max number of step in Acitivity DF

```r
#max number of steps in 5min interval, on average/day
Average[which.max(Average$Avg), ]
```

```
##     interval      Avg
## 835      835 206.1698
```



##Q3-Imputing missing values and in activity DF


```r
knitr::opts_chunk$set(echo = TRUE)
#Report total number od missing values (NAs in DF)
sum(is.na(Activity$steps))
```

```
## [1] 0
```

```r
#NA in Activity DF, will be replaced by the mean of the 5 im interval
imputed_steps<- Activity
dfsteps<- is.na(imputed_steps$steps)

Avg<- tapply(Activity$steps, Activity$interval, mean, na.rm=TRUE)

#substitute NA with the mean in steps column in imputed_steps DF
imputed_steps$steps[dfsteps]<- Avg[as.character(imputed_steps$interval[dfsteps])]

# Gather the information for plots: sum steps/day
DailysumSteps<- tapply(imputed_steps$steps, imputed_steps$date, sum )

summary(DailysumSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

##Q3-Daily Total steps imputed missing data in activity DF

```r
#plot histogram (Total Daily Steps -imputed data)
hist(x=DailysumSteps, breaks=20, col="green", xlab="Total Daily Steps", ylab="Frequency", main = "Total Steps per Day with imputed data")
```

![](test123_files/figure-html/Daily Total steps imputed missing data -1.png)<!-- -->

##Q3-Mean and mdeian Daily Total steps computation with imputed missing data in activity DF

```
## [1] 10766.19
```

```
## [1] 10765
```

##Q4-difference in activity patterns between weekdays & weekends


```r
knitr::opts_chunk$set(echo = TRUE)
Activity<- na.omit(Activity)
Activity$day<- weekdays(as.Date(Activity$date))
ActivityDF<- Activity

## Create new column which indicates the type of the days (weekday or weekend)
ActivityDF$Daytype <- ifelse(ActivityDF$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

library(plyr)

IntervalDF <- ddply(ActivityDF, .(interval, Daytype), summarize, Avg = mean(steps))

summary(IntervalDF)
```

```
##     interval        Daytype               Avg         
##  Min.   :   0.0   Length:576         Min.   :  0.000  
##  1st Qu.: 588.8   Class :character   1st Qu.:  1.854  
##  Median :1177.5   Mode  :character   Median : 26.295  
##  Mean   :1177.5                      Mean   : 39.208  
##  3rd Qu.:1766.2                      3rd Qu.: 62.321  
##  Max.   :2355.0                      Max.   :234.103
```

##Q4-Graph depicting difference in activity patterns between weekdays & weekends 

```r
knitr::opts_chunk$set(echo = TRUE)
##Plot data in a panel plot for Total steps in weekday & weekends

library(ggplot2)
ggplot(IntervalDF, aes(x=interval, y=Avg, color=Daytype))+
  geom_line()+ labs(title="Average Steps per Interval Based on Day Type)", x="interval", y="Average Number of Steps")+facet_wrap(~Daytype, ncol=1, nrow=2)
```

![](test123_files/figure-html/difference in activity patterns between weekdays & weekends-1.png)<!-- -->

