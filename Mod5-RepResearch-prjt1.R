#required library
library(knitr)

#Unzip ActivityData uploaded locally at 
Srcfile<- "C:/Users/mbouras/Desktop/Rprog/Mod5-Prjt1/activity.csv"

Activity <- read.csv(file=Srcfile, header=TRUE, sep=",")
Activity$date<- as.Date(Activity$date)
Activity$day<- weekdays(as.Date(Activity$date))
#Activity$DateTime<- as.POSIXct(Activity$date, format="%Y-%m-%d")

##remove na from Activity DF
Activity<- na.omit(Activity)

# Gather the information for plots: sum steps/day
SumStepsperday<- tapply(Activity$steps, Activity$date, sum )

#plot histogram (Total Daily Steps )
hist(x=SumStepsperday, breaks=20, col="magenta", xlab="Total Daily Steps", ylab="Frequency", main = "Total Steps per Day")

mean(SumStepsperday)
#[1] 10766.19
median(SumStepsperday)
#[1] 10765

#Average daily steps 
AvgdailyActivity<- tapply(Activity$steps, Activity$interval, mean )
Average<- data.frame(interval=as.integer(names(AvgdailyActivity)), Avg= AvgdailyActivity) 

#plot the average on each 5min Interval
with(Average, plot(interval, Avg, type="l", xlab="5 min interval", ylab="Daily Average steps per interval" ))

#max number of steps in 5mininterval, on average/day
Average[which.max(Average$Avg), ]
#interval      Avg
#835      835 206.1698

#---------------Q3-Imputing missing values--------------------
#Report total number od missing values (NAs in DF)
sum(is.na(Activity$steps))
#[1] 2304

#NA in Activity DF, will be replaced by the mean of the 5 im interval
imputed_steps<- Activity
dfsteps<- is.na(imputed_steps$steps)

Avg<- tapply(Activity$steps, Activity$interval, mean, na.rm=TRUE)

#substitute NA with the mean in steps column in imputed_steps DF
imputed_steps$steps[dfsteps]<- Avg[as.character(imputed_steps$interval[dfsteps])]

# Gather the information for plots: sum steps/day
DailysumSteps<- tapply(imputed_steps$steps, imputed_steps$date, sum )

#plot histogram (Total Daily Steps )
hist(x=DailysumSteps, breaks=20, col="green", xlab="Total Daily Steps", ylab="Frequency", main = "Total Steps per Day with imputed data")

mean(DailysumSteps)
#[1] 10766.19
median(DailysumSteps)
#[1] 10766.19

#---------------Q4-difference in activity patterns between weekdays & weekends -------------------
Activity<- na.omit(Activity)
Activity$day<- weekdays(as.Date(Activity$date))
ActivityDF<- Activity

## Create new column which indicates the type of the days (weekday or weekend)
ActivityDF$Daytype <- ifelse(ActivityDF$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

library(plyr)

IntervalDF <- ddply(ActivityDF, .(interval, Daytype), summarize, Avg = mean(steps))

##Plot data in a panel plot for Total steps in weekday & weekends

library(ggplot2)
ggplot(IntervalDF, aes(x=interval, y=Avg, color=Daytype))+
  geom_line()+ labs(ttitle="Average Steps per Interval Based on Day Type)", x="interval", y="Average Number of Steps")+
  facet_wrap(~Daytype, ncol=1, nrow=2)

