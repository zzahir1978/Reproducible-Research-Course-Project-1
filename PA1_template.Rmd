# Reproducible Research Course Project No. 1

By Zahiruddin B. Zahidanishah
```{r computetime, echo=FALSE}
time <- format(Sys.time(), "%a, %b %d %Y, %X")
```
Published on `r time`.

## Introduction.

This assignment use data from a personal activity monitoring device such as Fitbit, Nike Fuelband or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who make measurements about themselves regularly to improve their health and to find pattern in their behavior. 

This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

As part of the reproducible research criteria, this documents is produced in a single R markdown document using Rstudio and can be processed by knitr and converted to a HTML file for final submission.

## Loading and preprocessing the data

Load the required packages for the entire assessment and set the global option with echo equals to TRUE so that code is visible to any anyone reading this markdown file.
```{r maindata, echo=TRUE}
library(dplyr)
library(ggplot2)
library(knitr)
opts_chunk$set(echo=TRUE)
```
The data for this assignment is save in a csv file name activity.csv. 
```{r, echo=TRUE}
activity <- read.csv("activity.csv")
```
The following shows the detail main summary and structure of the collected data:-
```{r data structure, echo=TRUE}
summary(activity)
str(activity)
```
Based on the above, the variables included in the datasets are as follow:-

1. **steps**: *Number of steps in a 5-minute interval (missing values are coded as NA)*

2. **date**: *The date on which the measurement was taken in YYYY-MM-DD format*

3. **interval**: *Identifier for the 5-minute interval in which measurement was taken*


## What is mean total number of steps taken per day?

Based on the given datasets, the total daily number of steps from October to November are as detail below:-
```{r total_daily_steps, echo=TRUE}
StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
StepsPerDay
```

The following shows the histogram plot for the total daily number of steps for October and November:-
```{r histogram, echo=TRUE}
g <- ggplot(StepsPerDay, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkgreen", fill="lightgreen")+ggtitle("Total Daily Number of Steps for October and November")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))
```

Therefore, the **mean** of steps take each day are as follow:-
```{r mean, echo=TRUE}
mean(StepsPerDay$Steps, na.rm=TRUE)  
```

And the **median** of steps take each day are as follow:-
```{r, echo=TRUE}
median(StepsPerDay$Steps, na.rm=TRUE)
```


## What is the average daily activity pattern?

The time series plot of the average number of steps taken are as follow:-
```{r time_series_plot, echo=TRUE}
StepsPerTime <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
StepsPerTime$time <- StepsPerTime$interval/100
h <- ggplot(StepsPerTime, aes(time, steps))
h+geom_line(col="brown")+ggtitle("Average Number of Steps per Time Interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
```

The 5 minutes interval on average across all the days which contains the maximum number of steps are as follow:-
```{r max_interval, echo=TRUE}
ST <- tbl_df(StepsPerTime)
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))
```


## Imputing missing values

Based on the total daily number of steps taken in October and November shows that there are a number of days/intervals where there are a missing values (NA). The presence of this missing values may introduce bias to some calculations or summaries of the data.

The total number of missing values in the datasets are as follow:-
```{r total_NA, echo+TRUE}
ACT <- tbl_df(activity)
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())
```

Therefore, the new datasets after imputing the missing values.
```{r datasets_without_NA, echo=TRUE}
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(StepsPerTime$steps[match(activity$interval, StepsPerTime$interval)],0), activity$steps)
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
head(activityFull, n=10)
```
The new histogram plot based on the new datasets are as follow:-
```{r histogram_new_datasets, echo=TRUE}
StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Total Daily Number of Steps for October and November")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))
```

Therefore, the new **mean** of steps take each day are as follow:-
```{r new_mean, echo=TRUE}
mean(StepsPerDayFull$Steps)
```
And the new **median** of steps take each day are as follow:-
```{r new_median, echo=TRUE}
median(StepsPerDayFull$Steps)
```

## Are there differences in activity patterns between weekdays and weekends?

In order to identify the different activity patterns between weekday and weekends, a new factor variable in the datasets is created indicating weekdays and weekends. The following shows the first 10 rows set of data.
```{r new_datasets, echo=TRUE}
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
activityFull$weekday <- weekdays(activityFull$RealDate)
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'Weekends','Weekdays')
head(activityFull, n=10)
```

Based on this, a new plot to shows both average daily steps on weekdays and weekends are as follow:-
```{r new_plot, echo=TRUE}
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)
StepsPerTimeDT$time <- StepsPerTime$interval/100
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkred")+ggtitle("Average Steps per Time Interval: Weekdays vs. Weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
```

