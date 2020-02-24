Reproducible Research - Course project 1
========================================

Author: Joris Puttenstein

This file has been created as an assignment of the Coursera course
Reproducible Research by the Johns Hopkins University.

The following script is structured using the research questions of the
original assignment.

Date: 24-02-2020

\#Loading and preprocessing the data

    activity <- read.csv("activity.csv")

\#What is mean total number of steps taken per day?

\#\#1. Total number of steps taken per day

    sum(activity$steps, na.rm = TRUE)/length(unique(activity$date))

    ## [1] 9354.23

Answer: The total number of steps taken per day is 9354.

\#\#2. Histogram of the total number of steps taken each day

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:Hmisc':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    by_date <- group_by(activity, date)
    StepsPerDay <- summarise(by_date, sum(steps))
    hist(StepsPerDay$`sum(steps)`,
         breaks = 10,
         main = "Total Number of Steps taken each Day",
         xlab = "Number of Steps per Day",
         ylab = "Frequency")

![](PA1_template_files/figure-markdown_strict/Figure%201-1.png)

\#\#3. Mean and median of the total number of steps taken per day

    mean(StepsPerDay$`sum(steps)`, na.rm = TRUE)

    ## [1] 10766.19

    median(StepsPerDay$`sum(steps)`, na.rm = TRUE)

    ## [1] 10765

\#What is the average daily activity pattern?

    by_interval <- group_by(activity, interval)
    AvgStepsPerInterval <- summarise(by_interval, mean(steps, na.rm = TRUE))

\#\#1. Time series plot of the average number of steps taken

    plot(AvgStepsPerInterval,
        type = "l",
        main = "Average Number of Steps taken in Interval",
        xlab = "Interval",
        ylab = "Average Number of Steps")

![](PA1_template_files/figure-markdown_strict/Figure%202-1.png)

\#\#2. 5-minute interval with the maximum number of steps across all
days in the dataset

    i_MaxAvg <- which(AvgStepsPerInterval[,2] == max(AvgStepsPerInterval[,2]))
    AvgStepsPerInterval[i_MaxAvg, 1]

    ## # A tibble: 1 x 1
    ##   interval
    ##      <int>
    ## 1      835

\#Imputing missing values

\#\#1. Total number of missing values in the dataset

Answer: The total number of missing values in the dataset is:

    sum(is.na(activity$steps))

    ## [1] 2304

\#\#2. Strategy for filling in all of the missing values in the dataset

    NoOfNAPerDay <- summarise(by_date, sum(is.na(steps)))
    unique(NoOfNAPerDay[, 2])

    ## # A tibble: 2 x 1
    ##   `sum(is.na(steps))`
    ##                 <int>
    ## 1                 288
    ## 2                   0

    sum(is.na(StepsPerDay[, 2]))

    ## [1] 8

\#\#3. Creation of new dataset that is equal to the original dataset but
with the missing data filled in.

    activity_new <- activity

    j <- nrow(AvgStepsPerInterval)
    for (i in 1:nrow(activity)) {
        activity_new[i, 1] <- ifelse(i %% j != 0,
                                 
                                ifelse(is.na(activity[i, 1]), 
                                    AvgStepsPerInterval[i %% j, 2], 
                                    activity[i, 1]),
                                
                                ifelse(is.na(activity[i, 1]), 
                                    AvgStepsPerInterval[j, 2], 
                                    activity[i, 1]))
    }

\#\#4. Histogram of the total number of steps taken each day

    by_date_new <- group_by(activity_new, date)
    StepsPerDay_new <- summarise(by_date_new, sum(steps))

    hist(StepsPerDay_new$`sum(steps)`,
         breaks = 10,
         main = "Total Number of Steps taken each Day (NA's imputed)",
         xlab = "Number of Steps per Day",
         ylab = "Frequency")

![](PA1_template_files/figure-markdown_strict/Figure%203-1.png)

\#\#\#4.1 Mean and median of total number of steps taken per day

    mean(StepsPerDay_new$`sum(steps)`)

    ## [1] 10766.19

    median(StepsPerDay_new$`sum(steps)`)

    ## [1] 10766.19

\#\#\#\#Do these values differ from the estimates from the first part of
the assignment?

The mean value is not different from the estimate from the first part of
the assignment, since adding the average of a vector does not influence
the average of the resulting vector.

On the other hand, the median HAS changed, since the number of days with
which it is calculated is different now.

\#\#\#\#What is the impact of imputing missing data on the estimates of
the total daily number of steps?

    sum(activity_new$steps)/length(unique(activity_new$date))

    ## [1] 10766.19

\#Are there differences in activity patterns between weekdays and
weekends?

    activity_new$Weekday <- ifelse(as.POSIXlt(activity_new$date)$wday < 6,
                                   "Weekday", "Weekend")

\#\#2. Panel plot comparing the average number of steps taken per
5-minute interval across weekdays and weekends.

    by_interval_new <- group_by(activity_new, interval, Weekday)
    AvgStepsPerInterval_new <- summarise(by_interval_new, mean(steps))

    par(mfrow = c(1, 2), mar = c(4, 4, 6, 1))

    plot(subset(AvgStepsPerInterval_new, Weekday == "Weekday")$interval,
         subset(AvgStepsPerInterval_new, Weekday == "Weekday")$`mean(steps)`,
         type = "l",
         main = "Weekdays",
         xlab = "Interval",
         ylab = "Average Number of Steps")

    plot(subset(AvgStepsPerInterval_new, Weekday == "Weekend")$interval,
         subset(AvgStepsPerInterval_new, Weekday == "Weekend")$`mean(steps)`,
         type = "l",
         main = "Weekends",
         xlab = "Interval",
         ylab = "Average Number of Steps")

    title(main="Average Number of Steps taken in Interval", outer = T)

![](PA1_template_files/figure-markdown_strict/Figure%204-1.png)

Conclusion: During weekdays, people are more active during in the
morning. In the weekend, people are more active in the afternoon.
