# Analysing Wearable Device Data



This is the first project in the Reproducible research course.  
The following content is based on questions created to analyse wearable device data and find out certain relations from it.  

The data for this analysis was downloaded from the course website.  
It contains the following variables:
* steps: Indicates the number of steps taken in a given time interval for a given day
* date: The date on which the data was collected.
* interval: Identifier for the 5 min. interval for which data was measured.


##Loading and preprocessing the data
Also we load the ggplot2 library for future use

```r
data <- read.csv("activity.csv")
steps_data <- tapply(data$steps, data$date, sum)
library(ggplot2)
```

##What is the mean total steps taken per day?
The below histogram shows the total number of steps taken per day.  
Also, the mean and median values have been calculated and displayed on the screen.

```r
hist(steps_data, xlab = "Number of steps", col = "red", main = "Total Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanData <- mean(steps_data, na.rm = TRUE)
medianData <- median(steps_data, na.rm = TRUE)
```


```r
meanData
```

```
## [1] 10766.19
```

```r
medianData
```

```
## [1] 10765
```

##What is the average daily activity pattern?
Here, we have made a time series plot of the five minute interval(on the x axis) and the average number of steps taken across all days(on the y axis).


```r
data_intervals <- aggregate(steps ~ interval , data , mean)
plot(data_intervals$interval, data_intervals$steps, type= "l", xlab = "Interval", ylab = "Number of steps", main = "Average number of steps per day by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Also calculated below is the interval containing the maximum number of steps on average across all days.

```r
max_interval <- data_intervals[which.max(data_intervals$steps),1]
max_interval
```

```
## [1] 835
```

##Imputing missing values
Firstly we calculate the total number of missing values in the entire dataset.

```r
sum(is.na(data))
```

```
## [1] 2304
```

Now we need to handle the missing data.  
So we have created a new dataset called activity which contains the same data.  
Now we check the missing values of each step in activity.  
Wherever missing values are found, we replace it with the mean of the number of steps for that particular day.


```r
activity <- data
missing_values <- is.na(activity$steps)
average_interval <- tapply(activity$steps, activity$interval,mean, na.rm = TRUE,simplify = TRUE)
activity$steps[missing_values] <- average_interval[as.character(activity$interval[missing_values])]
sum(is.na(activity))
```

```
## [1] 0
```

As we can see, the missing NA values have been addressed.

Now we compare by displaying a histogram of the imputed dataset.

```r
steps_activity <- tapply(activity$steps, activity$date,sum)
hist(steps_activity, col="blue",xlab = "Number of steps", main = "Total Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
summary(steps_data)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

```r
summary(steps_activity)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
As is evident from the summary statistics of both the imputed and non imputed data, the mean and median data have not changed much.  
However, the quantile values have changed as can be seen.

##Are there differences in activity patterns in weekdays and weekends?
To answer this question, we add a new column day which tells us which day that particular date is on.  
Furthermore, we classify it as a weekday or weekend and store it in a separate column called daytype.

```r
activity$date <- as.Date(activity$date)
activity$day <- weekdays(activity$date)
activity$daytype <- as.factor(ifelse(activity$day == "Saturday" | activity$day == "Sunday", "Weekend", "Weekday"))
head(activity)
```

```
##       steps       date interval    day daytype
## 1 1.7169811 2012-10-01        0 Monday Weekday
## 2 0.3396226 2012-10-01        5 Monday Weekday
## 3 0.1320755 2012-10-01       10 Monday Weekday
## 4 0.1509434 2012-10-01       15 Monday Weekday
## 5 0.0754717 2012-10-01       20 Monday Weekday
## 6 2.0943396 2012-10-01       25 Monday Weekday
```

Finally we plot the values for weekday and weekend to notice the differences.

```r
ggplot(activity,aes(interval, steps, color = daytype)) + geom_line() + facet_wrap(~daytype, ncol= 1, nrow =2) +ggtitle("Average Steps per Day by interval") + ylab("Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
