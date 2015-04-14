## Peer Assessment 1


```{r}
echo = TRUE

```

### Loading and preprocessing the data
Show any code that is needed to
Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
activity <- read.csv("C:/Users/Itahisa/SkyDrive/Documentos/repdata-data-activity/activity.csv")
View(activity)
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```{r}
activity.date <- aggregate(activity[1],by=activity[2],FUN=sum,na.rm=TRUE)
hist(activity.date$steps,
     breaks=20,
     col = "red",
     main = "What is mean total number of steps taken per day?",
     xlab = "Steps per Day")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(activity.date$steps)   
median(activity.date$steps) 
```


### What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
activity.interval <- aggregate(activity[1],by=activity[3],FUN=mean,na.rm=TRUE)
plot(x=activity.interval$interval,
     y=activity.interval$steps,
     type="l",
     main="Average Steps Per 5-Minute Interval",
     xlab="Interval",
     ylab="Number of Steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max_interval <- activity.interval[activity.interval$steps==max(activity.interval$steps),]

max_interval[1] 
round(max_interval[2],1)
```

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
nrow(activity)
sum(is.na(activity$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity.impute <- ddply(activity, ~interval, transform, steps = impute.mean(steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

```{r}
activity.impute.date <- aggregate(activity.impute[1],by=activity.impute[2],FUN=sum,na.rm=TRUE)
hist(activity.impute.date$steps,
     breaks=20,
     col = "red",
     main = "Histogram of Total Number of Steps Taken per Day (Imputed Data)",
     xlab = "Steps per Day")

mean(activity.impute.date$steps)
median(activity.impute.date$steps)
```

The mean and the median are now almost the same after replacing missing values with the mean value for the relevant interval.

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
activity.impute$dateP <- as.POSIXlt(activity.impute$date,format="%Y-%m-%d")
activity.impute$day <- "Weekday"
activity.impute$day [weekdays(activity.impute$dateP) %in% c("Saturday","Sunday")] <- "Weekend"
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
activity.impute.interval <- aggregate(activity.impute[1],
                                   by=activity.impute[c(3,5)],
                                   FUN=mean,
                                   na.rm=TRUE)

library(lattice)
xyplot(steps~interval | day, data = activity.impute.interval,
       type = 'l', 
       xlab = 'Interval',
       ylab = 'Number of Steps',
       layout = c(1,2))
```


