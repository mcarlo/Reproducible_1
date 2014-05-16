Summarize activity
================
  
  This is an R Markdown document which summarizes aspects of personal movement 
activity data collected on an anonymous individual over 5-minute intervals in 
October and November 2007.

First, we read in the data.
```{r}
#setwd("~/GitHub/Reproducible_1")
activity <- read.csv("activity.csv")
```
We wish to answer the question, "What is mean total number of steps taken per 
day?"
```{r}
byDate <- tapply(activity$steps[!is.na(activity$steps)], 
                 +activity$date[!is.na(activity$steps)],sum)
```
Let us remove NAs
```{r}
byDate <- byDate[!is.na(byDate)]
```
Now we make a histogram of the total number of steps taken each day
```{r}
hist(byDate,main = "Frequency of Steps Each Day", xlab = "#Steps")
```
Next we calculate and report the mean and median total number of steps taken per
day
```{r}
rawMean <- mean(byDate)
rawMedian <- median(byDate)
```
Now we answer the question, "What is the average daily activity pattern?"

First we make a time series plot (i.e. type = "l") of the 5-minute interval 
(x-axis) and the average number of steps taken, averaged across all days 
(y-axis)

```{r}
byInterval <- as.numeric(tapply(activity$steps[!is.na(activity$steps)], 
                                +activity$interval[!is.na(activity$steps)], 
                                +mean))
intervals <- data.frame(as.numeric(rownames(table(activity$interval))))
intervals <- cbind(intervals,byInterval)
colnames(intervals) <- c("interval","steps")
par(mfrow = c(1,1))
plot(intervals$interval,intervals$steps, type = "l", xlab = "5-minute Interval",
     +ylab = "Steps", main = "Average Number of Steps Taken")
```
Now we identify Which 5-minute interval, on average across all the days in the 
dataset, contains the maximum number of steps?

```{r}
intervals[which(intervals$steps==max(intervals$steps)),]
```

Imputing missing values
Note that there are a number of days/intervals where there are missing values 
(coded as NA). The presence of missing days may introduce bias into some 
calculations or summaries of the data.

First let us calculate and report the total number of missing values in the 
dataset (i.e. the total number of rows with NAs)

```{r}
TotalNAs <- length(activity$steps[!complete.cases(activity)])
```

Now we devise a strategy for filling in all of the missing values in the 
dataset. The strategy does not need to be sophisticated. For example, we could 
use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing 
data filled in.

While we do not HAVE to use a sophisticated strategy, I choose to use one: I 
will use the package mice: Multivariate Imputation by Chained Equations.

```{r}
require(mice)
imputedActivity <- complete(mice(activity))
```
Next we will make a histogram of the total number of steps taken each day
```{r}
hist(byDate2,main = "Frequency of Steps Each Day (NAs imputed)", xlab = "#Steps")
```

Now we calculate and report the mean and median total number of steps taken per 
day. 
```{r}
byDate2 <- tapply(imputedActivity$steps,imputedActivity$date,sum)
meanImputed <- mean(byDate2)
medianImputed <- median(byDate2)

```
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily 
number of steps?


Next we examine whether there are differences in activity patterns between 
weekdays and weekends.

```{r}
weekday <- weekdays(as.Date(imputedActivity$date))
imputedActivity$type <- rep("weekday",17568)
imputedActivity$type[weekday %in% c("Saturday","Sunday")] <- "weekend"
weekends <- subset(imputedActivity,type == "weekend")
weekdays <- subset(imputedActivity,type == "weekday")

weekendInterval <- as.numeric(tapply(weekends$steps, weekends$interval, mean))
weekendInterval <- cbind(data.frame(as.numeric(rownames(table(activity$interval)))),weekendInterval)
colnames(weekendInterval) <- c("interval","steps")
weekendInterval$type <- rep("weekend",nrow(weekendInterval))

weekdayInterval <- as.numeric(tapply(weekdays$steps, weekdays$interval, mean))
weekdayInterval <- cbind(data.frame(as.numeric(rownames(table(activity$interval)))),weekdayInterval)
colnames(weekdayInterval) <- c("interval","steps")
weekdayInterval$type <- rep("weekday",nrow(weekdayInterval))

weeksplit <- rbind(weekendInterval,weekdayInterval)

require("lattice")

# xy plots by factor level
xyplot(steps~interval|type, data = weeksplit, type="l",
       main="Avg. #Steps Taken", 
       xlab="Interval", ylab="Number of Steps", 
       layout=c(1,2))
```
