setwd("~/GitHub/Reproducible_1")
activity <- read.csv("activity.csv")

byDate <- tapply(activity$steps[!is.na(activity$steps)],activity$date[!is.na(activity$steps)],sum)
byDate <- byDate[!is.na(byDate)]

#Make a histogram of the total number of steps taken each day
hist(byDate,main = "Frequency of Steps Each Day", xlab = "#Steps")

#Calculate and report the mean and median total number of steps taken per day
rawMean <- mean(byDate)
rawMedian <- median(byDate)

#rownames(table(activity$interval))

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
byInterval <- as.numeric(tapply(activity$steps[!is.na(activity$steps)], activity$interval[!is.na(activity$steps)], mean))
intervals <- data.frame(as.numeric(rownames(table(activity$interval))))
intervals <- cbind(intervals,byInterval)
colnames(intervals) <- c("interval","steps")
par(mfrow = c(1,1))
plot(intervals$interval,intervals$steps, type = "l", xlab = "5-minute Interval", ylab = "Steps", main = "Average Number of Steps Taken")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervals[which(intervals$steps==max(intervals$steps)),]

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
TotalNAs <- length(activity$steps[!complete.cases(activity)])

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
require(mice)
imputedActivity <- complete(mice(activity))

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
byDate2 <- tapply(imputedActivity$steps,imputedActivity$date,sum)

#Make a histogram of the total number of steps taken each day
hist(byDate2,main = "Frequency of Steps Each Day (NAs imputed)", xlab = "#Steps")
meanImputed <- mean(byDate2)
medianImputed <- median(byDate2)

#Are there differences in activity patterns between weekdays and weekends?
weekday <- weekdays(as.Date(imputedActivity$date))
imputedActivity$weekday <- rep("weekday",17568)
imputedActivity$weekday[weekday %in% c("Saturday","Sunday")] <- "weekend"
weekends <- subset(imputedActivity,weekday == "weekend")
weekdays <- subset(imputedActivity,weekday == "weekday")

weekendInterval <- as.numeric(tapply(weekends$steps, weekends$interval, mean))
weekendInterval <- cbind(data.frame(as.numeric(rownames(table(activity$interval)))),weekendInterval)
colnames(weekendInterval) <- c("interval","steps")
<<<<<<< HEAD
weekendInterval$type <- "weekend"
=======
  >>>>>>> 35611a80f1cfc30aad50c9d6b83bbd2c3acd6f48

weekdayInterval <- as.numeric(tapply(weekdays$steps, weekdays$interval, mean))
weekdayInterval <- cbind(data.frame(as.numeric(rownames(table(activity$interval)))),weekdayInterval)
colnames(weekdayInterval) <- c("interval","steps")
<<<<<<< HEAD
weekdayInterval$type <- "weekday"

weeksplit <- rbind(weekendInterval,weekdayInterval)

require("lattice")

# xy plots by factor level
xyplot(steps~interval|type, data = weeksplit, type="l",
       main="Avg. #Steps Taken", 
       xlab="Interval", ylab="Number of Steps", 
       layout=c(1,2))
