# Loading and processing the data
unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))

# clean the data by excluding the missing values
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
dim(noNA)
totalsteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
mean(totalsteps)

# calculate the total number of steps taken per day
StepsByday <- tapply(data$steps, data$date, sum, na.rm = TRUE)

# make a histogrem of the total number of steps taken each day
ggplot(data, aes(date, steps)) + geom_bar(stat = "identity",
                                          colour = "steelblue",
                                          fill = "steelblue") +
        facet_grid(.~month, scales = "free") +
        labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total Number of Steps")

# what is mean and median of total number of steps taken per day
mean(StepsByday)
median(StepsByday)

# what is the average daily activity pattern
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
AvgSteps <- aggregate(data$steps, 
                      list(interval = as.numeric(as.character(data$interval))),
                      FUN = "mean", 
                      na.rm = TRUE)
names(AvgSteps)[2] <- "AvgSteps"
ggplot(AvgSteps, aes(interval, AvgSteps)) +
        geom_line(color = "steelblue", size = 1.0) +
        labs(title = "Time Series Plot of 5mins Interval", x = "Interval", y = "Average Number of Steps")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
AvgSteps[AvgSteps$AvgSteps == max(AvgSteps$AvgSteps),]

# Imputing the missing values
# check total number of rows with NAs
sum(is.na(data))
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# fill in missing data by the mean for that 5 mins interval
newdata <- data
for (i in 1:nrow(newdata)) {
        if (is.na(newdata$steps[i])) {
                newdata$steps[i] <- AvgSteps[which(newdata$interval[i] == AvgSteps$interval), ]$AvgSteps
        }
}
head(newdata)
sum(is.na(newdata))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
ggplot(newdata, aes(date, steps)) + 
        geom_bar(stat = "identity",
                 color = "steelblue",
                 fill = "steelblue",
                 width = 0.8) +
        facet_grid(.~month, scales = "free") +
        labs(title = "Histogram of Total Number of Steps by Date", x = "Date", y = "Total Number of Steps")

# mean and median of steps
NewTotalSteps <- tapply(newdata$steps, newdata$date, sum, na.rm = TRUE)
mean(NewTotalSteps)
median(NewTotalSteps)
# compare with old mean and median
mean(NewTotalSteps) - mean(StepsByday)
median(NewTotalSteps) - median(StepsByday)


NewTotalSteps1 <- aggregate(newdata$steps, list(Date = newdata$date), FUN = "sum")$x

# Are there differences in activity patterns between weekdays and weekends?

mean(NewTotalSteps) - mean(StepsByday)
median(NewTotalSteps) - median(StepsByday)

# Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
newdata$daytype <- factor(format(newdata$date, "%A"))
levels(newdata$daytype) <- list(weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                weekend = c("Saturday", "Sunday"))
levels(newdata$daytype)
table(newdata$daytype)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
AvgStepsDaytype <- aggregate(newdata$steps, 
                             list(interval = as.numeric(as.character(newdata$interval)),
                                  daytype = newdata$daytype),
                                FUN = "mean")
names(AvgStepsDaytype)[3] <- "MeanSteps"
head(AvgStepsDaytype)
library(lattice)
xyplot(AvgStepsDaytype$MeanSteps ~ AvgStepsDaytype$interval | AvgStepsDaytype$daytype,
       layout = c(1,2), type = "l",
       xlab = "Interval", ylab = "Number of Steps")












