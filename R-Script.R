## Loading Essential Packages
library(dplyr)
library(ggplot2)

## Loading the data 
activity = read.csv("activity.csv")

## General Characteristics of the dataset
names(activity)
dim(activity)

## Count of the missing values
lapply(activity, function(x) sum(is.na(x)))

## Imputing the missing Values
## Only 'steps' has missing values. Hence, replace them by the mean and store 
## the analytic data in a new dataset - 'activity_ana'
activity_ana <- activity
activity_ana$steps[is.na(activity_ana$steps)] <- 
  mean(activity_ana$steps, na.rm = TRUE)

## Verifying replacement of NA values
lapply(activity_ana, function(x) sum(is.na(x)))         

## Summary statistics
summary(activity_ana)

## Grouping mean, median and total steps by day
steps_details <- activity_ana %>%
  group_by(date) %>%
  summarise(steps_per_day = sum(steps), median_per_day = median(steps),
            mean_per_day = mean(steps))
View(steps_details)

## Histogram of total steps per day
ggplot(mapping = aes(x = steps_details$steps_per_day))+
  geom_histogram()+
  
  theme(axis.text.x = element_blank(), axis.title.x = element_text("Days"), 
        axis.title.y = element_text("Total Steps"), 
        plot.title = element_text("Histogram of Total Steps per Day"))

## Bar Plot of total steps per day
ggplot(steps_details, mapping = aes(x = date, y = steps_per_day))+
  geom_bar(stat = "identity")+
  
  theme(axis.text.x = element_blank(), axis.title.x = element_text("Days"), 
        axis.title.y = element_text("Total Steps"), 
        plot.title = element_text("Barplot of Total Steps per Day"))

## Average steps during each time interval
# Grouping the variables on the basis of the interval
interval_details <- activity_ana %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps))

# Time series plot
ggplot(interval_details, mapping = aes(x=interval, y=mean_steps))+
  geom_line()+
  theme(axis.title.y = element_text("Steps Averaged over all days"), 
        plot.title = element_text("Time Series Plot of average steps averaged over each 
        interval"))

# Calculating the interval during which mean number of steps is the highest
interval_details$interval[which.max(interval_details$mean_steps)]

## Are there differences in activity patterns between weekdays and weekends?
# Creating the factor variable 'day_type'
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity_ana$day_type <- 
  factor((weekdays(as.Date(activity_ana$date)) %in% weekdays1), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

# Boxplot to compare the steps during weekdays and weekends
ggplot(activity_ana, mapping = aes(x = day_type, y = log10(steps)))+
    geom_boxplot() +
    theme(axis.title.x = element_blank())

    