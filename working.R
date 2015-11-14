

if (!file.exists("activity.csv")) {
  unzip ("activity.zip", exdir = "./")


}
data <-
  read.csv(
    "activity.csv", header = TRUE,sep = ",", na.strings = "NA", stringsAsFactors = FALSE
  )
data$date <- as.Date(data$date, "%Y-%m-%d")

#SUM
#Calculate the total number of steps taken per day
#

#What is mean total number of steps taken per day?
library(dplyr)
stepsPreDay <- data %>%
  group_by(date) %>%
  summarise(
    total = sum(steps), meanVal = mean(steps,rm.na = TRUE), medianVal =  median(steps)
  )

#Make a histogram of the total number of steps taken each day
countUni = length(unique(stepsPreDay$total,incomparables = FALSE))
countUni = (countUni - 1)
hist(
  stepsPreDay$total,
  main = "Histogram: Steps Per Day \n(NA's included)",
  xlab = "Steps Per Day",
  las = 1,col = "orange",
  border = "orange",
  breaks = 8,
  xlim = c(0,25000),
  ylim = c(0,25)
)
#png("figures/)
# Calculate and report the mean and median of the total number of steps taken per day

meanTotalStepPerDay <- mean(stepsPreDay$total,na.rm = TRUE)
medianTotalStepsPerDay <- median(stepsPreDay$total,na.rm = TRUE)
sumTotalStepsPerDay <- sum(stepsPreDay$total,na.rm = TRUE)

print("Sum Total Steps per Day")
print(sumTotalStepsPerDay)
print("Mean Total Steps Per Day")
print(meanTotalStepPerDay)
print("Median Total Steps per Day")
print(medianTotalStepsPerDay)

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
aggdata <- aggregate(steps ~ interval, data, mean)
plot(
  aggdata,
  type = 'l',
  main = "Time Series: Steps by Interval",
  xlab = "Intervals - 5 Minute",
  ylab = "Average Number of Steps",
  las = 1,
  xlim = c(0,2500)
)

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
print("Interval with maximum average number of steps")
print(aggdata[which.max(aggdata$steps),])



#Calculate and report the total number of missing values in the dataset
print("Total Number of Missing Values (NA)")
print(sum(is.na(data)))

##Imputing missing values
# Devise a strategy for filling in all of the missing values in the dataset
# impute NA to mean Interval Value (from aggdata)
# Create a new dataset that is equal to the original dataset but with the missing data filled in.

# This is sort of slow, but I couldn't figure out a fancier way, Sorry.
if (!exists("dataImp")) {
  dataImp <- data
  ct <- nrow(dataImp)
  x = 0
  for (i in 1:ct) {
    if (is.na(dataImp$steps[i])) {
      t = filter(aggdata, dataImp$interval[i] == aggdata$interval)
      tt = t[2]
      dataImp$stepsImp[i] = as.numeric(tt)
    }
    else
      dataImp$stepsImp[i] =  as.numeric(dataImp$steps[i])
  }
  #drop the unimputed steps col, juggle and rename to match the originaal data set.
  dataImp <- subset(dataImp, select = -c(steps))
  dataImp <- dataImp[,c(3,1,2)]
  names(dataImp)[names(dataImp) == 'stepsImp'] <- 'steps'
}

# Make a histogram of the total number of steps taken each day
# and Calculate and report the mean and median total number of steps taken per day.

stepsPreDayImp <- dataImp %>%
  group_by(date) %>%
  summarise(
    total = sum(steps),
    meanVal = mean(steps,rm.na = TRUE),
    medianVal =  median(steps)
  )

countUniImp = length(unique(stepsPreDay$total,incomparables = FALSE))
countUniImp = (countUniImp  - 1)
hist(
  stepsPreDayImp$total,
  main = "Histogram: Steps Per Day \n(imputed missing values)",
  xlab = "Steps Per Day",
  las = 1,
  col = "blue",
  border = "blue",
  breaks = 8,
  xlim = c(0,25000),
  ylim = c(0,25)
)

meanTotalStepPerDayImp <- mean(stepsPreDayImp$total,na.rm = TRUE)
medianTotalStepsPerDayImp <- median(stepsPreDayImp$total,na.rm = TRUE)
sumTotalStepsPerDayImp <- sum(stepsPreDayImp$total,na.rm = TRUE)

print("Sum Total Steps Per Day (imputed missin values)")
print(sumTotalStepsPerDayImp)
print("Mean Total Steps Per Day (imputed missin values)")
print(meanTotalStepPerDayImp)
print("Median Total Steps per Day (imputed missin values)")
print(medianTotalStepsPerDayImp)

##Do these values differ from the estimates from the first part of the assignment?
##yes
##What is the impact of imputing missing data on the estimates of the total daily number of steps?
# imputing missing values increases the sum total daily number of steps
# because NA, having no numeric value are replaced with the imputed value
# (mean value of steps calculated for each interval among overseravation having reported values)
# 656,737.5 - 570,608 = 86,129.5 : %15.09 increase

#Are there differences in activity patterns between weekdays and week- ends?
weekdayT <- weekdays(dataImp$date,abbreviate = FALSE)
weekdayList <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
dataImp$dayType <-
  as.factor(ifelse(weekdayT %in% weekdayList,"weekday","weekend"))


meanStepsPerDayByIntervalImp <- dataImp %>%
  group_by(dayType,interval) %>%
  summarise(meanStepVal = mean(steps,rm.na = TRUE))

library(lattice)
library(RColorBrewer)
colorsTheme <- brewer.pal(6,"Blues")

my.theme <- list(
  superpose.polygon = list(col = colorsTheme[2:5], border = "transparent"),
  strip.background = list(col = colorsTheme[6]),
  strip.border = list(col = colorsTheme[6])
)
plotd <-
  xyplot(
    meanStepsPerDayByIntervalImp$meanStepVal ~ meanStepsPerDayByIntervalImp$interval |
      meanStepsPerDayByIntervalImp$dayType,
    type = "l",
    main = "Average Number of Steps By Interval\n(Weekend vs Weekdays)",
    ylab = "Number of Steps",
    xlab = "Interval",
    par.settings = my.theme,
    par.strip.text = list(col = "white", font = 2),
    layout = (c(1,2))
  )
png("figures/MyTestFigure.png")
print(plotd)
dev.off()

#Are there differences in activity patterns between weekdays and week- ends?
# Yes, on weekends people are more active in general, espacally in the interal between 1000 and 2000.
# Weekend activity is more consistent throughtout the day.
# On both weekends and weekdays there is a peek in  activity between intervals 750 and 1000.
# The peek is more pronounced on weekdays - perhaps as people are going to work/shcool?
# People are less active on weekdays in the interal between 1000 and 2000 - perhaps because they are sitting still at work/shcool?
# Activity on weekdays is greater in the interal between 500 and 750 - a time perion of appox 2 hours, sugesting that people wake up much earlier on weekday.

