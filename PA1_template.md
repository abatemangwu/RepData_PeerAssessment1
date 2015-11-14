# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
```r

```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
#Are there differences in activity patterns between weekdays and week- ends?

```r
library(knitr)
library(dplyr)
echo = TRUE
weekdayT <- weekdays(dataImp$date,abbreviate = FALSE)
weekdayList <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
dataImp$dayType <-
  as.factor(ifelse(weekdayT %in% weekdayList,"weekday","weekend"))


p <- meanStepsPerDayByIntervalImp <- dataImp %>%
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

print(plotd)

```
#Are there differences in activity patterns between weekdays and week- ends?
# Yes, on weekends people are more active in general, espacally in the interal between 1000 and 2000.
# Weekend activity is more consistent throughtout the day.
# On both weekends and weekdays there is a peek in  activity between intervals 750 and 1000.
# The peek is more pronounced on weekdays - perhaps as people are going to work/shcool?
# People are less active on weekdays in the interal between 1000 and 2000 - perhaps because they are sitting still at work/shcool?
# Activity on weekdays is greater in the interal between 500 and 750 - a time perion of appox 2 hours, sugesting that people wake up much earlier on weekday.


