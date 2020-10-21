<<<<<<< HEAD
fileURL <-
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
=======
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
>>>>>>> 35a9f948e5ddaf64e152493cd5ee61a79634f9f1
tempFile <- tempfile()
download.file(fileURL, tempFile)

AD_name <- "activity.csv"
data <- read.csv(unz(tempFile, AD_name))

unlink(tempFile)

<<<<<<< HEAD
stepsByDate <-
    unlist(lapply(split(data$steps, data$date), sum, na.rm = TRUE))
hist(stepsByDate,
     10,
     main = "Activity monitoring - Histogram",
     sub = "NA included",
     xlab = "Steps by day (total)")
=======
#library(dplyr)
stepsByDate <- unlist(lapply(split(data$steps, data$date), sum, na.rm=TRUE))
hist(stepsByDate, 10, main="Activity monitoring - Histogram", sub="NA included", xlab="Steps by day (total)")
>>>>>>> 35a9f948e5ddaf64e152493cd5ee61a79634f9f1
meanSteps <- mean(stepsByDate, na.rm = TRUE)
medianSteps <- median(stepsByDate, na.rm = TRUE)

stepsByInterval <-
    unlist(lapply(split(data$steps, data$interval), mean, na.rm = TRUE))
plot(
    stepsByInterval,
    type = 'l',
    main = "Average number of steps taken",
    sub = "Averaged across all days",
    xlab = "5-minute interval",
    ylab = "Steps"
)
max(stepsByInterval)

as.numeric(names(stepsByInterval[stepsByInterval == max(stepsByInterval)]))

sum(is.na(data$steps))

stepsByInterval <-
    data.frame(interval = names(stepsByInterval), steps = stepsByInterval)
data2 <- merge(data, stepsByInterval, by = "interval")
data2$steps.x <-
    ifelse(is.na(data2$steps.x), round(data2$steps.y), data2$steps.x)
data2 <-
    data.frame(
        steps = data2$steps.x,
        date = data2$date,
        interval = data2$interval
    )

stepsByDate2 <-
    unlist(lapply(split(data2$steps, data2$date), sum, na.rm = TRUE))
hist(stepsByDate2,
     10,
     main = "Activity monitoring - Histogram",
     sub = "NA substituted by estimated values",
     xlab = "Steps by day")
meanSteps2 <- mean(stepsByDate2, na.rm = TRUE)
medianSteps2 <- median(stepsByDate2, na.rm = TRUE)

meanSteps2 - meanSteps
medianSteps2 - medianSteps

comparison <-
    data.frame(
        'tics' = hist(stepsByDate, 10)$breaks[1:11],
        'with' = hist(stepsByDate, 10)$counts,
        'without' = h1 <- hist(stepsByDate2, 10)$counts
    )
graphic1 <- ggplot(comparison, aes(x = tics, y = with)) +
    geom_bar(stat = "identity", fill = 'blue') +
    geom_point(aes(y = without), colour = 'red') +
    labs(x = "Steps (mean value across interval)", y = "Frequency")
graphic1

sum(stepsByDate2) - sum(stepsByDate)

data2$dayType <-
    as.factor(ifelse(
        weekdays(as.Date(data2$date)) %in% c("sábado", "domingo"),
        "weekend",
        "weekday"
    ))

library(dplyr)
data2ByDayType <- data2[, c("steps", "interval", "dayType")] %>%
    group_by(dayType, interval) %>%
    mutate(steps = mean(steps)) %>%
    unique()

library(ggplot2)
graphic2 <- ggplot(data2ByDayType, aes(x = interval, y = steps)) +
    geom_line() +
    facet_wrap( ~ dayType,  ncol = 1, strip.position = "top") +
    labs(
        x = "Interval",
        y = "Average number of steps taken",
        title = "5-minute interval time series",
        subtitle = "Averaged across all days"
    )

<<<<<<< HEAD
graphic2
=======
data2ByDayType <- data2[,c("steps", "interval", "dayType")] %>%
    group_by(dayType, interval) %>%
    mutate(steps = mean(steps)) %>%
    unique()

#library(ggplot2)    
graphic <- ggplot(data2ByDayType, aes(x=interval, y=steps))+
    geom_line()+
    facet_wrap(~dayType,  ncol=1, strip.position = "top")+
    labs(x="Interval", y="Average number of steps taken", title="5-minute interval time series", subtitle = "Averaged across all days")

graphic
>>>>>>> 35a9f948e5ddaf64e152493cd5ee61a79634f9f1
