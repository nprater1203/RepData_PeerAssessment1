---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## 1. Loading and preprocessing the data

```r
library(knitr)
#Read the data and organize it to be used later
data <- read.csv("./activity/activity.csv")

data[["date"]] <- as.Date(data[["date"]])
data[["steps"]] <- as.numeric(as.character(data[["steps"]]))
```
## 2. Histogram of the total number of steps taken each day

```r
#Create a data frame with the total steps per day
totalStepsDF <- data.frame(
  date <- unique(data[["date"]]),
  totalSteps <- tapply(data[["steps"]], data[["date"]], sum) 
)

colnames(totalStepsDF) <- c("Date","Total Steps")
totalStepsDF <- na.omit(totalStepsDF)

hist(totalStepsDF[["Total Steps"]], main = "Total Number of Steps Taken",
     xlab = "Steps",
     col = "blue",
     border = "black",)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## 3. Mean and median number of steps taken each day

```r
meanSteps = mean(totalStepsDF[["Total Steps"]], na.rm = TRUE)

cat("Mean number of steps = ", meanSteps)
```

```
## Mean number of steps =  10766.19
```

```r
medianSteps = median(totalStepsDF[["Total Steps"]])
cat("Median number of steps = ", medianSteps)
```

```
## Median number of steps =  10765
```

## 4. Time series plot of the average number of steps taken?

```r
#Create a data frame with the mean number of steps per day
meanStepsDF <- data.frame(
  date <- unique(data[["date"]]),
  meanSteps <- tapply(data[["steps"]], data[["date"]], mean) 
)

colnames(meanStepsDF) <- c("Date","Average Steps")
meanStepsDF <- na.omit(meanStepsDF)


plot(meanStepsDF[["Date"]], meanStepsDF[["Average Steps"]], type = "l",
     col = "blue",
     lwd = 2,
     main = "Time Series Plot of Average Steps per Day",
     xlab = "Date",
     ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## 5. Code to describe and show a strategy for imputing missing data


```r
#Create a list of the rows with missing data
rowsMissing <- which(!complete.cases(data))
missingDF <- data

#Replace the missing data with the mean number of steps for an interval
for(i in rowsMissing){
  missingDF[i, 1] <- round(mean(data[["steps"]], na.rm = TRUE))
}

totalStepsMissingDF <- data.frame(
  date <- unique(missingDF[["date"]]),
  totalSteps <- tapply(missingDF[["steps"]], missingDF[["date"]], sum) 
)

colnames(totalStepsMissingDF) <- c("Date","Total Steps")
```

## 6. Histogram of the total number of steps taken each day after missing values are inputed


```r
hist(totalStepsMissingDF[["Total Steps"]], 
    main = "Missing Data Number of Steps",
     xlab = "Steps",
     col = "blue",
     border = "black",)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## 7. Are there differences in activity patterns between weekdays and weekends?


```r
totalStepsDF <- data.frame(
  date <- unique(data[["date"]]),
  totalSteps <- tapply(data[["steps"]], data[["date"]], sum) 
)

colnames(totalStepsDF) <- c("Date","Total Steps")
totalStepsDF <- na.omit(totalStepsDF)

#Look to see if each date is a weekend or weekday and add create a column with
#its designation
totalStepsDF[["Day"]] <- ifelse(weekdays(totalStepsDF[["Date"]]) %in% 
                          c("Saturday","Sunday"), "Weekend", "Weekday")

totalStepsMissingDF[["Day"]] <- ifelse(weekdays(totalStepsMissingDF[["Date"]]) %in% 
                                  c("Saturday","Sunday"), "Weekend", "Weekday")

#Creates dataframes for the two sperate ones witht the total steps for weekdays 
#and weekends
nonMissingDF <- data.frame(
  day = unique(totalStepsDF[["Day"]]),
  steps = c(sum(subset(totalStepsDF[["Total Steps"]], totalStepsDF[["Day"]] == "Weekday")),
            sum(subset(totalStepsDF[["Total Steps"]], totalStepsDF[["Day"]] == "Weekend")))
)

colnames(nonMissingDF) <- c("Day", "Steps")

replaceMissingDF <- data.frame(
  day = unique(totalStepsDF[["Day"]]),
  steps = c(sum(subset(totalStepsMissingDF[["Total Steps"]], totalStepsMissingDF[["Day"]] == "Weekday")),
            sum(subset(totalStepsMissingDF[["Total Steps"]], totalStepsMissingDF[["Day"]] == "Weekend")))
)

colnames(replaceMissingDF) <- c("Day", "Steps")

par(mfrow = c(1,2))

barplot(nonMissingDF[["Steps"]] / 1e5, names.arg = nonMissingDF[["Day"]],
        main = "Non Added Values Bar Graph",
        col = "blue",
        ylab = "Steps (Hundred Thousands)"
)

barplot(replaceMissingDF[["Steps"]] / 1e5, names.arg = replaceMissingDF[["Day"]],
        main = "Added Values Bar Graph",
        col = "red",
        ylab = "Steps (Hundred Thousands)"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
