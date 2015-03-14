---
title: "PeerAssesment1"
output: html_document
---
UnzipFile

```r
setwd("C:/Users/538321/Documents/DataManagement/ReproducibleResearch/assignment1")
downLoadTo <- getwd()
downLoadTo <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", downLoadTo)
unzip(downLoadTo)
unlink(downLoadTo)
```
The Data has been downloaded to local directory. Read The File

```r
fileToRead <- read.csv("C:/Users/538321/Documents/DataManagement/ReproducibleResearch/assignment1/activity.csv")
```
What is the mean total number of steps taken per day?
Sum Steps perday, create histogram and claculate mean and median

```r
total.steps <- aggregate(steps~date,fileToRead, FUN = sum)
barplot(total.steps$steps,  names.arg = total.steps$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
Mean ad Median

```r
mean(total.steps$steps)
```

```
## [1] 10766.19
```

```r
median(total.steps$steps)
```

```
## [1] 10765
```
What is the daily average activity pattern
*Calculate the average steps for each interval for all days
*Plot the average number steps per day by interval
*Fine the interval with most average steps

```r
stepsByInterval <- aggregate(steps~interval, fileToRead, FUN = mean)

plot(stepsByInterval, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
stepsByInterval$interval[which.max(stepsByInterval$steps)]
```

```
## [1] 835
```
Impute Missing Values
1.Calulate and report the total number of missing values in the dataset

```r
sum(is.na(fileToRead))
```

```
## [1] 2304
```
Devise a stategy for filling in all missing values in the dataset.The strategy need ot be sophisticated. It can be mean/median for that day or mean for that 5 minute interval
1.Calculate the number of rows in dataset
2.If in the steps column the data in any row is NA then calculate the interval value
3.Populate the row for steps coulumn that has NA with values of steps from the data set that holds the average steps in stepsByInterval for that interval



```r
numOriginal <- nrow(fileToRead)
for( i in  1:numOriginal){
   
   
    if( is.na(fileToRead[i,]$steps)  ){
      iVal <- fileToRead[i,]$interval
      fileToRead[i,1] <- stepsByInterval[stepsByInterval$interval == iVal, "steps"]

    }else{
      fileToRead[i,1] <- fileToRead[i,]$steps
    }
   
    
  }
```

Make Histogram of the total number of steps taken each day and calculate and report the median and mean total number of steps taken perday. Do these values differ from weekdays versus weekends.


```r
totalStepsEachDay <- aggregate(steps~date, data = fileToRead, FUN = sum)
barplot(totalStepsEachDay$steps, names.arg = totalStepsEachDay$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(totalStepsEachDay$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsEachDay$steps)
```

```
## [1] 10766.19
```

```r
filledData <- fileToRead

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

filledData$date <- as.Date(filledData$date)
filledData$day <- sapply(filledData$date, FUN=weekday.or.weekend)
averages <- aggregate(steps ~ interval + day, data=filledData, mean)
library(ggplot2)

ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png) 



