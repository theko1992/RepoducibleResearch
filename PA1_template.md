
 +# Reproducible Research: Peer Assessment 1
 +
 +
 +
 +
 +## Loading and preprocessing the data
 +
 +```r
 +unzip(zipfile="~./ReproRes/Week2/RepData_PeerAssessment1/activity.zip", exdir = "~./ReproRes/Week2/RepData_PeerAssessment1")
 +
 +data <- read.csv("~./ReproRes/Week2/RepData_PeerAssessment1/activity.csv")
 +```
 +
 +
 +## What is mean total number of steps taken per day?
 +
 +```r
 +total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
 +mean.steps <- mean(total.steps)
 +median.steps <- median(total.steps)
 +
 +par(mfrow=c(1,2))
 +  boxplot(total.steps, main="Boxplot", ylab="Total number taken each day", xlab="Steps", col="green", notch = TRUE)
 +  text(1,median.steps, "The median", col = "blaCK", adj = c(0, -.5))
 +
 +    hist(total.steps, main="Histogram", ylab="Frequncy of occurrence", xlab="Total number of steps taken each day", col="red")
 +  abline(v = mean.steps, col = "black", lwd = 2)
 +  text(mean.steps-1500,25, "The mean", col = "blaCK")
 +```
 +
 +![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
 +
 +```r
 +mean.steps
 +```
 +
 +```
 +## [1] 9354.23
 +```
 +
 +```r
 +median.steps
 +```
 +
 +```
 +## [1] 10395
 +```
 +
 +
 +## What is the average daily activity pattern?
 +
 +```r
 +library(ggplot2)
 +```
 +
 +```
 +## Warning: package 'ggplot2' was built under R version 3.3.1
 +```
 +
 +```r
 +mean.dap <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
 +Max.steps <- mean.dap[which.max(mean.dap$steps),]
 +
 +Mdap <- ggplot(data=mean.dap, aes(x=interval, y=steps))
 +  Mdap +  geom_line(colour = "blue", size = 2) + xlab("Intervals (5-minute each)") + ylab("Average number of steps taken") + ggtitle("Daily activity pattern")
 +```
 +
 +![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
 +
 +```r
 +Max.steps
 +```
 +
 +```
 +##     interval    steps
 +## 104      835 206.1698
 +```
 +
 +
 +## Imputing missing values
  +
 +```r
 +missing <- is.na(data$steps)
 +```
 +
 +
 +# How many missing
 +
 +```r
 +table(missing)
 +```
 +
 +```
 +## missing
 +## FALSE  TRUE 
 +## 15264  2304
 +```
 +
 +
 +# Replace each missing value with the mean value of its 5-minute interval 
 +
 +```r
 +mean.dap <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
 +
 +  impute <- function(steps, interval) {
 +                impu <- NA
 +        if (!is.na(steps))
 +                impu <- c(steps)
 +        else
 +                impu <- (mean.dap[mean.dap$interval == interval, "steps"])
 +        return(impu)
 +  }
 +
 +
 +imp.data <- data
 +imp.data$steps <- mapply(impute, imp.data$steps, imp.data$interval)
 +imp.total.steps <- tapply(imp.data$steps, imp.data$date, FUN=sum)
 +imp.mean.steps <- mean(imp.total.steps)
 +imp.median.steps <- median(imp.total.steps)
 +
 +imp.mean.steps
 +```
 +
 +```
 +## [1] 10766.19
 +```
 +
 +```r
 +imp.median.steps
 +```
 +
 +```
 +## [1] 10766.19
 +```
 +
 +
 +# Summary of the numerical difference between original data and imputed data
 +
 +```r
 +summary(data)
 +```
 +
 +```
 +##      steps                date          interval     
 +##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
 +##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
 +##  Median :  0.00   2012-10-03:  288   Median :1177.5  
 +##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
 +##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
 +##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
 +##  NA's   :2304     (Other)   :15840
 +```
 +
 +```r
 +summary(imp.data)
 +```
 +
 +```
 +##      steps                date          interval     
 +##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
 +##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
 +##  Median :  0.00   2012-10-03:  288   Median :1177.5  
 +##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
 +##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
 +##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
 +##                   (Other)   :15840
 +```
 +
 +
 +# Visualisation of the original data
 +
 +```r
 +par(mfrow=c(1,2))
 +
 +  boxplot(total.steps, main="Original data: Boxplot", ylab="Total number taken each day", xlab="Steps", col="green", notch = TRUE)
 +  text(1,median.steps, "The median", col = "blaCK", adj = c(0, -.5))
 +
 +  hist(total.steps, main="Original data: Histogram", ylab="Frequncy of occurrence", xlab="Total number of steps taken each day", col="red")
 +  abline(v = mean.steps, col = "black", lwd = 2)
 +  text(mean.steps-1500,25, "The mean", col = "blaCK")
 +```
 +
 +![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
 +
 +
 +# Visualisation of the imputed data
 +
 +```r
 +par(mfrow=c(1,2))
 +
 +  boxplot(imp.total.steps, main="Imputed data: Boxplot", ylab="Total number taken each day", xlab="Steps", col="purple", notch = TRUE)
 +  text(1,imp.median.steps, "The median", col = "blaCK", adj = c(0, -.5))
 +
 +  hist(imp.total.steps, main="Imputed data: Histogram", ylab="Frequncy of occurrence", xlab="Total number of steps taken each day", col="orange")
 +  abline(v = imp.mean.steps, col = "black", lwd = 2)
 +  text(imp.mean.steps-4000,25, "The mean", col = "blaCK")
 +```
 +
 +(PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
 +
 +
 +## Are there differences in activity patterns between weekdays and weekends?
 +
 +```r
 +  whatday <- function(date) {
 +        day <- weekdays(date)
 +        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
 +                return("weekday")
 +        else if (day %in% c("Saturday", "Sunday"))
 +                return("weekend")
 +        else
 +                stop("invalid date")
 +  }
 +
 +imp.data$date <- as.Date(imp.data$date)
 +imp.data$day <- sapply(imp.data$date, FUN = whatday)
 +
 +
 +av.dap <- aggregate(steps ~ interval + day, data = imp.data, mean)
 +Adap <- ggplot(av.dap, aes(interval, steps)) 
 +Adap + geom_line(colour = "blue", size = 2) + facet_grid(day ~ .) + xlab("Intervals (5-minute each)") + ylab("Average number of steps taken") + ggtitle("Daily activity pattern")
 +```
 +
 +![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
 +
