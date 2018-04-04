Loading and preprocessing the data
----------------------------------

    if(!file.exists("activity.csv")) {
             file <- unzip("activity.zip")
    }

    data <- read.csv("activity.csv")

What is mean total number of steps taken per day?
-------------------------------------------------

    stepsPerDay <- tapply(data$steps, data$date, sum, na.rm = TRUE)

#### Histogram of the total number of steps taken each day

    library(ggplot2)
    qplot(stepsPerDay, xlab = "Total Steps", ylab = "Frequency", binwidth = 500)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

#### Mean and median number of steps taken each day

    meanStepsPerDay <- mean(stepsPerDay)
    medianStepsPerDay <- median(stepsPerDay)

-   Mean: 9354.2295
-   Median: 10395

------------------------------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

    average <- aggregate(x=list(steps=data$steps),by=list(interval=data$interval), FUN = mean, na.rm = TRUE)

#### Time series plot of the average number of steps taken

    ggplot(data = average, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("Interval") +
    ylab("Average number of steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

#### The 5-minute interval that, on average, contains the maximum number of steps

    average[which.max(average$steps),]

    ##     interval    steps
    ## 104      835 206.1698

-   Most Steps at: 8:35

------------------------------------------------------------------------

Imputing missing values
-----------------------

Many days/intervals contain missing values. To impute missing values we
first calculate the total number of missing values.

    numMissingValues <- length(which(is.na(data$steps)))

-   Number of missing values: 2304

We then create a new dataset and fill in the missing values

    activityDataImputed <- data
    activityDataImputed$steps <- sapply(data$steps, mean)

    stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)

#### Histogram of the total number of steps taken each day after missing values are imputed

    qplot(stepsByDayImputed, xlab='Total steps per day', ylab='Frequency', binwidth=500)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

#### The mean and median total number of steps taken per day

    stepsByDayMeanImputed <- mean(stepsByDayImputed, na.rm = TRUE)
    stepsByDayMedianImputed <- median(stepsByDayImputed)

-   Mean (Imputed): 1.0766 × 10<sup>4</sup>
-   Median (Imputed): 1.0766 × 10<sup>4</sup>

------------------------------------------------------------------------

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

At first we create a new factor variable in the dataset with two levels
- "weekday" and "weekend" indicating whether a given date is a weekday
or weekend day.

    day <- function(date) {
      day <- weekdays(date)
      if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
      else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
      else
        stop("invalid date")
    }
    data$date <- as.Date(data$date)
    data$day <- sapply(data$date, FUN=day)

#### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

    averages <- aggregate(steps ~ interval + day, data=data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +xlab("5-minute interval") + ylab("Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)
