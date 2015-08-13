Peer Assesment 1
=================

This is the R markdown file for Peer Assesment 1 in Reproducible Resarch module of the Data Science track.
You can find more information about this in the Readme.md file.

We will delve straight into the analysis.

We will be using a number of packages so lets load them outright.


```r
require(lubridate)
require(sqldf)
require(ggplot2)
```

Let's also turn off the scientific notation

```r
options(scipen=999)
```
#Loading and preprocessing the data.
Unzip and read the file


```r
dt <- read.csv(unzip("activity.zip","activity.csv"));
```

Let's look at the structure of the data.


```r
str(dt)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

#What is the mean total number of steps taken per day?

Let's first remove the missing values

```r
cc <- dt[complete.cases(dt),]
```

Now we are asked to make a histogram of the **total** number of steps taken **each day**.

Let's aggregate the total number of steps taken each day.

```r
st <- aggregate(steps ~ date,cc,FUN=sum)
```

1. Now let's plot the histogram using qplot

```r
qplot(steps,data=st,geom="histogram")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

We are asked to calculate the mean and median number of steps taken each day.

```r
mean_ <- mean(st$steps)
median_ <- median(st$steps)
```

2.
The mean number of steps is : ***10766.1886792***  
The median number of steps is : ***10765***

#What is the average daily activity pattern?

Lets first make an aggregate of the average for each interval.


```r
iAvg <- aggregate(steps ~ interval,cc,FUN=mean)
```

1. Now, let's plot the line graph of steps for each 5 min interval in the day.

```r
qplot(interval,steps,data=iAvg,geom="line")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Let's get the interval of which our aggregated mean is its maximum

```r
maxInt_ <- iAvg[which.max(iAvg$steps),1]
```


2. The interval of the maximum average number of steps is : ***835***

#Imputing missing values

Let's first caluculate how many missing values are present in our original data set. (Stored in the dt variable)

```r
mis_ <- sum(is.na(dt$steps))
```

1. The total number of missing values in the data set is : ***2304***  

2. We need to fill these missing data. Let's use the interval mean (from hour iAvg) to fill in these missing values


```r
dtF <- dt #copy the original dataframe
misInt <- as.data.frame(dt[is.na(dt$steps),3]) #Store the missing intervals
colnames(misInt)[1] <- "interval" #rename the colname

for(i in misInt$interval){ #iterate through each missing interval and fill the data frame with the respective average from iAvg.
      dtF[dtF$interval == i & is.na(dtF$steps),1] <- subset(iAvg,interval %in% i,steps)
}
```

3. Let's double check that all the missing items have been filled.

```r
sum(!complete.cases(dtF))
```

```
## [1] 0
```

Now let's aggregate the total steps by date then get the mean and median.

```r
stF <- aggregate(steps ~ date,dtF,FUN=sum)
meanF <- mean(stF$steps)
medianF <- median(stF$steps)
```

4. Plot the histogram and report the values.

```r
qplot(steps,data=stF,geom="histogram")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

Mean steps of the filled data frame is : ***10766.1886792***  
Median steps of the filled data frame is : ***10766.1886792***

#Are there differences in activity patterns between weekdays and weekends?

Let's first convert our dates in our freshly filled data frame (dtF) into date format.

```r
dtF$date <- as.Date(dtF$date,"%Y-%m-%d")
```

Now add the day of the week to the dataframe

```r
dtF$day <- weekdays(dtF$date)
```

Replace saturday and sunday with weekend

```r
dtF[dtF$day %in% c('Saturday','Sunday'),4] <- "weekend"
```

Replace the monday to friday with weekday

```r
dtF[(dtF$day %in% c('Monday','Tuesday','Wednesday','Thursday','Friday')),4] <- "weekday"
```

1. Factorise the day

```r
dtF$day <- as.factor(dtF$day)
```

Aggregate the average steps with respect to the time interval for both weekday and weekend

```r
iAggr <- aggregate(steps ~ interval + day,data=dtF,FUN=mean)
```


2. Make the panel plot of the step behaviour with respect to the interval

```r
qplot(interval,steps,data=iAggr,geom="line",facets=.~day)
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png) 










