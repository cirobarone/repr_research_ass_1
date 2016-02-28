---
title: "Project Assignment 1 - Reproducible Research"
author: "Ciro Barone"
date: "28 febbraio 2016"
output: pdf_document
---
# Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

# Loading and preprocessing the data

Show any code that is needed to:

- Load the data (i.e. read.csv())
- Process/transform the data (if necessary) into a format suitable for your analysis

### Load the data 
```{r}
activity <- read.csv("D:/Users/cibarone/Progetti_Capgemini/reproducible_research_ass_1/activity.csv",header = TRUE, sep = ",",na.strings = "NA", colClasses = c("numeric","character","numeric"))
```

```{r}
library(dplyr)
library(ggplot2)
```


### Process/transform the data

```{r, echo=FALSE}
str(activity)
head(activity)

```

### Treat of Missing Value

```{r}
table(is.na(activity$steps))
table(is.na(activity$date))
table(is.na(activity$interval))

```

### Subsetting Data set

With the following code I exclude drastically all NA value into the Steps Column.

```{r}
activity_clean <- filter(activity,is.na(activity$steps)==FALSE)
activity_clean <-  droplevels.data.frame(activity_clean) 
```

Then the dataset looks like that:
```{r}
head(activity_clean)
```

### Formatting other columns

```{r}
str(activity_clean)
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
activity$date <-as.Date(activity$date)
str(activity)
```

### AS is Tidy Data

```{r}
summary(activity)
```


#What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

- Calculate the total number of steps taken per day
- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
- Calculate and report the mean and median of the total number of steps taken per day


1 Calculate the total number of steps taken per day

```{r}
steps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
print()
```

2  Make a histogram of the total number of steps taken each day

```{r}
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
```


3 Calculate and report the mean and median of the total number of steps taken per day


```{r}
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
mean_steps
median_steps
```


#What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

1.1 First of all, Calculate average for interval with dplyr:

```{r}
interval <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
print
```

1.2 Then, plot it :

```{r}
p <- ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")
print
```


2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
interval[which.max(interval$steps),]
```

#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).
- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- Create a new dataset that is equal to the original dataset but with the missing data filled in.
- Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


1 Summarize all the missing values:
```{r}
table(is.na(activity$steps))
```
Missing values are 2304.

2 

```{r}
activity_clean <- activity
nas <- is.na(activity_clean$steps)
avg <- tapply(activity_clean$steps, activity_clean$interval, mean, na.rm=TRUE, simplify=TRUE)
activity_clean$steps[nas] <- avg[as.character(activity_clean$interval[nas])]
```


```{r}
table(is.na(activity_clean$steps))
```

3 Calculate the number of steps taken in each 5-minute interval per day :


```{r}
steps_clean <- activity_clean %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

3.1 Histogram:

```{r}
ggplot(steps_clean, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```



4 Calculate the mean and median steps with the filled in values:



```{r}

mean_steps_clean<- mean(steps_clean$steps, na.rm = TRUE)
median_steps_clean <- median(steps_clean$steps, na.rm = TRUE)
mean_steps_clean
median_steps_clean
```


#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```{r}
activity_clean <- mutate(activity_clean, weektype = ifelse(weekdays(activity_clean$date) == "Saturday" | weekdays(activity_clean$date) == "Sunday", "weekend", "weekday"))
activity_clean$weektype <- as.factor(activity_clean$weektype)
head(activity_clean)
```


2 finally we have :

```{r}
interval_clean<- activity_clean %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(interval_clean, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
```

Finish