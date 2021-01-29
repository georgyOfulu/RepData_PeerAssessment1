---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
This is my submission to the first course project of Reproducible Research[1] —— the fifth course of the John Hopkin's Data Science Specialization on Coursera. This assignment makes use of data[2] from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. The code chunk below shows the global options I have setup for the analysis.


```r
knitr::opts_chunk$set(
  echo = TRUE, cache = TRUE, warning = FALSE,
  message = FALSE, dpi = 180, tidy = TRUE
)
library(tidyverse) #package required for basic data manipulation and plotting
library(silgelib) #package required for theme_plex()
theme_set(theme_plex()) #custom ggplot2 theme
library(knitr) #package required for printing out nicely formatted tables.
library(lubridate) #package required for the wday() function
```

## Loading and preprocessing the data

```r
# unzip the data file if the file doesn't already exist.
if (!file.exists("activity.zip")) {
  unzip("activity.zip")
}
activityData <- read_csv("activity.csv")  #Load data
```

## What is mean total number of steps taken per day?
**1. Histogram of the total number of steps per day:** 

```r
# removing all rows with NAs
activityData_na.rm <- activityData %>% na.omit()

# some data manipulation for the plot
data_total_daily_steps <- activityData_na.rm %>%
  group_by(date) %>%
  summarize(total_daily_steps = sum(steps), mean = mean(steps), median = median(steps))

## ggplot histogram of the total number of steps per day.
data_total_daily_steps %>%
  ggplot(aes(total_daily_steps)) +
  geom_bar(fill = "#c49300") +
  scale_x_binned() +
  xlab("Daily total number of steps") +
  ylab(NULL) +
  labs(title = "The distribution of the daily total number of steps") +
  theme(
    panel.grid.major.x = element_blank(),             #remove all major gridlines in the x direction
    panel.grid.minor.y = element_blank(),             #remove all minor gridlines in the y direction
    plot.background = element_rect(fill = "#343434"), #fill plot background with custom shade of grey
    axis.text = element_text(colour = "white"),       #format the axis text in the x and y axes.
    axis.title = element_text(colour = "white", face = "bold"),                #format the axis title
    plot.title = element_text(colour = "white", face = "bold", hjust = 0.5)    #format the plot title
  )
```

![](PA1_template_files/figure-html/mean_steps_per_day-1.png)<!-- -->


**2.  Calculate and report the mean and median of the total number of steps taken per day:** The mean and median total number of steps per day is calculated as seen in the table below

```r
#define a vecor of column names to be used in the printed table
column_names <- c("Date", "Total Steps per Day", 
                  "Mean Total Steps per day",
                  "Median Total Steps per day")
#printing out the first 15 rows of the dataset
head(data_total_daily_steps, 15) %>% kable(format = "simple", col.names = column_names)
```



Date          Total Steps per Day   Mean Total Steps per day   Median Total Steps per day
-----------  --------------------  -------------------------  ---------------------------
2012-10-02                    126                    0.43750                            0
2012-10-03                  11352                   39.41667                            0
2012-10-04                  12116                   42.06944                            0
2012-10-05                  13294                   46.15972                            0
2012-10-06                  15420                   53.54167                            0
2012-10-07                  11015                   38.24653                            0
2012-10-09                  12811                   44.48264                            0
2012-10-10                   9900                   34.37500                            0
2012-10-11                  10304                   35.77778                            0
2012-10-12                  17382                   60.35417                            0
2012-10-13                  12426                   43.14583                            0
2012-10-14                  15098                   52.42361                            0
2012-10-15                  10139                   35.20486                            0
2012-10-16                  15084                   52.37500                            0
2012-10-17                  13452                   46.70833                            0



## What is the average daily activity pattern?
**1. Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):** 

```r
# some data manipulation to get the required dataframe for the plot.
data_interval_avg <- activityData_na.rm %>%
  group_by(interval) %>%
  summarize(daily_avg = mean(steps)) %>%
  ungroup()

#ggplot2 code for the required vizualization
data_interval_avg %>%
  ggplot(aes(interval, daily_avg)) +
  geom_line(col = "#c49300", size = 1.1) +          #modifying the thickness of the line and the color
  ylim(0, 250) +                                    #expanding the limits of the y axis to 250
  xlab("Interval") +                                
  ylab("Daily average steps per interval") +
  labs(title = "Average Daily activity pattern") +
  theme(
    panel.grid.major.x = element_blank(),           #removing all major gridlines in the x direction
    panel.grid.minor.x = element_blank(),           #removing all minor gridlines in the x direction
    panel.grid.minor.y = element_blank(),           #removing all minor gridlines in the y direction
    plot.background = element_rect(fill = "#343434"),    #set plot background to a custom grey color
    axis.text = element_text(colour = "white"),          #format the axis texts in the x and y axes
    axis.title = element_text(colour = "white", face = "bold"),               #format the axis text
    plot.title = element_text(colour = "white", face = "bold", hjust = 0.5) #format the plot title text
  ) 
```

![](PA1_template_files/figure-html/avg_daily_activity-1.png)<!-- -->

```r
max_steps <- max(data_interval_avg$daily_avg)
interval_max_steps <- data_interval_avg$interval[data_interval_avg$daily_avg == max_steps]
```
**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?:** From the data, it is evident that interval **835** has the highest average number of steps across the days in the data.

## Impute missing values


```r
# Calculating total missing values and storing it in a variable
total_missing_values <- sum(is.na(activityData))
```
**1. Calculate and report the total number of missing values in the dataset:** The total number of missing values in the data is **2304**  
**2. Devise a strategy for filling in all of the missing values in the dataset:** The missing values would be inputed using the mean for 5-minute-interval across the days.  
**3. Create a new dataset that is equal to the original dataset but with the missing data filled in:**  


```r
# join the activityData with the data on interval average by interval to compute missing values
activityData_na.impute <- activityData %>%
  inner_join(data_interval_avg, by = "interval") %>%
  mutate(steps = ifelse(is.na(steps), daily_avg, steps)) %>%
  select(-daily_avg)
#print out the first few rows of the imputed dataset
kable(head(activityData_na.impute), format = "simple", col.names = c("Steps", "Date", "Interval"))
```

     Steps  Date          Interval
----------  -----------  ---------
 1.7169811  2012-10-01           0
 0.3396226  2012-10-01           5
 0.1320755  2012-10-01          10
 0.1509434  2012-10-01          15
 0.0754717  2012-10-01          20
 2.0943396  2012-10-01          25

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day:** The values do not differ much from the previous estimates. 

```r
df <- activityData_na.impute %>%
  group_by(date) %>% 
  summarize(total_daily_steps2 = sum(steps), median = median(steps), mean = mean(steps))
#printing the first 15 rows of the data
kable(head(df, 15), format = "simple")
```



date          total_daily_steps2     median       mean
-----------  -------------------  ---------  ---------
2012-10-01              10766.19   34.11321   37.38260
2012-10-02                126.00    0.00000    0.43750
2012-10-03              11352.00    0.00000   39.41667
2012-10-04              12116.00    0.00000   42.06944
2012-10-05              13294.00    0.00000   46.15972
2012-10-06              15420.00    0.00000   53.54167
2012-10-07              11015.00    0.00000   38.24653
2012-10-08              10766.19   34.11321   37.38260
2012-10-09              12811.00    0.00000   44.48264
2012-10-10               9900.00    0.00000   34.37500
2012-10-11              10304.00    0.00000   35.77778
2012-10-12              17382.00    0.00000   60.35417
2012-10-13              12426.00    0.00000   43.14583
2012-10-14              15098.00    0.00000   52.42361
2012-10-15              10139.00    0.00000   35.20486

```r
df %>% 
  ggplot(aes(total_daily_steps2)) +
  geom_bar(fill = "#c49300") +
  scale_x_binned() +
  xlab("Daily total number of steps") +
  ylab(NULL) +
  labs(title = "The distribution of the daily total number of steps") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#343434"),
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "white", face = "bold"),
    plot.title = element_text(colour = "white", face = "bold", hjust = 0.5)
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

```r
 activityData_na.impute %>% 
mutate(day_type = lubridate::wday(date)) %>% 
mutate(day_type = as.factor(ifelse(day_type <= 5, "Weekday", "Weekend")))  %>% 
  group_by(interval, day_type) %>% 
  summarize(avg_steps = mean(steps)) %>% 
  ggplot(aes(x = interval, y = avg_steps)) +
  geom_line(col = "#c49300", size = 1.1) +
  facet_wrap(~day_type, nrow = 2) +
  ylim(0, 250) +
  xlab("Interval") +
    ylab("Daily average steps per interval") +
  labs(title = "Difference in average Daily activity pattern between the weekends and weekdays") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black", face = "bold"),
    plot.title = element_text(colour = "black", face = "bold", hjust = "1", size = 13),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold", hjust = 0.5)
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
