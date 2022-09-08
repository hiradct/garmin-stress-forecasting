#### STEP_1 - Load libraries ####
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sqldf)
library(readxl)
library(mice)
library(car)
library(estimatr)
library(caret)
#install.packages("GGally")
library(GGally)
library(ggcorrplot)
#install.packages("mosaic")
library(mosaic)
library(pROC)
library(Hmisc)
#install.packages('forecast')
library(forecast)


#### STEP 2 - Initial Data Clean-up and Transformations#####
garmin_raw_data <- read.csv('/Users/hiradch/Desktop/garmin-predictive-modeling/garmin_final.csv')
#drop napTimeSeconds, sleepStress - HIRAD TAKES NO NAPS!
garmin_df1 <- subset(garmin_raw_data, select = -c(8,14))

#split timestamp into date, time
garmin_df1$date <- as.Date(garmin_df1$timestamp)
garmin_df1$time <- format(as.POSIXct(garmin_df1$timestamp),"%H:%M:%S")
garmin_df1$timestamp <- as.POSIXct( garmin_df1$timestamp, tz = "UTC" )
#reorder columns
garmin_all <- select(select(garmin_df1,17,18,1:16), 1:4,7,15,17,14,16,5,6,8,9:13,18)

##Understand dataset
str(garmin_all)
describe(garmin_all)
summary(garmin_all)
Hmisc::describe(garmin_all)

##------> The dataset consists of  64,081 observations with 18 variables
##------> Spans from 2022-05-30 to 2022-08-26
##------> timestamp is every 2min, 720 observations per 24hrs/day
##------> Variables that 'should' be populated in all rows and recorded by device every 2-min:
# timestamp, steps, primaryActivityLevel, activityLevelConstant, 
#...heartRate, stressLevel, bodyBatteryLevel, bodyBatteryStatus, bodyBatteryVersion

#Create trimmed version of df with only variables of interest
garmin_series = garmin_all[,c(1:7)]

##Get week# from date - add new column
garmin_series$week <- strftime(garmin_series$date, format = "%V")
hr_series = garmin_series[,c(1:3,8,5)]
stress_series = garmin_series[,c(1:3,8,6)]

## Time Series Visualization & Missing Value Gaps in Daily Samples ##
#------> 1) % of total missing values in each variable over 90 days
colMeans(is.na(garmin_series))


##### STEP_3 - Missing/Gaps Treatment #####

#### 3A: Visualize ALL/Monthly plots of GAPS ####
##HEART RATE 
#June
ggplot_na_distribution(x = garmin_series[c(1:21600),]$heartRate, 
                       x_axis_labels = garmin_series[c(1:21600),]$timestamp,
                       title="Missing Values - Heart Rate - June")
#June - Week 1
ggplot_na_distribution(x = garmin_series[c(1:5040),]$heartRate, 
                       x_axis_labels = garmin_series[c(1:5040),]$timestamp,
                       title="Missing Values - Heart Rate - June")
#July
ggplot_na_distribution(x = garmin_series[c(21601:43199),]$heartRate, 
                       x_axis_labels = garmin_series[c(21601:43199),]$timestamp,
                       title="Missing Values - Heart Rate - July")

#August
ggplot_na_distribution(x = garmin_series[c(43200:64081),]$heartRate, 
                       x_axis_labels = garmin_series[c(43200:64081),]$timestamp,
                       title="Missing Values - Heart Rate - August")

## STRESS LEVEL 

##what does the random -1, -2 recordings mean? 
#garmin says..."If you are too active to determine stress level 
#....(such as during a workout), a stress level will not be recorded."

#but what is the distinction between -1 and -2 value?
##--> my theory from observing when these values occur in data is that whenever HRV(which is hidden from us)
#...is high, Garmin pauses on stress score then when user reaches a more stable period it start recording again
#June
ggplot_na_distribution(x = garmin_series[c(1:21600),]$stressLevel, 
                       x_axis_labels = garmin_series[c(1:21600),]$timestamp,
                       title="Missing Values - Stress Level - June")
#July
ggplot_na_distribution(x = garmin_series[c(21601:43199),]$stressLevel, 
                       x_axis_labels = garmin_series[c(21601:43199),]$timestamp,
                       title="Missing Values - Stress Level - July")

#August
ggplot_na_distribution(x = garmin_series[c(43200:64081),]$stressLevel, 
                       x_axis_labels = garmin_series[c(43200:64081),]$timestamp,
                       title="Missing Values - Stress Level- August")

## BODY BATTERY LEVEL
#June
ggplot_na_distribution(x = garmin_series[c(1:21600),]$bodyBatteryLevel, 
                       x_axis_labels = garmin_series[c(1:21600),]$timestamp,
                       title="Missing Values - Body Battery - June")
#July
ggplot_na_distribution(x = garmin_series[c(21601:43199),]$bodyBatteryLevel, 
                       x_axis_labels = garmin_series[c(21601:43199),]$timestamp,
                       title="Missing Values - Body Battery - July")

#August
ggplot_na_distribution(x = garmin_series[c(43200:64081),]$bodyBatteryLevel, 
                       x_axis_labels = garmin_series[c(43200:64081),]$timestamp,
                       title="Missing Values - Body Battery- August")

##STEPS
#June
ggplot_na_distribution(x = garmin_series[c(1:21600),]$steps, 
                       x_axis_labels = garmin_series[c(1:21600),]$timestamp,
                       title="Missing Values - Steps - June")
#July
ggplot_na_distribution(x = garmin_series[c(21601:43199),]$steps, 
                       x_axis_labels = garmin_series[c(21601:43199),]$timestamp,
                       title="Missing Values - Steps - July")

#August
ggplot_na_distribution(x = garmin_series[c(43200:64081),]$steps, 
                       x_axis_labels = garmin_series[c(43200:64081),]$timestamp,
                       title="Missing Values - Steps- August")


#### 3B: Remove Weeks with low data-availability  ####
#Define WEEK as window criteria for gaps 

# pattern of missingness in our most-time-continuous variables
library(naniar)
library("imputeTS")
#which days have the most missing values in the four variables step, HR, stress, bodybattery?
##useful link --> https://cran.r-project.org/web/packages/imputeTS/vignettes/gallery_visualizations.html
##FYI - Every bin represent a day
ggplot_na_intervals(garmin_series$heartRate, 
                    interval_size = 720,
                    title ="Distribution of Missing Heart Rate Values")

##Missing on weekly slices
#example W23
w23 = garmin_series[garmin_series$week ==23,]
ggplot_na_distribution(w23$heartRate, x_axis_labels = w23$date)
ggplot_na_intervals(w23$heartRate,
                    interval_size = 720,
                    title ="Distribution of Missing Heart Rate Values - WEEK 23")
#Number of occurance of each types of gaps (i.e,  how many gaps with 4min, or 10min)
#GAP limit of 20min aka 10 datapoints
a = ggplot_na_gapsize(garmin_series[garmin_series$week ==30,]$heartRate, include_total = F)
#in W30 we see the LARGEST gap is 394(or 13.1hrs)

##----> Criteria for acceptable GAPS: no more than 2hrs gap type on any week series
        #2 hours --> 120min --> 60 datapoints --> 60 MAX GAP of occurrence

#What are the max gap size in each Week? (22-34)
#LATER - create a function that computes the max missing gap size in weekly series
w22 = garmin_series[garmin_series$week ==22,]
ggplot_na_gapsize(w22$heartRate, include_total = F)
max_gap_w22  = max(ggplot_na_gapsize(w22$heartRate, include_total = F)[["plot_env"]][["totals_bar"]])

w23 = garmin_series[garmin_series$week ==23,]
ggplot_na_gapsize(w23$heartRate, include_total = F)
max_gap_w23  = max(ggplot_na_gapsize(w23$heartRate, include_total = F)[["plot_env"]][["totals_bar"]])

w24 = garmin_series[garmin_series$week ==24,]
ggplot_na_gapsize(w24$heartRate, include_total = F)
max_gap_w23  = max(ggplot_na_gapsize(w24$heartRate, include_total = F)[["plot_env"]][["totals_bar"]])




#ggplot_na_intervals(garmin_series$stressLevel, interval_size = 720,title ="Distribution of Missing Body Battery Level Values")


#### IGNORE THIS CODE -- TRIM the daily window to be from 9AM to 10PM #### 
library(tibbletime)
garmin_series_trim <- data.frame() 
dateslist = unique(garmin_series$date)
for(i in 1:length(dateslist)) {
  
  aaa = garmin_series[garmin_series$date==dateslist[i],]
  FB <- as_tbl_time(aaa, index = timestamp)
  bbb = filter_time(FB,paste(dateslist[i],' 09:00:00') ~ paste(dateslist[i],' 20:58:00'))
  garmin_series_trim <- rbind(garmin_series_trim, bbb)
}
# NOW there will be 360 observations per 24hrs/day

str(garmin_series_trim)
bj = garmin_series_trim[c(1:360),]

## Let us see how the daily missing values look like now for 9AM-9PM Windows
## ---> looks better already
ggplot_na_intervals(garmin_series_trim$heartRate, 
                    interval_size = 360,
                    title ="Distribution of Missing Heart Rate Values")

ggplot_na_intervals(garmin_series_trim$stressLevel, 
                    interval_size = 360,
                    title ="Distribution of Missing Body Battery Level Values")

####---------

##We can also visualize the gaps in time-series format



##### Missing Data Treatment - Removing Days TBD #####

#Step_1 Remove any weeks with more than any instance of gaps >5hrs
## or any weeks with any days missing more than 30% total of its heart rate values (more than 360/720)


#Step_2 Remove any day missing more than 50% of its stress level (more than 360/720)


##### Looking only at Heart Rate #####

#
imp <- na_interpolation(tsAirgap)
ggplot_na_imputations(tsAirgap, imp)