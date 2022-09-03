#### Load libraries ####
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


#### Load the data & Preproc#####
garmin_raw_data <- read.csv('/Users/hiradch/Desktop/garmin-predictive-modeling/garmin_final.csv')


#drop napTimeSeconds - - HIRAD TAKES NO NAPS!
#drop sleepStress - redundant, already captured in the stressLevel field
garmin_df1 <- subset(garmin_raw_data, select = -c(8,14))

#split timestamp into date, time
garmin_df1$date <- as.Date(garmin_df1$timestamp)
garmin_df1$time <- format(as.POSIXct(garmin_df1$timestamp),"%H:%M:%S")
garmin_df1$timestamp <- as.POSIXct( garmin_df1$timestamp, tz = "UTC" )

#reorder columns
garmin_df2 <- select(select(garmin_df1,17,18,1:16), 1:4,7,15,17,14,16,5,6,8,9:13,18)
#select(data,3,1,2)


#### EDA ####

##Understand dataset format (READ DATA DICTIONARY BEFORE STARTING)
str(garmin_df2)
describe(garmin_df2)
summary(garmin_df2)

Hmisc::describe(garmin_df2)

##------> The dataset consists of  64,081 observations with 18 variables
##------> Spans from 2022-05-30 to 2022-08-26
##------> timestamp is every 2min, 720 observations per 24hrs/day
##------> Variables that 'should' be populated in all rows and recorded by device every 2-min:
                  # timestamp, steps, primaryActivityLevel, activityLevelConstant, 
                  #...heartRate, stressLevel, bodyBatteryLevel, bodyBatteryStatus, bodyBatteryVersion

##### Missing Data Assessment #####

# Missing values % in each column
colMeans(is.na(garmin_df2))

# pattern of missingness in our most-time-continuous variables
library(naniar)
vis_miss(garmin_df2[c(4:7)])
# pattern of missingness in other variables
vis_miss(garmin_df2[c(8:18)])

#which days have the most missing values in the four variables step, HR, stress, bodybattery?
##useful linek --> https://cran.r-project.org/web/packages/imputeTS/vignettes/gallery_visualizations.html
library("imputeTS")

##Heart rate has 17% missing values across 90 days, which period contributes the most?
##FYI - Every bin represent a day
ggplot_na_intervals(garmin_df2$heartRate, 
                    interval_size = 720,
                    title ="Distribution of Missing Heart Rate Values")

##Body Batter has 13% missing values across 90 days, which period contributes the most?
ggplot_na_intervals(garmin_df2$bodyBatteryLevel, 
                    interval_size = 720,
                    title ="Distribution of Missing Body Battery Level Values")


##We can also visualize the gaps in time-series format
##-------> HEART RATE GAPS
#June
ggplot_na_distribution(x = garmin_df2[c(1:21600),]$heartRate, 
                       x_axis_labels = garmin_df2[c(1:21600),]$timestamp,
                       title="Missing Values - Heart Rate - June")
#July
ggplot_na_distribution(x = garmin_df2[c(21601:43199),]$heartRate, 
                       x_axis_labels = garmin_df2[c(21601:43199),]$timestamp,
                       title="Missing Values - Heart Rate - July")

#August
ggplot_na_distribution(x = garmin_df2[c(43200:64081),]$heartRate, 
                       x_axis_labels = garmin_df2[c(43200:64081),]$timestamp,
                       title="Missing Values - Heart Rate - August")

##-------> STRESS LEVEL GAPS
#June
ggplot_na_distribution(x = garmin_df2[c(1:21600),]$stressLevel, 
                       x_axis_labels = garmin_df2[c(1:21600),]$timestamp,
                       title="Missing Values - Stress Level - June")
#July
ggplot_na_distribution(x = garmin_df2[c(21601:43199),]$stressLevel, 
                       x_axis_labels = garmin_df2[c(21601:43199),]$timestamp,
                       title="Missing Values - Stress Level - July")

#August
ggplot_na_distribution(x = garmin_df2[c(43200:64081),]$stressLevel, 
                       x_axis_labels = garmin_df2[c(43200:64081),]$timestamp,
                       title="Missing Values - Stress Level- August")

##-------> BODY BATTERY LEVEL GAPS
#June
ggplot_na_distribution(x = garmin_df2[c(1:21600),]$bodyBatteryLevel, 
                       x_axis_labels = garmin_df2[c(1:21600),]$timestamp,
                       title="Missing Values - Body Battery - June")
#July
ggplot_na_distribution(x = garmin_df2[c(21601:43199),]$bodyBatteryLevel, 
                       x_axis_labels = garmin_df2[c(21601:43199),]$timestamp,
                       title="Missing Values - Body Battery - July")

#August
ggplot_na_distribution(x = garmin_df2[c(43200:64081),]$bodyBatteryLevel, 
                       x_axis_labels = garmin_df2[c(43200:64081),]$timestamp,
                       title="Missing Values - Body Battery- August")

##-------> STEPS LEVEL GAPS
#June
ggplot_na_distribution(x = garmin_df2[c(1:21600),]$steps, 
                       x_axis_labels = garmin_df2[c(1:21600),]$timestamp,
                       title="Missing Values - Steps - June")
#July
ggplot_na_distribution(x = garmin_df2[c(21601:43199),]$steps, 
                       x_axis_labels = garmin_df2[c(21601:43199),]$timestamp,
                       title="Missing Values - Steps - July")

#August
ggplot_na_distribution(x = garmin_df2[c(43200:64081),]$steps, 
                       x_axis_labels = garmin_df2[c(43200:64081),]$timestamp,
                       title="Missing Values - Steps- August")



##### Missing Data Treatment - Removing Days TBD #####

#Step_1 Remove any day missing more than 30% of its heart rate values (more than 360/720)
#Step_2 Remove any day missing more than 50% of its stress level (more than 360/720)


##### Looking only at Heart Rate #####

#
imp <- na_interpolation(tsAirgap)
ggplot_na_imputations(tsAirgap, imp)