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
library(naniar)


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
#Create trimmed version of df with only variables of interest
garmin_series = garmin_all[,c(1:7)]


### Add Month, Week and Day numbers
garmin_series$week <- strftime(garmin_series$date, format = "%V")
garmin_series$month <- strftime(garmin_series$date, format = "%m")
#Find Numeric Day of Week (Assuming Week Starts on Monday)
garmin_series$day <- wday(garmin_series$date, week_start=1)

#REMOVE SAT-SUN - To adjust for lifestyle factor, only include week days (Mon-Friday)
garmin_series_f <-garmin_series[!(garmin_series$day %in% c(6,7)),]


## REMOVE SOME WEEKS 
#--->1) WEEK #22 - first week reading on device is not as accurate
#--->2) WEEK #31 has large HR gap day 
#--->3) WEEK #33 has large Stress gap

#->Before
statsNA(garmin_series_f$heartRate) #BEFORE - #gaps = 170 w/ ave. 45.23 and longest 394
statsNA(garmin_series_f$stressLevel)
#ggplot_na_intervals(garmin_series_f$heartRate, interval_size = 720, measure = "count",color_missing = "gold3")
# ggplot_na_distribution(garmin_series_f$heartRate, x_axis_labels = garmin_series_f$date)
# ggplot_na_intervals(garmin_series_f$heartRate, interval_size = 720,color_missing = "gold3")
# ggplot_na_distribution(garmin_series_f$stressLevel, x_axis_labels = garmin_series_f$date)
# ggplot_na_intervals(garmin_series_f$stressLevel, interval_size = 720,color_missing = "gold3")

garmin_series_ff <-garmin_series_f[!(garmin_series_f$week %in% c('22','31','33')),]
#REMOVED 10,000 datapoints, from 13 to 10 weeks

#->After
statsNA(garmin_series_ff$heartRate) #BEFORE - #gaps = 129 w/ ave. 42
statsNA(garmin_series_ff$stressLevel)
#ggplot_na_intervals(garmin_series_f$heartRate, interval_size = 720, measure = "count",color_missing = "gold3")
# ggplot_na_distribution(garmin_series_ff$heartRate, x_axis_labels = garmin_series_ff$date)
# ggplot_na_intervals(garmin_series_ff$heartRate, interval_size = 720,color_missing = "gold3")
# ggplot_na_distribution(garmin_series_ff$stressLevel, x_axis_labels = garmin_series_ff$date)
# ggplot_na_intervals(garmin_series_ff$stressLevel, interval_size = 720,color_missing = "gold3")

#Fill -1,-2 and NAs in Stress
garmin_series_ff <- garmin_series_f %>% replace_with_na(replace = list(stressLevel = c(-1,-2)))


##### STEP_3 - Visualize Missing Values/GAPS #####

## Visualize Time-series and remaining gaps to impute
#HEART RATE
ggplot_na_distribution(garmin_series_ff$heartRate, x_axis_labels = garmin_series_ff$timestamp)
ggplot_na_distribution(garmin_series_ff$stressLevel, x_axis_labels = garmin_series_ff$timestamp)

##-----> Visualize Missing on MONTHLY slices
#HEART RATE
m5 = garmin_series_ff[garmin_series_ff$month == '05',]
m6 = garmin_series_ff[garmin_series_ff$month =='06',]
m7 = garmin_series_ff[garmin_series_ff$month =='07',]
m8 = garmin_series_ff[garmin_series_ff$month =='08',]

ggplot_na_distribution(m5$heartRate)
ggplot_na_distribution(m6$heartRate)
ggplot_na_distribution(m7$heartRate)
ggplot_na_distribution(m8$heartRate)

## STRESS LEVEL 
ggplot_na_distribution(m5$stressLevel)
ggplot_na_distribution(m6$stressLevel)
ggplot_na_distribution(m7$stressLevel)
ggplot_na_distribution(m8$stressLevel)


#### STEP_4 - Impute Gaps in HR & Stress ####

#Narrow Down to for heart rate and stress
garmin_procd = garmin_series_ff[,c(1:3,8:10,5,6)]


library(naniar)
library("imputeTS")
##useful link --> https://cran.r-project.org/web/packages/imputeTS/vignettes/gallery_visualizations.html

#heartRate
statsNA(garmin_series$heartRate)
#stressLevel
statsNA(garmin_series$stressLevel)

## IMPUTE ---> HEART RATE- Missing Value Imputation by Last Observation Carried Forward
#https://steffenmoritz.github.io/imputeTS/reference/na_locf.html
imp4<-na_locf(garmin_procd$heartRate)
ggplot_na_imputations(garmin_procd$heartRate, imp4)

## IMPUTE ---> STRESS LEVEL - Missing Value Imputation by Linear interpolation
imp1<-na_interpolation(garmin_procd$stressLevel)
ggplot_na_imputations(garmin_procd$stressLevel, imp1)


garmin_procd_inter <- garmin_procd
garmin_procd_inter$stressLevel <-na_interpolation(garmin_procd$stressLevel)
garmin_procd_inter$heartRate <-na_locf(garmin_procd$heartRate)

ggplot_na_distribution(garmin_procd_inter$stressLevel)
ggplot_na_distribution(garmin_procd_inter$heartRate)





