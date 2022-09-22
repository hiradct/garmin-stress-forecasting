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



#### STEP_2 - Read the data ####
garmin_raw_data <- read.csv('/Users/hiradch/Desktop/garmin-predictive-modeling/garmin_upd.csv')

#select only times, HR and stress
garmin_raw_df <- subset(garmin_raw_data, select = c(1,5,15))

#### STEP 3 - Initial Data Clean-up and Transformations#####
library(lubridate)
#split timestamp into more fields
garmin_raw_df$timestamp <- as.POSIXct(garmin_raw_df$timestamp, tz = "UTC" )
garmin_raw_df$date <- as.Date(garmin_raw_df$timestamp)
### Add Month, Week and Day numbers
garmin_raw_df$week <- as.integer(strftime(garmin_raw_df$date, format = "%V"))
garmin_raw_df$month <- as.integer(strftime(garmin_raw_df$date, format = "%m"))
#Find Numeric Day of Week (Assuming Week Starts on Monday)
#garmin_raw_df$day <- wday(garmin_raw_df$date, week_start=1)
garmin_raw_df$hour <- as.integer(format(as.POSIXct(garmin_raw_df$timestamp),"%H"))
garmin_raw_df$min <- as.integer(format(as.POSIXct(garmin_raw_df$timestamp),"%M"))

#reorder columns
garmin_series <- select(garmin_raw_df,1,4,6,5,7:8,2,3)


## REMOVE SOME WEEKS 
#--->1) WEEK #19-22 - first week reading on device is not as accurate

library(naniar)
library("imputeTS")

# statsNA(garmin_series$heartRate) 
# statsNA(garmin_series$stressLevel)
# ggplot_na_intervals(garmin_series$heartRate, interval_size = 720, measure = "count",color_missing = "gold3")
# ggplot_na_distribution(garmin_series$heartRate, x_axis_labels = garmin_series$date)
# # ggplot_na_intervals(garmin_series_f$heartRate, interval_size = 720,color_missing = "gold3")
# ggplot_na_distribution(garmin_series$stressLevel, x_axis_labels = garmin_series$date)
# ggplot_na_intervals(garmin_series$stressLevel, interval_size = 720,color_missing = "gold3")

garmin_df <-garmin_series[!(garmin_series$week %in% c(19,20,21,22)),]

describe(garmin_df)
#Fill -1,-2 and NAs in Stress
garmin_df_2 <- garmin_df %>% replace_with_na(replace = list(stressLevel = c(-1,-2)))
describe(garmin_df_2)

#### STEP_5 - Impute Gaps in HR & Stress ####
library(naniar)
library("imputeTS")
##useful link --> https://cran.r-project.org/web/packages/imputeTS/vignettes/gallery_visualizations.html

## IMPUTE ---> HEART RATE- Missing Value Imputation by Last Observation Carried Forward
#https://steffenmoritz.github.io/imputeTS/reference/na_locf.html

garmin_df_inter <- garmin_df_2
garmin_df_inter$stressLevel <-na_interpolation(garmin_df_2$stressLevel)
garmin_df_inter$heartRate <-na_locf(garmin_df_2$heartRate)


#### STEP_6 - Create TS Objects & Resample to Hourly ####

#-Create separate data frames
garmin_df_stress = garmin_df_inter[,c(1,8)]
garmin_df_hr =  garmin_df_inter[,c(1,7)]

#-Resample from 2-min to hourly series w/ averaging
garmin_df_stress$timestamp2 <- droplevels(cut(garmin_df_stress$timestamp, breaks='hour'))
### This calculate the mean of stress in each hour at each date.
garmin_df_stress.hourly <- aggregate(stressLevel ~ timestamp2, data=garmin_df_stress, FUN=mean) 
garmin_df_stress.hourly$timestamp2 <- as.POSIXct(garmin_df_stress.hourly$timestamp2, tz = "UTC" )

garmin_df_hr$timestamp2 <- droplevels(cut(garmin_df_hr$timestamp, breaks='hour'))
### This calculate the mean of stress in each hour at each date.
garmin_df_hr.hourly <- aggregate(heartRate ~ timestamp2, data=garmin_df_hr, FUN=mean) 
garmin_df_hr.hourly$timestamp2 <- as.POSIXct(garmin_df_hr.hourly$timestamp2, tz = "UTC" )

#round the stress and hr decimals to 2 decimal points
garmin_df_stress.hourly$stressLevel <- as.integer(round(garmin_df_stress.hourly$stressLevel, 0))
garmin_df_hr.hourly$heartRate <- as.integer(round(garmin_df_hr.hourly$heartRate, 0))

#-Create time series objects using XTS
#https://www.datacamp.com/cheat-sheet/xts-cheat-sheet-time-series-in-r
library(xts)
stress.xts <- as.xts(garmin_df_stress.hourly[,"stressLevel"],garmin_df_stress.hourly$timestamp2,dateFormat="POSIXct")
plot.xts(stress.xts)

heart.rate.xts = as.xts(garmin_df_hr.hourly[,"heartRate"],garmin_df_hr.hourly$timestamp2,dateFormat="POSIXct")
plot.xts(heart.rate.xts)

#--> stress.xts
#---> heart.rate.xts

#Create TS objects from the XTS objects as well
library(tsbox)
stress.ts <-ts_ts(stress.xts)
heart.rate.ts <-ts_ts(heart.rate.xts)

plot.ts(stress.ts)
plot.ts(heart.rate.ts)


#### STEP_6 - SPLIT into HIST, SHORT-HIST & DEMO 2-day TEST####

# Datasets --> garmin_df_stress.hourly & garmin_df_hr.hourly

first(stress.xts) # 2022-06-06
last(stress.xts) # 2022-09-11
#remove the last 2 days
stress.xts <- stress.xts["/2022-09-10"]
last(stress.xts) # 2022-09-10


train.xts = stress.xts["2022-08-08/2022-09-08"]
last(train.xts)

demo.test.xts = stress.xts["2022-09-09/"] 
first(demo.test.xts)

demo.hist.xts = stress.xts["2022-09-01/2022-09-08"]
first(demo.hist.xts)
last(demo.hist.xts)


train.ts <-ts_ts(train.xts)
demo.test.ts <-ts_ts(demo.test.xts)
demo.hist.ts <-ts_ts(demo.hist.xts)

#### STEP_7 - Identify Time-Series Components####

#------------Visualize Decomposition---------#
library(forecast)
library(fpp)

# plot.xts(train.xts)
# plot.ts(train.ts)
# 
# data <- ts(train.ts, frequency = 24) 
# fit <- stl(data,"periodic") # estimate the daily cyclical component
# autoplot(fit)

#deaseason the data ro remove the pattern that occur every day
# train.ts_deSeason = data - seasonal(fit)
# plot.ts(train.ts)
# plot.ts(train.ts_deSeason)

## -----Check for hetero Does the variance increases with mean (heteroscedasticity)?-----
#--> Statistical Testing -->  NO HETERO



#-------------Modelling via Automatic ARIMA --------------------

model.auto <- auto.arima(train.xts, seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE, max.p = 10, max.q = 10) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
#---auto AIMA IS STUPID

# It suggests a ARIMA(4,0, 1) model with non-zero mean, 
#checkresiduals(model.auto)  # Check the quality of fit. Residuals should: 


##TRY the same model using SARIMA function
#fit_model = sarima(train.xts,4,0,1,0,1,1,24)

##-----------FINAL MODEL PARAMETERS-------##
p <- 4
d <- 0
q <- 1

P <- 0
D <- 1
Q <- 1

interval <- 24
prediction_window = 48 #NEXT 48 HOURS

#PREDICTION OF September 9-10
forecasted = sarima.for(demo.hist.xts,prediction_window,p,d,q,P,D,Q,interval, plot.all = TRUE)

predicted_demo_48hrs <- as.data.frame(forecasted) #pred is forcast and se is pred standard errors
names(predicted_demo_48hrs)[1] <- 'Predict_Stress'
names(predicted_demo_48hrs)[2] <- 'Predict_Stress_SE'

demo_actual_48hrs <- as.data.frame(demo.test.xts)
names(demo_actual_48hrs)[1] <- 'Actual_Stress'

demo_ouput_df <- cbind(demo_actual_48hrs,predicted_demo_48hrs)

demo_hist_df <-  as.data.frame(demo.hist.xts)
names(demo_hist_df)[1] <- 'Historical Stress'

myforecasts <- forecast::forecast(elecequip_sarima2, h=40) 
## plot the forecasts
autoplot(forecasted$pred) + autolayer(elecequip_ts) + xlim(2005,2013) 


####----------->>>>>>THERE YOU HAVE IT LADIES AND GENTLEMEN<<<<<------------####
demo_hist_df
demo_ouput_df


write.csv(demo_hist_df,'/Users/hiradch/Desktop/garmin-predictive-modeling/demo_hist_df.csv', row.names = TRUE)
write.csv(demo_ouput_df,'/Users/hiradch/Desktop/garmin-predictive-modeling/demo_ouput_df.csv',row.names = TRUE)


