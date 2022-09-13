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
garmin_raw_data <- read.csv('/Users/hiradch/Desktop/garmin-predictive-modeling/garmin_final.csv')

#select only times, HR and stress
garmin_raw_df <- subset(garmin_raw_data, select = c(1,5,15))

#### STEP 3 - Initial Data Clean-up and Transformations#####

#split timestamp into more fields
garmin_raw_df$timestamp <- as.POSIXct( garmin_raw_df$timestamp, tz = "UTC" )
garmin_raw_df$date <- as.Date(garmin_raw_df$timestamp)
### Add Month, Week and Day numbers
garmin_raw_df$week <- as.integer(strftime(garmin_raw_df$date, format = "%V"))
garmin_raw_df$month <- as.integer(strftime(garmin_raw_df$date, format = "%m"))
#Find Numeric Day of Week (Assuming Week Starts on Monday)
garmin_raw_df$day <- as.integer(wday(garmin_raw_df$date, week_start=1))
garmin_raw_df$hour <- as.integer(format(as.POSIXct(garmin_raw_df$timestamp),"%H"))
garmin_raw_df$min <- as.integer(format(as.POSIXct(garmin_raw_df$timestamp),"%M"))

#reorder columns
garmin_series <- select(garmin_raw_df,1,4,6,5,7:8,2,3)


## REMOVE SOME WEEKS 
#--->1) WEEK #22 - first week reading on device is not as accurate

library(naniar)
library("imputeTS")

# statsNA(garmin_series$heartRate) 
# statsNA(garmin_series$stressLevel)
# ggplot_na_intervals(garmin_series$heartRate, interval_size = 720, measure = "count",color_missing = "gold3")
# ggplot_na_distribution(garmin_series$heartRate, x_axis_labels = garmin_series$date)
# # ggplot_na_intervals(garmin_series_f$heartRate, interval_size = 720,color_missing = "gold3")
# ggplot_na_distribution(garmin_series$stressLevel, x_axis_labels = garmin_series$date)
# ggplot_na_intervals(garmin_series$stressLevel, interval_size = 720,color_missing = "gold3")

garmin_df <-garmin_series[!(garmin_series$week %in% c('22')),]
#REMOVED 5,000 datapoints, from 13 to 10 weeks

describe(garmin_df)
#Fill -1,-2 and NAs in Stress
garmin_df_2 <- garmin_df %>% replace_with_na(replace = list(stressLevel = c(-1,-2)))
describe(garmin_df_2)


#### STEP_4 - Visualize Missing Values/GAPS #####

## Visualize Time-series and remaining gaps to impute
#HEART RATE
ggplot_na_distribution(garmin_df_2$heartRate, x_axis_labels = garmin_df_2$timestamp)
ggplot_na_distribution(garmin_df_2$stressLevel, x_axis_labels = garmin_df_2$timestamp)

##-----> Visualize Missing on MONTHLY slices
#HEART RATE
m6 = garmin_df_2[garmin_df_2$month ==6,]
m7 = garmin_df_2[garmin_df_2$month ==7,]
m8 = garmin_df_2[garmin_df_2$month ==8,]

ggplot_na_distribution(m6$heartRate)
ggplot_na_distribution(m7$heartRate)
ggplot_na_distribution(m8$heartRate)

## STRESS LEVEL 
ggplot_na_distribution(m6$stressLevel)
ggplot_na_distribution(m7$stressLevel)
ggplot_na_distribution(m8$stressLevel)



#### STEP_5 - Impute Gaps in HR & Stress ####
library(naniar)
library("imputeTS")
##useful link --> https://cran.r-project.org/web/packages/imputeTS/vignettes/gallery_visualizations.html

#heartRate
statsNA(garmin_df_2$heartRate) # 17.1% missing with 212 gaps, ave gap of 47.6 datapoints
#stressLevel
statsNA(garmin_df_2$stressLevel) # 28.2% missing with 1714 gaps, ave gap of 9.7 --> only 20min

#SINCE THE AVE.GAP is 20min, we should resample to hourly instead of 2-min intervals

## IMPUTE ---> HEART RATE- Missing Value Imputation by Last Observation Carried Forward
#https://steffenmoritz.github.io/imputeTS/reference/na_locf.html
imp4<-na_locf(garmin_df_2$heartRate)
ggplot_na_imputations(garmin_df_2$heartRate, imp4)

## IMPUTE ---> STRESS LEVEL - Missing Value Imputation by Linear interpolation
imp1<-na_interpolation(garmin_df_2$stressLevel)
ggplot_na_imputations(garmin_df_2$stressLevel, imp1)


garmin_df_inter <- garmin_df_2
garmin_df_inter$stressLevel <-na_interpolation(garmin_df_2$stressLevel)
garmin_df_inter$heartRate <-na_locf(garmin_df_2$heartRate)

#---> RAW IMPUTED TIME SERIES PLOTS
ggplot_na_distribution(garmin_df_inter$stressLevel)
ggplot_na_distribution(garmin_df_inter$heartRate)

str(garmin_df_inter)


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


#### STEP_6 - Split data into train & Test set####

# Datasets --> garmin_df_stress.hourly & garmin_df_hr.hourly

train.xts = stress.xts["2022-06-06/2022-08-10"] #1580
test.xts = stress.xts["2022-08-11/"] #385

train.ts <-ts_ts(train.xts)
test.xts <-ts_ts(test.xts)


#### STEP_7 - Identify Time-Series Components####

#------------Visualize Decomposition---------#
library(forecast)
library(fpp)

plot.xts(train.xts)
plot.ts(train.ts)

data <- ts(train.ts, frequency = 24) 
fit <- stl(data,"periodic") # estimate the daily cyclical component
autoplot(fit)

#deaseason the data ro remove the pattern that occur every day
train.ts_deSeason = data - seasonal(fit)
plot.ts(train.ts)
plot.ts(train.ts_deSeason)

## -----Check for hetero Does the variance increases with mean (heteroscedasticity)?-----
#--> Statistical Testing -->  NO HETERO

#The McLeod-Li test is a version of the Ljung-Box test for autocorrelation based on the squared data.
#The alternative hypothesis is that the data have autoregressive conditional heteroscedasticity (ARCH).
library(TSA)
a =diff(log(train.xts))*100
McLeod.Li.test(y=a)

#------------De-trending-----------#

#--from the plot we see that our Stress series has variable trend but non-increasing over the week
#--we also observe that the it also has seasonality on daily basis when we decompose on 24hr
#---HENCE, our prelim suspicion is that the Stress series is NOT stationary

train.ts_deSeason_diff = diff(train.ts_deSeason,1)

#------------Formal Test for STATIONARITY-----------#

# Augmented Dickey-Fuller (ADF) test to check stationarity. 
#The null hypothesis assumes that the series is non-stationary.
#p-value=0.01, rejects the null. the series is stationary
library(astsa)
adf.test(train.ts_deSeason_diff,alternative = "stationary") #hmm...sounds like it is stationarty to me..

# Autocorrelation & Partial-Autocorrelation 
library(astsa)
#ACF 
Acf(diff(train.xts,1),lag.max = 50)
Acf(diff(train.ts_deSeason,1),lag.max = 50)
Acf(diff(train.ts_deSeason_diff,1),lag.max = 50)

Pacf(diff(train.xts,1),lag.max = 50)

plot.ts(train.xts) # OG
plot.ts(train.ts_deSeason) #TAKE OUT SEASONALITY
plot.ts(train.ts_deSeason_diff) #TAKE OUT TRENDING


#---->1) Check for seasonality - NONE EXISTS
#ACF function
# (ACFs and CCFs) as tools for clarifying relations that may occur within
# and between time series at various lags.

# # There are no seasonality between days and weeks
# w1 = train.ts[train.ts$week == '23',]
# w2 = train.ts[train.ts$week == '29',]
# w3 = train.ts[train.ts$week == '30',]
# Acf(diff(w1$stressLevel,1),lag.max =3600)
# Acf(diff(w2$stressLevel,1),lag.max =3600)
# Acf(diff(w3$stressLevel,1),lag.max =3600)
# 
# # There are no seasonality between days and weeks
# d1 = train.ts[(train.ts$week == '23' & train.ts$day == 2),]
# d2 = train.ts[(train.ts$week == '29' & train.ts$day == 4),]
# d3 = train.ts[(train.ts$week == '30' & train.ts$day == 1),]
# Acf(diff(d1$stressLevel,1),lag.max =200)
# Acf(diff(d2$stressLevel,1),lag.max =720)
# Acf(diff(d3$stressLevel,1),lag.max =720)




#-------------Modelling via Automatic ARIMA --------------------

model.auto <- auto.arima(train.xts, seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE, max.p = 10, max.q = 10) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
#---auto AIMA IS STUPID

# It suggests a ARIMA(5,0, 0) model with non-zero mean, 
checkresiduals(model.auto)  # Check the quality of fit. Residuals should: 
# The p-values for the Ljung–Box statistics are small, 
#....indicating there is some pattern in the residuals. There is still information to be extracted from the data.
# so this means that the daily cyclical pattern requires us to use SARIMA instead of ARIMA

fit_season <- Arima(train.xts, order=c(5,0,0), seasonal=list(order=c(0,1,1),period=24))
fit_season
# Plot the forecasting in the original scale
fc1<-forecast(fit_season,96)
autoplot(fc1)

summary(fit_season)


##TRY the same model using SARIMA function

fit_model = sarima(train.xts,5,0,0,0,1,1,24)

forecasted = sarima.for(train.xts,48,5,0,0,0,1,1,24)

?sarima.for
#------------- DO NOT USE - Automatic ARIMA Modeling - Use DE-seasoned ts -------------------

model.auto_2 <- auto.arima(train.ts_deSeason,trace=TRUE, max.p = 10, max.q = 10, stepwise = TRUE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto_2

# It suggests a seasonal ARIMA(1,0,2)(0,0,1) with  model with non-zero mean, AIC=12183.39   AICc=12183.44   BIC=12215.6
checkresiduals(model.auto_2)  # Check the quality of fit. Residuals should: 
#The p-value in the Ljung–Box test is large, 
#....indicating that the residuals are patternless—meaning all the information 
#......has been extracted by the model and only noise is left behind.

fit_season2 <- Arima(train.ts, order=c(1,0,2), seasonal=list(order=c(0,0,1),period=24))
fit_season2
# Plot the forecasting in the original scale
fc2<-forecast(fit_season2,96)
autoplot(fc2)

#-------------DO NOT USE - Automatic ARIMA Modeling - Use DE-seasoned + DIFF ts -------------------
model.auto_3 <- auto.arima(train.ts_deSeason_diff,trace=TRUE, max.p = 10, max.q = 10, stepwise = TRUE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto_3

# It suggests a Best model: ARIMA(4,0,0)(0,0,1)[24] with zero mean   with  model with non-zero mean, AIC=12282.86   AICc=12282.91   BIC=12315.06
# NOTE - the AIC/AICs/BIC of this model is slighlty worse than than the prev de-seasoned-only model, should we not diff by 1?

checkresiduals(model.auto_3)  # Check the quality of fit. Residuals should: 
# The p-values for the Ljung–Box statistics are small, 
#....indicating there is some pattern in the residuals. There is still information to be extracted from the data.


fit_season3 <- Arima(train.ts_deSeason_diff, order=c(1,0,2), seasonal=c(0,0,1))
fit_season3
# Plot the forecasting in the original scale
fc<-forecast(fit_season3,96)
autoplot(fc)









#Create a Box Plot by Day

#Create a Box Plot by Week



 
#-------------Make Predictions on test set with trained SARIMA model --------------------


# specify the xts test and train series 


# fit the selected sarima model on train set


## produce forecasts for next
myforecasts <- forecast::forecast(elecequip_sarima2, 385) 

## plot the forecasts
autoplot(myforecasts) + autolayer(elecequip_ts) + xlim(2005,2013) 


