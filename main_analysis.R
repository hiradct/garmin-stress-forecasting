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
install.packages('Metrics')
library(forecast)
library(naniar)
library(Metrics)


#### STEP_2 - Read the data - READ ONLY LATEST FINAL CSV ####
garmin_raw_data <- read.csv('/Users/hiradch/Desktop/garmin-predictive-modeling/garmin_upd.csv')

#select only times, HR and stress
garmin_raw_df <- subset(garmin_raw_data, select = c(1,5,15))

#### STEP 3 - Initial Data Clean-up and Transformations#####

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

#reorder columns
garmin_series <- select(garmin_raw_df,1,4,6,5,7:8,2,3)

## REMOVE SOME WEEKS 
#--->1) WEEK #19-22 - first week reading on device is not as accurate
#---> Remaining Weeks --.> 23-36 --> 14 weeks
# Value         23    24    25    26    27    28    29    30    31    32    33    34    35    36
# Frequency   5040  5040  5040  5040  5040  5040  5040  5040  5040  5040  5040  5040  5040  4321
# Proportion 0.072 0.072 0.072 0.072 0.072 0.072 0.072 0.072 0.072 0.072 0.072 0.072 0.072 0.062

library(naniar)
library("imputeTS")

# statsNA(garmin_series$heartRate) 
# statsNA(garmin_series$stressLevel)
# ggplot_na_intervals(garmin_series$heartRate, interval_size = 720, measure = "count",color_missing = "gold3")
# ggplot_na_distribution(garmin_series$heartRate, x_axis_labels = garmin_series$date)
# # ggplot_na_intervals(garmin_series_f$heartRate, interval_size = 720,color_missing = "gold3")
ggplot_na_distribution(garmin_series$stressLevel, x_axis_labels = garmin_series$date)
ggplot_na_intervals(garmin_series$stressLevel, interval_size = 720,color_missing = "gold3")

garmin_df <-garmin_series[!(garmin_series$week %in% c(19,20,21,22)),]
ggplot_na_distribution(garmin_df$stressLevel, x_axis_labels = garmin_df$timestamp)

plot.ts(as.ts(garmin_df$stressLevel))

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


#### STEP_6 - Resample to Hourly ####

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



#### STEP_7 - Split data into diff training hist Vectors + one week prediction period ####


# Datasets --> garmin_df_stress.hourly & garmin_df_hr.hourly
#---lowest : 2022-06-06 00:00:00
#---highest: 2022-09-10 20:00:00

#-Create time series objects using XTS and ts
library(xts)
stress.xts <- as.xts(garmin_df_stress.hourly[,"stressLevel"],garmin_df_stress.hourly$timestamp2,dateFormat="POSIXct")
plot.xts(stress.xts)
library(tsbox)
stress.ts <-ts_ts(stress.xts)

# 4 day prediction from 7 day history

#DEMO Forecast Set --> ["2022-09-09/2022-09-10"] (2 days)
#TEST --> ["2022-09-05/022-09-08"] (4 days)

# demo.test.xts <- stress.xts["2022-09-09/"]
# demo.hist.xts = stress.xts["2022-09-01/2022-09-08"]
TEST.xts = stress.xts["2022-09-05/2022-09-08"] 

#PAST_1 --> ["2022-08-29/2022-09-04"] (1 week) (7 days) (Mon-Sunday)
#PAST_2 --> ["2022-08-22/2022-09-04"] (2 weeks) (14 days)
#PAST_4 --> ["2022-08-08/2022-09-04"] (4 weeks) (28 days) 
#PAST_8 --> ["2022-07-11/2022-09-04"] (8 weeks) (56 days) 
#PAST_12 --> ["2022-06-13/2022-09-08"] (12 weeks) (84 days) 

PAST_1.xts = stress.xts["2022-08-29/2022-09-04"]
PAST_2.xts = stress.xts["2022-08-22/2022-09-04"] 
PAST_4.xts = stress.xts["2022-08-08/2022-09-04"] 
PAST_8.xts = stress.xts["2022-07-11/2022-09-04"]
PAST_12.xts = stress.xts["2022-06-13/2022-09-08"]




# #train.xts = stress.xts["2022-06-08/2022-09-08"] #train set of 3 months
# #train.xts = stress.xts["2022-07-08/2022-09-08"] #train set of 2 months
# train.xts = stress.xts["2022-08-08/2022-09-04"] #train set of 1 months
# 
# train.ts <-ts_ts(train.xts)
# demo.test.ts <-ts_ts(demo.test.xts)
# demo.hist.ts <-ts_ts(demo.hist.xts)
# 
# plot.ts(stress.ts)


#### STEP_8 - Identify Time-Series Components####

#------------Visualize Decomposition---------#
library(forecast)
library(fpp)

plot.xts(PAST_4.xts)
PAST_4.ts <- ts_ts(PAST_4.xts)
plot.ts(PAST_4.ts)


#Decompose on daily
data_d <- ts(PAST_4.ts, frequency = 24) 
fit_d <- stl(data_d,"periodic") # estimate the daily cyclical component
autoplot(fit_d)
summary(fit_d)


#Decompose daily, seasonality window = 24hr
fit_d_1 <- stl(data_d,s.window = 24) # estimate the daily cyclical component
autoplot(fit_d_1)
summary(fit_d_1)



#### STEP_9 - Check for hetero Does the variance increases with mean (heteroscedasticity)?-----
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





#### STEP_10 - Build Models with varying training history size --------------------

## MODEL#1 DEV --> One Week Training Window, Trained Model --> (1,0,1)(0,1,1) ####

#Obtain non-seasonal params from Auto ARIMA --> RESULT: (1,0,1)

model_1b <- auto.arima(PAST_1.xts, seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE, max.p = 10, max.q = 10) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
checkresiduals(model_1) # The p-values for the Ljung–Box statistics are LARGE,--> ARIMA is sufficient 
#....indicating there is some pattern in the residuals. There is still information to be extracted from the data.
# so this means that the daily cyclical pattern requires us to use SARIMA instead of ARIMA


#Manually incorporate order=c(0,1,1) for seasonal component
model_1_SARIMA <- Arima(PAST_1.xts, order=c(1,0,1), seasonal=list(order=c(0,1,1),period=24))
summary(model_1_SARIMA)
checkresiduals(model_1_SARIMA)
#sarima is a better fit

model_1
model_1_SARIMA

## MODEL#1 EVAL --> Past 1 Test --> fcp1_RMSE = 18.43 #####

forecasted_past1 = sarima.for(PAST_1.xts,96,1,0,1,0,1,1,24)

forecasted_past1_df <- as.data.frame(forecasted_past1) #input forecasts to dataframe
TEST1_df <- as.data.frame(TEST.xts) #input actuals to dataframe
names(forecasted_past1_df)[1] <- 'Predicted_Stress'
names(TEST1_df)[1] <- 'Actual_Stress'
TEST1_PAST_1_comb<- cbind(forecasted_past1_df,TEST1_df)

fcp1_MSE <- mean((TEST1_PAST_1_comb$Predicted_Stress-TEST1_PAST_1_comb$Actual_Stress)^2) #calculate and display MSE in the testing set
#MSE, is calculated as the average of the squared forecast error values. Squaring the forecast error values forces them to be positive; 
#it also has the effect of putting more weight on large errors.
fcp1_RMSE <- rmse(TEST1_PAST_1_comb$Actual_Stress, TEST1_PAST_1_comb$Predicted_Stress) #calculate and display RMSE 
# MSE can be transformed back into the original units of the predictions by taking the square root of the mean squared error score

TEST1_PAST_1_comb.xts <- as.xts(TEST1_PAST_1_comb)

myplot_test1 <- plot.xts(TEST1_PAST_1_comb.xts$Actual_Stress, main = "Actual vs Forecasted Stress", lwd = 3, grid.col = NA, col = "Blue")
lines(TEST1_PAST_1_comb.xts$Predicted_Stress, col = "Red", lwd = 3)



## MODEL#2 DEV --> Two Week Training Window, Trained Model --> (4,0,1)(0,1,1) ####

model_2 <- auto.arima(PAST_2.xts,seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE, max.p = 10, max.q = 10) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)

checkresiduals(model_2) 
summary(model_2)

model_2_SARIMA <- Arima(PAST_2.xts, order=c(4,0,1), seasonal=list(order=c(0,1,1),period=24))
#model_2_SARIMA_2 <- sarima(PAST_2.xts,4,0,1,0,1,1,24) #using SARIMA function
summary(model_2_SARIMA)
checkresiduals(model_2_SARIMA)


model_2
model_3_SARIMA

## MODEL#2 EVAL --> Past 2 Test --> fcp2_RMSE = 15.80776 #####

forecasted_past2 = sarima.for(PAST_2.xts,96,4,0,1,0,1,1,24)

forecasted_past2_df <- as.data.frame(forecasted_past2) #input forecasts to dataframe
#TEST1_df <- as.data.frame(TEST.xts) #input actuals to dataframe
names(forecasted_past2_df)[1] <- 'Predicted_Stress'
#names(TEST1_df)[1] <- 'Actual_Stress'
TEST1_PAST_2_comb<- cbind(forecasted_past2_df,TEST1_df)

fcp2_MSE <- mean((TEST1_PAST_2_comb$Predicted_Stress-TEST1_PAST_2_comb$Actual_Stress)^2) #calculate and display MSE in the testing set
fcp2_RMSE <- rmse(TEST1_PAST_2_comb$Actual_Stress, TEST1_PAST_2_comb$Predicted_Stress) #calculate and display RMSE 

TEST1_PAST_2_comb.xts <- as.xts(TEST1_PAST_2_comb)

myplot_test1 <- plot.xts(TEST1_PAST_2_comb.xts$Actual_Stress, main = "Actual vs Forecasted Stress", lwd = 3, grid.col = NA, col = "Blue")
lines(TEST1_PAST_2_comb.xts$Predicted_Stress, col = "Red", lwd = 3)

fcp2_RMSE


## MODEL#3 DEV --> **** FINAL MODEL **** Four Week Training Window, Trained Model --> (4,0,1)(0,1,1) ####

model_3 <- auto.arima(PAST_4.xts, max.order = 20,seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
# model_3b <- auto.arima(PAST_4.xts,D=1,seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE)
# 
# Lambda <- BoxCox.lambda(ts_ts(PAST_4.xts))
# model_3c <- auto.arima(PAST_4.xts,lambda = Lambda,D=1,seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE, max.p = 10, max.q = 10)

checkresiduals(model_3) 
summary(model_3)

model_3_SARIMA <- Arima(PAST_4.xts, order=c(4,0,1), seasonal=list(order=c(0,1,1),period=24))
#model_3_SARIMA_2 <- sarima(PAST_2.xts,4,0,1,0,1,1,24) #using SARIMA function
summary(model_3_SARIMA)
checkresiduals(model_3_SARIMA)


model_3
model_3_SARIMA

## MODEL#3 EVAL --> Past 4 Test --> fcp4_RMSE = 15.80907 #####

forecasted_past4 = sarima.for(PAST_4.xts,96,4,0,1,0,1,1,24)

forecasted_past4_df <- as.data.frame(forecasted_past4) #input forecasts to dataframe
#TEST1_df <- as.data.frame(TEST.xts) #input actuals to dataframe
names(forecasted_past4_df)[1] <- 'Predicted_Stress'
#names(TEST1_df)[1] <- 'Actual_Stress'
TEST1_PAST_4_comb<- cbind(forecasted_past4_df,TEST1_df)

fcp4_MSE <- mean((TEST1_PAST_4_comb$Predicted_Stress-TEST1_PAST_4_comb$Actual_Stress)^2) #calculate and display MSE in the testing set
fcp4_RMSE <- rmse(TEST1_PAST_4_comb$Actual_Stress, TEST1_PAST_4_comb$Predicted_Stress) #calculate and display RMSE 

TEST1_PAST_4_comb.xts <- as.xts(TEST1_PAST_4_comb)

myplot_test1 <- plot.xts(TEST1_PAST_4_comb.xts$Actual_Stress, main = "Actual vs Forecasted Stress", lwd = 3, grid.col = NA, col = "Blue")
lines(TEST1_PAST_4_comb.xts$Predicted_Stress, col = "Red", lwd = 3)

fcp4_RMSE






## MODEL#4 DEV --> Eight Week Training Window, Trained Model --> (0,1,3)(0,1,1)####

#Lambda <- BoxCox.lambda(PAST_8.xts)
#D=1,lambda = Lambda
model_4 <- auto.arima(PAST_8.xts,max.order = 20,seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
measures(ts_ts(PAST_8.xts))

checkresiduals(model_4) 
summary(model_4)

model_4_SARIMA <- Arima(PAST_8.xts, order=c(0,1,3), seasonal=list(order=c(0,1,1),period=24))
#model_3_SARIMA_2 <- sarima(PAST_2.xts,4,0,1,0,1,1,24) #using SARIMA function
summary(model_4_SARIMA)
checkresiduals(model_4_SARIMA)


model_4
model_4_SARIMA

## MODEL#4 EVAL --> Past 8 Test --> fcp8_RMSE = 15.77468 #####

forecasted_past8 = sarima.for(PAST_8.xts,96,1,0,3,0,1,1,24)

forecasted_past8_df <- as.data.frame(forecasted_past8) #input forecasts to dataframe
#TEST1_df <- as.data.frame(TEST.xts) #input actuals to dataframe
names(forecasted_past8_df)[1] <- 'Predicted_Stress'
#names(TEST1_df)[1] <- 'Actual_Stress'
TEST1_PAST_8_comb<- cbind(forecasted_past8_df,TEST1_df)

fcp8_MSE <- mean((TEST1_PAST_8_comb$Predicted_Stress-TEST1_PAST_8_comb$Actual_Stress)^2) #calculate and display MSE in the testing set
fcp8_RMSE <- rmse(TEST1_PAST_8_comb$Actual_Stress, TEST1_PAST_8_comb$Predicted_Stress) #calculate and display RMSE 

TEST1_PAST_8_comb.xts <- as.xts(TEST1_PAST_8_comb)

myplot_test1 <- plot.xts(TEST1_PAST_8_comb.xts$Actual_Stress, main = "Actual vs Forecasted Stress", lwd = 3, grid.col = NA, col = "Blue")
lines(TEST1_PAST_8_comb.xts$Predicted_Stress, col = "Red", lwd = 3)

fcp8_RMSE


## MODEL#5 DEV --> Twelve  Week Training Window, Trained Model --> (2,0,4)(0,1,1) ####

#Lambda <- BoxCox.lambda(PAST_8.xts)
#D=1,lambda = Lambda
model_5 <- auto.arima(PAST_12.xts,max.order = 20,seasonal= TRUE,approximation = FALSE,trace=TRUE, stepwise = FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
measures(ts_ts(PAST_12.xts))

checkresiduals(model_5) 
summary(model_5)

model_4_SARIMA <- Arima(PAST_8.xts, order=c(2,0,4), seasonal=list(order=c(0,1,1),period=24))
#model_3_SARIMA_2 <- sarima(PAST_2.xts,4,0,1,0,1,1,24) #using SARIMA function
summary(model_4_SARIMA)
checkresiduals(model_4_SARIMA)

## MODEL#5 EVAL --> Past 12 Test --> fcp12_RMSE =  15.45822 #####

forecasted_past12 = sarima.for(PAST_12.xts,96,2,0,4,0,1,1,24)

forecasted_past12_df <- as.data.frame(forecasted_past12) #input forecasts to dataframe
#TEST1_df <- as.data.frame(TEST.xts) #input actuals to dataframe
names(forecasted_past12_df)[1] <- 'Predicted_Stress'
#names(TEST1_df)[1] <- 'Actual_Stress'
TEST1_PAST_12_comb<- cbind(forecasted_past12_df,TEST1_df)

fcp12_MSE <- mean((TEST1_PAST_12_comb$Predicted_Stress-TEST1_PAST_12_comb$Actual_Stress)^2) #calculate and display MSE in the testing set
fcp12_RMSE <- rmse(TEST1_PAST_12_comb$Actual_Stress, TEST1_PAST_12_comb$Predicted_Stress) #calculate and display RMSE 

TEST1_PAST_12_comb.xts <- as.xts(TEST1_PAST_12_comb)

myplot_test1 <- plot.xts(TEST1_PAST_12_comb.xts$Actual_Stress, main = "Actual vs Forecasted Stress", lwd = 3, grid.col = NA, col = "Blue")
lines(TEST1_PAST_12_comb.xts$Predicted_Stress, col = "Red", lwd = 3)
geom_ribbon(aes(ymin=TEST1_PAST_12_comb.xts$Predicted_Stress-TEST1_PAST_12_comb.xts$se,
                ymax=TEST1_PAST_12_comb.xts$Predicted_Stress+TEST1_PAST_12_comb.xts$se),fill="grey70") + geom_line()

fcp12_RMSE










#### STEP_11 - NAIVE VS SARIMA Benchmarking & Performance ####

#Hist period --> first recorded 4 weeks --> ["2022-06-06/2022-07-03"]
library(forecast)
library(MLmetrics)

hist_data <- stress.xts["2022-06-06/2022-07-03"]; hist_data.ts = ts_ts(hist_data); hist_data.ts = ts(hist_data.ts, frequency = 24)

#1-6 DAYS PREDICTION WINDOWS
actual_1_day <- stress.xts["2022-07-04"] ; names(actual_1_day)[1] <- 'Actual'; actual_1_day.df <- as.data.frame(actual_1_day);actual_1_day.ts = ts_ts(actual_1_day); actual_1_day.ts = ts(actual_1_day.ts, frequency = 24)
actual_2_day <- stress.xts["2022-07-04/2022-07-05"] ; names(actual_2_day)[1] <- 'Actual'; actual_2_day.df <- as.data.frame(actual_2_day);actual_2_day.ts = ts_ts(actual_2_day); actual_2_day.ts = ts(actual_2_day.ts, frequency = 24)
actual_3_day <- stress.xts["2022-07-04/2022-07-06"] ; names(actual_3_day)[1] <- 'Actual'; actual_3_day.df <- as.data.frame(actual_3_day);actual_3_day.ts = ts_ts(actual_3_day); actual_3_day.ts = ts(actual_3_day.ts, frequency = 24)
actual_4_day <- stress.xts["2022-07-04/2022-07-07"] ; names(actual_4_day)[1] <- 'Actual'; actual_4_day.df <- as.data.frame(actual_4_day);actual_4_day.ts = ts_ts(actual_4_day); actual_4_day.ts = ts(actual_4_day.ts, frequency = 24)
actual_5_day <- stress.xts["2022-07-04/2022-07-08"] ; names(actual_5_day)[1] <- 'Actual'; actual_5_day.df <- as.data.frame(actual_5_day);actual_5_day.ts = ts_ts(actual_5_day); actual_5_day.ts = ts(actual_5_day.ts, frequency = 24)
actual_6_day <- stress.xts["2022-07-04/2022-07-09"] ; names(actual_6_day)[1] <- 'Actual'; actual_6_day.df <- as.data.frame(actual_6_day);actual_6_day.ts = ts_ts(actual_6_day); actual_6_day.ts = ts(actual_6_day.ts, frequency = 24)



#WEEKLY PREDICTION WINDOWS
actual_1 <- stress.xts["2022-07-04/2022-07-10"] ; names(actual_1)[1] <- 'Actual'; actual_1.df <- as.data.frame(actual_1);actual_1.ts = ts_ts(actual_1); actual_1.ts = ts(actual_1.ts, frequency = 24)
actual_2 <- stress.xts["2022-07-04/2022-07-17"] ; names(actual_2)[1] <- 'Actual'; actual_2.df <- as.data.frame(actual_2); actual_2.ts = ts_ts(actual_2); actual_2.ts = ts(actual_2.ts, frequency = 24)
actual_4 <- stress.xts["2022-07-04/2022-07-31"] ; names(actual_4)[1] <- 'Actual'; actual_4.df <- as.data.frame(actual_4); actual_4.ts = ts_ts(actual_4); actual_4.ts = ts(actual_4.ts, frequency = 24)
actual_6 <- stress.xts["2022-07-04/2022-08-14"] ; names(actual_6)[1] <- 'Actual'; actual_6.df <- as.data.frame(actual_6); actual_6.ts = ts_ts(actual_6); actual_6.ts = ts(actual_6.ts, frequency = 24)
actual_8 <- stress.xts["2022-07-11/2022-09-04"] ; names(actual_8)[1] <- 'Actual'; actual_8.df <- as.data.frame(actual_8); actual_8.ts = ts_ts(actual_8); actual_8.ts = ts(actual_8.ts, frequency = 24)

model_BM <- Arima(hist_data, order=c(4,0,1), seasonal=list(order=c(0,1,1),period=24))
summary(model_BM)
checkresiduals(model_BM)


#### 1 DAY PREDICTION from 1 month history ####

seasonal.naive_11day <- snaive(hist_data.ts,h=length(actual_1_day.ts))

forecasted_SAR_11day= as.data.frame(sarima.for(hist_data,24,4,0,1,0,1,1,24)); names(forecasted_SAR_11day)[1] <- 'Pred_SAR'
forecasted_NAIVE_11day <- as.data.frame(seasonal.naive_11day$mean); names(forecasted_NAIVE_11day)[1] <- 'Pred_Naive'

test_11day_combo <- cbind(actual_1_day.df,forecasted_NAIVE_11day, forecasted_SAR_11day)

SAR_11day_MSE <- mean((test_11day_combo$Pred_SAR-test_11day_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_11day_RMSE <- rmse(test_11day_combo$Actual, test_11day_combo$Pred_SAR) #calculate and display RMSE 
SAR_11day_MAPE <- mape(test_11day_combo$Actual, test_11day_combo$Pred_SAR)*100

NAIVE_11day_MSE <- mean((test_11day_combo$Pred_Naive-test_11day_combo$Actual)^2) 
NAIVE_11day_RMSE <-rmse(test_11day_combo$Actual, test_11day_combo$Pred_Naive)
NAIVE_11day_MAPE <-mape(test_11day_combo$Actual, test_11day_combo$Pred_Naive)*100

#### 2 DAY PREDICTION from 1 month history ####

seasonal.naive_12day <- snaive(hist_data.ts,h=length(actual_2_day.ts))

forecasted_SAR_12day= as.data.frame(sarima.for(hist_data,48,4,0,1,0,1,1,24)); names(forecasted_SAR_12day)[1] <- 'Pred_SAR'
forecasted_NAIVE_12day <- as.data.frame(seasonal.naive_12day$mean); names(forecasted_NAIVE_12day)[1] <- 'Pred_Naive'

test_12day_combo <- cbind(actual_2_day.df,forecasted_NAIVE_12day, forecasted_SAR_12day)

SAR_12day_MSE <- mean((test_12day_combo$Pred_SAR-test_12day_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_12day_RMSE <- rmse(test_12day_combo$Actual, test_12day_combo$Pred_SAR) #calculate and display RMSE 
SAR_12day_MAPE <- mape(test_12day_combo$Actual, test_12day_combo$Pred_SAR)*100

NAIVE_12day_MSE <- mean((test_12day_combo$Pred_Naive-test_12day_combo$Actual)^2) 
NAIVE_12day_RMSE <-rmse(test_12day_combo$Actual, test_12day_combo$Pred_Naive)
NAIVE_12day_MAPE <-mape(test_12day_combo$Actual, test_12day_combo$Pred_Naive)*100


#### 3 DAY PREDICTION from 1 month history ####

seasonal.naive_13day <- snaive(hist_data.ts,h=length(actual_3_day.ts))

forecasted_SAR_13day= as.data.frame(sarima.for(hist_data,72,4,0,1,0,1,1,24)); names(forecasted_SAR_13day)[1] <- 'Pred_SAR'
forecasted_NAIVE_13day <- as.data.frame(seasonal.naive_13day$mean); names(forecasted_NAIVE_13day)[1] <- 'Pred_Naive'

test_13day_combo <- cbind(actual_3_day.df,forecasted_NAIVE_13day, forecasted_SAR_13day)

SAR_13day_MSE <- mean((test_13day_combo$Pred_SAR-test_13day_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_13day_RMSE <- rmse(test_13day_combo$Actual, test_13day_combo$Pred_SAR) #calculate and display RMSE 
SAR_13day_MAPE <- mape(test_13day_combo$Actual, test_13day_combo$Pred_SAR)*100

NAIVE_13day_MSE <- mean((test_13day_combo$Pred_Naive-test_13day_combo$Actual)^2) 
NAIVE_13day_RMSE <-rmse(test_13day_combo$Actual, test_13day_combo$Pred_Naive)
NAIVE_13day_MAPE <-mape(test_13day_combo$Actual, test_13day_combo$Pred_Naive)*100

#### 4 DAY PREDICTION from 1 month history ####
seasonal.naive_14day <- snaive(hist_data.ts,h=length(actual_4_day.ts))

forecasted_SAR_14day= as.data.frame(sarima.for(hist_data,96,4,0,1,0,1,1,24)); names(forecasted_SAR_14day)[1] <- 'Pred_SAR'
forecasted_NAIVE_14day <- as.data.frame(seasonal.naive_14day$mean); names(forecasted_NAIVE_14day)[1] <- 'Pred_Naive'

test_14day_combo <- cbind(actual_4_day.df,forecasted_NAIVE_14day, forecasted_SAR_14day)

SAR_14day_MSE <- mean((test_14day_combo$Pred_SAR-test_14day_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_14day_RMSE <- rmse(test_14day_combo$Actual, test_14day_combo$Pred_SAR) #calculate and display RMSE 
SAR_14day_MAPE <- mape(test_14day_combo$Actual, test_14day_combo$Pred_SAR)*100

NAIVE_14day_MSE <- mean((test_14day_combo$Pred_Naive-test_14day_combo$Actual)^2) 
NAIVE_14day_RMSE <-rmse(test_14day_combo$Actual, test_14day_combo$Pred_Naive)
NAIVE_14day_MAPE <-mape(test_14day_combo$Actual, test_14day_combo$Pred_Naive)*100

#### 5 DAY PREDICTION from 1 month history ####
seasonal.naive_15day <- snaive(hist_data.ts,h=length(actual_5_day.ts))

forecasted_SAR_15day= as.data.frame(sarima.for(hist_data,120,4,0,1,0,1,1,24)); names(forecasted_SAR_15day)[1] <- 'Pred_SAR'
forecasted_NAIVE_15day <- as.data.frame(seasonal.naive_15day$mean); names(forecasted_NAIVE_15day)[1] <- 'Pred_Naive'

test_15day_combo <- cbind(actual_5_day.df,forecasted_NAIVE_15day, forecasted_SAR_15day)

SAR_15day_MSE <- mean((test_15day_combo$Pred_SAR-test_15day_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_15day_RMSE <- rmse(test_15day_combo$Actual, test_15day_combo$Pred_SAR) #calculate and display RMSE 
SAR_15day_MAPE <- mape(test_15day_combo$Actual, test_15day_combo$Pred_SAR)*100

NAIVE_15day_MSE <- mean((test_15day_combo$Pred_Naive-test_15day_combo$Actual)^2) 
NAIVE_15day_RMSE <-rmse(test_15day_combo$Actual, test_15day_combo$Pred_Naive)
NAIVE_15day_MAPE <-mape(test_15day_combo$Actual, test_15day_combo$Pred_Naive)*100

#### 6 DAY PREDICTION from 1 month history ####

seasonal.naive_16day <- snaive(hist_data.ts,h=length(actual_6_day.ts))

forecasted_SAR_16day= as.data.frame(sarima.for(hist_data,144,4,0,1,0,1,1,24)); names(forecasted_SAR_16day)[1] <- 'Pred_SAR'
forecasted_NAIVE_16day <- as.data.frame(seasonal.naive_16day$mean); names(forecasted_NAIVE_16day)[1] <- 'Pred_Naive'

test_16day_combo <- cbind(actual_6_day.df,forecasted_NAIVE_16day, forecasted_SAR_16day)

SAR_16day_MSE <- mean((test_16day_combo$Pred_SAR-test_16day_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_16day_RMSE <- rmse(test_16day_combo$Actual, test_16day_combo$Pred_SAR) #calculate and display RMSE 
SAR_16day_MAPE <- mape(test_16day_combo$Actual, test_16day_combo$Pred_SAR)*100

NAIVE_16day_MSE <- mean((test_16day_combo$Pred_Naive-test_16day_combo$Actual)^2) 
NAIVE_16day_RMSE <-rmse(test_16day_combo$Actual, test_16day_combo$Pred_Naive)
NAIVE_16day_MAPE <-mape(test_16day_combo$Actual, test_16day_combo$Pred_Naive)*100



#### 1 WEEK PREDICTION from 1 month history - ****USE THIS FOR PRESENTATION**** ####
#
SAR_11_MODEL <- Arima(hist_data, order=c(4,0,1), seasonal=list(order=c(0,1,1),period=24))
#model_3_SARIMA_2 <- sarima(PAST_2.xts,4,0,1,0,1,1,24) #using SARIMA function
summary(SAR_11_MODEL)
checkresiduals(SAR_11_MODEL)
#

seasonal.naive_11 <- snaive(hist_data.ts,h=length(actual_1.ts))

forecasted_SAR_11= as.data.frame(sarima.for(hist_data,168,4,0,1,0,1,1,24)); names(forecasted_SAR_11)[1] <- 'Pred_SAR'
forecasted_NAIVE_11 <- as.data.frame(seasonal.naive_11$mean); names(forecasted_NAIVE_11)[1] <- 'Pred_Naive'

test_11_combo <- cbind(actual_1.df,forecasted_NAIVE_11, forecasted_SAR_11)

SAR_11_MSE <- mean((test_11_combo$Pred_SAR-test_11_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_11_RMSE <- rmse(test_11_combo$Actual, test_11_combo$Pred_SAR) #calculate and display RMSE 
SAR_11_MAPE <- mape(test_11_combo$Actual, test_11_combo$Pred_SAR)*100

NAIVE_11_MSE <- mean((test_11_combo$Pred_Naive-test_11_combo$Actual)^2) 
NAIVE_11_RMSE <-rmse(test_11_combo$Actual, test_11_combo$Pred_Naive)
NAIVE_11_MAPE <-mape(test_11_combo$Actual, test_11_combo$Pred_Naive)*100


#### 2 WEEK PREDICTION from 1 month history ####
seasonal.naive_12 <- snaive(hist_data.ts,h=length(actual_2.ts))

forecasted_SAR_12= as.data.frame(sarima.for(hist_data,336,4,0,1,0,1,1,24)); names(forecasted_SAR_12)[1] <- 'Pred_SAR'
forecasted_NAIVE_12 <- as.data.frame(seasonal.naive_12$mean); names(forecasted_NAIVE_12)[1] <- 'Pred_Naive'

test_12_combo = cbind(actual_2.df,forecasted_NAIVE_12, forecasted_SAR_12)

SAR_12_MSE <- mean((test_12_combo$Pred_SAR-test_12_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_12_RMSE <- rmse(test_12_combo$Actual, test_12_combo$Pred_SAR) #calculate and display RMSE 
SAR_12_MAPE <- mape(test_12_combo$Actual, test_12_combo$Pred_SAR)*100

NAIVE_12_MSE <- mean((test_12_combo$Pred_Naive-test_12_combo$Actual)^2) 
NAIVE_12_RMSE <-rmse(test_12_combo$Actual, test_12_combo$Pred_Naive)
NAIVE_12_MAPE <-mape(test_12_combo$Actual, test_12_combo$Pred_Naive)*100




#### 4 WEEK PREDICTION from 1 month history ####

seasonal.naive_14 <- snaive(hist_data.ts,h=length(actual_4.ts))

forecasted_SAR_14= as.data.frame(sarima.for(hist_data,672,4,0,1,0,1,1,24)); names(forecasted_SAR_14)[1] <- 'Pred_SAR'
forecasted_NAIVE_14 <- as.data.frame(seasonal.naive_14$mean); names(forecasted_NAIVE_14)[1] <- 'Pred_Naive'

test_14_combo = cbind(actual_4.df,forecasted_NAIVE_14, forecasted_SAR_14)

SAR_14_MSE <- mean((test_14_combo$Pred_SAR-test_14_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_14_RMSE <- rmse(test_14_combo$Actual, test_14_combo$Pred_SAR) #calculate and display RMSE 
SAR_14_MAPE <- mape(test_14_combo$Actual, test_14_combo$Pred_SAR)*100

NAIVE_14_MSE <- mean((test_14_combo$Pred_Naive-test_14_combo$Actual)^2) 
NAIVE_14_RMSE <-rmse(test_14_combo$Actual, test_14_combo$Pred_Naive)
NAIVE_14_MAPE <-mape(test_14_combo$Actual, test_14_combo$Pred_Naive)*100

#### 6 WEEK PREDICTION from 1 month history ####

seasonal.naive_16 <- snaive(hist_data.ts,h=length(actual_6.ts))

forecasted_SAR_16= as.data.frame(sarima.for(hist_data,1008,4,0,1,0,1,1,24)); names(forecasted_SAR_16)[1] <- 'Pred_SAR'
forecasted_NAIVE_16 <- as.data.frame(seasonal.naive_16$mean); names(forecasted_NAIVE_16)[1] <- 'Pred_Naive'

test_16_combo = cbind(actual_6.df,forecasted_NAIVE_16, forecasted_SAR_16)

SAR_16_MSE <- mean((test_16_combo$Pred_SAR-test_16_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_16_RMSE <- rmse(test_16_combo$Actual, test_16_combo$Pred_SAR) #calculate and display RMSE 
SAR_16_MAPE <- mape(test_16_combo$Actual, test_16_combo$Pred_SAR)*100

NAIVE_16_MSE <- mean((test_16_combo$Pred_Naive-test_16_combo$Actual)^2) 
NAIVE_16_RMSE <-rmse(test_16_combo$Actual, test_16_combo$Pred_Naive)
NAIVE_16_MAPE <-mape(test_16_combo$Actual, test_16_combo$Pred_Naive)*100

#### 8 WEEK PREDICTION from 1 month history ####
seasonal.naive_18 <- snaive(hist_data.ts,h=length(actual_8.ts))

forecasted_SAR_18= as.data.frame(sarima.for(hist_data,1344,4,0,1,0,1,1,24)); names(forecasted_SAR_18)[1] <- 'Pred_SAR'
forecasted_NAIVE_18 <- as.data.frame(seasonal.naive_18$mean); names(forecasted_NAIVE_18)[1] <- 'Pred_Naive'

test_18_combo = cbind(actual_8.df,forecasted_NAIVE_18, forecasted_SAR_18)

SAR_18_MSE <- mean((test_18_combo$Pred_SAR-test_18_combo$Actual)^2) #calculate and display MSE in the testing set
SAR_18_RMSE <- rmse(test_18_combo$Actual, test_18_combo$Pred_SAR) #calculate and display RMSE 
SAR_18_MAPE <- mape(test_18_combo$Actual, test_18_combo$Pred_SAR)*100

NAIVE_18_MSE <- mean((test_18_combo$Pred_Naive-test_18_combo$Actual)^2) 
NAIVE_18_RMSE <-rmse(test_18_combo$Actual, test_18_combo$Pred_Naive)
NAIVE_18_MAPE <-mape(test_18_combo$Actual, test_18_combo$Pred_Naive)*100

#


# 1 mon






####----> Extract Model Performance Results ####

#NAIVE Results
NAIVE_11day_RMSE; NAIVE_12day_RMSE; NAIVE_13day_RMSE; NAIVE_14day_RMSE; NAIVE_15day_RMSE; NAIVE_16day_RMSE;
NAIVE_11_RMSE; NAIVE_12_RMSE; NAIVE_14_RMSE; NAIVE_16_RMSE; NAIVE_18_RMSE;

SAR_11day_RMSE; SAR_12day_RMSE; SAR_13day_RMSE; SAR_14day_RMSE; SAR_15day_RMSE; SAR_16day_RMSE;
SAR_11_RMSE; SAR_12_RMSE; SAR_14_RMSE; SAR_16_RMSE; SAR_18_RMSE;


