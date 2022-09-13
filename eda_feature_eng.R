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
library(naniar)
library("imputeTS")

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
garmin_series_ff <- garmin_series_ff %>% replace_with_na(replace = list(stressLevel = c(-1,-2)))


#### STEP_3 - Visualize Missing Values/GAPS #####

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


#### STEP_5 - Split data into train & Test set####

# we have 10 weeks, use first 8 as the training and reserve last two for testing
unique(garmin_procd_inter$week)
# "23" "24" "25" "26" "27" "28" "29" "30" "32" "34"
train.ts = garmin_procd_inter[!(garmin_procd_inter$week %in% c('32','34')),]
test.ts = garmin_procd_inter[(garmin_procd_inter$week %in% c('32','34')),]


#### STEP_6 - Identify Time-Series Components####

#------------Visualize Decomposition---------#
library(forecast)
library(fpp)

plot.ts(train.ts$stressLevel)
plot.ts(train.ts$heartRate)

#---stress
#Decompose the Data Into Four Components on DAILY
stressdata <- ts(train.ts$stressLevel, frequency = 720) # freq is # of obs per unit of time, aka per day
st.decomp <- decompose(stressdata, "multiplicative")
plot(st.decomp)

#Decompose the Data Into Four Components on HALF-DAILY
stressdata <- ts(train.ts$stressLevel, frequency = 360) # freq is # of obs per unit of time, aka per day
st.decomp <- decompose(stressdata, "multiplicative")
plot(st.decomp)

#Decompose the Data Into Four Components on week
stressdata <- ts(train.ts$stressLevel, frequency = 3600) # freq is # of obs per unit of time, aka per day
st.decomp <- decompose(stressdata, "multiplicative")
plot(st.decomp)


#---heart rate
#Decompose the Data Into Four Components on DAILY
hrdata <- ts(train.ts$heartRate, frequency = 720) # freq is # of obs per unit of time, aka per day
hr.decomp <- decompose(hrdata, "multiplicative")
plot(hr.decomp)


#------------De-trending/ Stationary -----------#

## -----(1) Does the variance increases with mean (heteroscedasticity)?-----
#--> Visually HT can be seem when higher the value, more volatility, but we are NOT seeing that
#--> Statistical Testing -->  NO HETERO

#The McLeod-Li test is a version of the Ljung-Box test for autocorrelation based on the squared data.
#The alternative hypothesis is that the data have autoregressive conditional heteroscedasticity (ARCH).
library(TSA)
a =diff(log(train.ts$stressLevel))*100
McLeod.Li.test(y=a)

#-----OLD CODE-----
# stress.lambda <- BoxCox.lambda(train.ts$stressLevel)  # The transform is parameterized by lambda
# # lambda is usually chosen between -2 and 2
# stress.BoxCox<-BoxCox(train.ts$stressLevel, stress.lambda)
# 
# # Check the transformed data and compare it with log transform
# plot.ts(stress.BoxCox, xlab="Date", ylab="stress")
# plot.ts(log(train.ts$stressLevel), xlab="Date", ylab="stress")
# # visually we are not able to check for heterosk



##-----(2)  Does the data has cyclic pattern(seasonality)?-----

w1 = train.ts[train.ts$week == '23',]
w2 = train.ts[train.ts$week == '29',]
w3 = train.ts[train.ts$week == '30',]

# Autocorrelation & Partial-Autocorrelation 


#Visual Test (Global - WEEKLY)
plot.ts(w1$stressLevel) # we see a daily pattern repeated throughout the week
#Visual Test (Locally - Dayily)
d2 = train.ts[(train.ts$week == '29' & train.ts$day == 4),]
plot.ts(d2$stressLevel) # we dont see a patttern here

library(astsa)
trend(train.ts$stressLevel, lowess=TRUE)



#ACF - PER 2MIN
Acf(diff(w3$stressLevel,1),lag.max = 100)
Pacf(diff(w3$stressLevel,1),lag.max = 100)
#cut off at 15 due to big difference between 15 and 16
#hence we will set p = 15

train.ts_deSeasonality = diff(train.ts$stressLevel,15)
plot.ts(train.ts$stressLevel)
plot.ts(train.ts_deSeasonality)




#---->NOW WE TEST FOR STATIONARITY
# Augmented Dickey-Fuller (ADF) test to check stationarity. 
#The null hypothesis assumes that the series is non-stationary.
#p-value=0.01, rejects the null. the series is stationary
adf.test(train.ts_deSeasonality,alternative = "stationary")



### -----> TS TESTS & Treatment






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




#-------------Automatic ARIMA Modeling -------------------
# To begin, we use an automated algorithm to find a good model. However, there is no guarantee that it is the best model. So we treat it as a starting point. 
model.auto <- auto.arima(train.ts_deSeasonality, stepwise=FALSE, seasonal= TRUE,trace=TRUE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)

model.auto

# It suggests a ARIMA(1,1, 2) model with non-zero mean, AIC=258724.1
checkresiduals(model.auto)  # Check the quality of fit. Residuals should: 
# (1) not have any significant autocorrelation- maybe?
# (2) follow normal distribution-yes
# (3) have stable variance over time - yes

fit.yourself <- Arima(train.ts$stressLevel, order=c(1,0,4), seasonal=list(order=c(0,1,0),period=15)) #The no seasonal difference
fit.yourself

plot(forecast(auto.arima(train.ts_deSeasonality, stepwise=FALSE, seasonal= TRUE,trace=TRUE),h=3000))


plot(forecast(fit.yourself,1000) )

# Plot the forecasting in the original scale
fc<-forecast(fit.yourself,720)



fc$x <- exp(fc$x)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
autoplot(fc)

#-------------Improving the Automatically selected Model -------------------
# Note that the auto.arima function may not always find the model with the lowest AIC/AICc/BIC. 
# To improve the existing model, we may explore other models manually to see if one could yield a lower criterion than the existing model. For example: 
fit.alternative1 <- Arima(train.ts$stressLevel, order=c(4,0,2)) 
fit.alternative1
checkresiduals(fit.alternative1)
fc1<-forecast(fit.yourself,20)

fc1$x <- exp(fc1$x)
fc1$mean <- exp(fc1$mean)
fc1$lower <- exp(fc1$lower)
fc1$upper <- exp(fc1$upper)
autoplot(fc1)
####AIC not better than (2,0,2) 










#Create a Box Plot by Day

#Create a Box Plot by Week



#-----------Remove Seasonality through Seasonal Differencing ---------------
# To check the period of cyclic pattern, use the autocorrelation function 
Acf(diff(garmin_procd_inter$heartRate,1),lag.max =25) 
Acf(diff(garmin_procd_inter$stressLevel,1),lag.max =25) 



