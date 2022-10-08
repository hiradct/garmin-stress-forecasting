# Forecasting Garmin Stress Levels

## Introduction

The common saying, "Stress is a silent killer!" has become a proverbial phrase. As the late Hungarian-Canadian Hans Selye, the father of stress research, famously said, "It's not stress that kills us; it's our reaction to it." But what is stress? Simply put, stress is our body's reaction to a particular challenge. Small, short-term bursts of stress are beneficial for our health. In contrast, the lingering chronic types are harmful to our body and mind in the long term.

The good news is that stress management science and technology are rapidly evolving. With accessible heart rate monitors in smartwatches, we can now indirectly measure our body’s response to psychological stressors. Some companies, such as Garmin, are providing real-time Stress Scores to users based on their phycological measures. Would it be possible to use the same Stress Score data to forecast our stress values in future hours or days?'

The purpose of this study is to assess the forecastability of Garmin's Stress Score utilizing only historical values. The scope of the analysis was limited to my Garmin's Forerunner 245 datapoints over four months. Garmin is a wearable technology company that provides stress monitoring features in its latest watches. The feature gives users a 0-100 stress level and interpretation, such as "Rest" or "High" to inform users of their real-time stress levels.

## Data Extraction & Preparation

With Garmin’s robust API providing 34 different tracked metrics in a JSON format, the stress level data over the previous 90 days was extracted with python script and the raw formating was transformed into readable data frame format for further cleaning and exploration in R-Studio. 

A preliminary analysis was performed on the stress time-series plot to assess missing/noisy values. The visualization of the stress series for missing gaps was performed using the “nanviar” package functions in R. The left plot below shows the June-July period having the most missing values. The left plot shows the individual missing gaps in 85k+ datapoints across 14 weeks.

<img width="241" alt="image" src="https://user-images.githubusercontent.com/29240649/194685127-cbfdcccc-6b01-4fb5-baed-1655d932772c.png"> <img width="256" alt="image" src="https://user-images.githubusercontent.com/29240649/194685138-718d7cb6-b5f3-4ebb-9416-58022997c787.png">

The initial four weeks in May-June correspond to when the participant first purchased the Garmin 245 Forerunner watch. We did not want to utilize the stress values from this period as it signifies calibration values for establishing the participant’s baseline stress values. Hence, we removed the first four weeks as a baseline. Additionally, we replaced the default intra-hour values signifying buffering period where the watch assesses whether the fluctuating HRV is due to activities (i.e. walking) or actual psychological stress. We also removed the default intra-hour value “-1” and “-2” signifying estimation by Garmin.

<img width="436" alt="image" src="https://user-images.githubusercontent.com/29240649/194685182-8b7c76ae-a6ba-465a-999d-6c7b2dc15160.png">
 
We also realized that we had missing data over some periods when the study participant was not wearing the watch, and the health metrics were not recorded. We found that linear interpolation was the most effective method to account for missing data. Lastly, we resampled our data from 2 minutes intervals to hourly values by averaging them out to avoid imputing too many missing values. The following code snapshot shows the code used to perform the imputation and resampling. The two time-series plots below visualize the stress data after the mentioned clean-up and transformations.
 
<img width="428" alt="image" src="https://user-images.githubusercontent.com/29240649/194685193-bc6b02d7-f634-4f43-a5f9-bf2fd344450d.png">

<img width="485" alt="image" src="https://user-images.githubusercontent.com/29240649/194685197-8df7698e-f131-4fc6-b07f-5f7732d8d1d7.png">

<img width="253" alt="image" src="https://user-images.githubusercontent.com/29240649/194685203-3f641634-5e10-418b-8ec1-fa0462573cf3.png"> <img width="244" alt="image" src="https://user-images.githubusercontent.com/29240649/194685204-640f8bf0-1ae8-48d2-8516-b1364d164d79.png">


 ## Time Series Analysis
 
 ### Additive Decomposition – SLT Function
A crucial preparatory step to time-series modelling is decomposing the raw data into trend, seasonality and remainder components. We did this step to understand to what extent the historical stress scores follow a consistent, systematic path versus random fluctuations. We used the “Seasonal and Trend decomposition using Loess” method since it's more flexible in depicting fluctuating seasonality and trend over time and more accurate in estimating nonlinear relationships. The SLT was run on a four-week training set. The results plot shows that a large portion of stress data can be explained by its regular 24-hour pattern, also known as the seasonality component.

<img width="427" alt="image" src="https://user-images.githubusercontent.com/29240649/194685236-3349f3ff-2972-4f8d-9fea-d682e4159402.png">
<img width="311" alt="image" src="https://user-images.githubusercontent.com/29240649/194685237-704bcd6e-ba82-4974-9251-4b0645d97a42.png">

### Stationarity Test & Checking for Heteroskedasticity
To check for stationarity, we used Augmented Dicky-Fuller (ADF) Test. The results confirmed our suspicion that the series is stationary with a p-value less than 0.01. We chose to test for heteroskedasticity directly, rather than relying on visual inspection, to check for high-volatility or nonconstant variance in stress observations over time. 

We used a particular hypothesis test called the McLeod-Li test for assessing autocorrelation based on the squared data. The alternative hypothesis is that data has autoregressive conditional heteroskedasticity (ARCH). The result of the MLL-test p-value on 14-week data is far below the significance level for varying lags. Hence ARCH effects are present. However, when we perform the test on a two-week (14 days) sample (randomly selected), we see p-values above the 0.05 threshold, indicating no ARCH in the data.

McLeod-Li Test P-value Plot - 14-week Data - ARCH is present                                                                                              

<img width="244" alt="image" src="https://user-images.githubusercontent.com/29240649/194685344-b7534e42-ff9c-443a-b8f7-f3a580d7628b.png">
 
McLeod-Li Test P-value Plot - 4-week Data - Partial presence of ARCH

<img width="244" alt="image" src="https://user-images.githubusercontent.com/29240649/194685346-ae785386-5703-4844-b600-6bf61daab3ce.png">

McLeod-Li Test P-value Plot - 2-week Data - No ARCH is present

<img width="233" alt="image" src="https://user-images.githubusercontent.com/29240649/194685351-c8fc9e2b-9961-487d-a1fe-af885e47a259.png">

## Forecast Model Development & Evaluation

### Training 5 different SARIMA Models for Varying Training Sizes
In this step, we split the preprocessed series into a one-week test period and training sets with varying sizes. The following figure shows the design of data split between the five models.

<img width="324" alt="image" src="https://user-images.githubusercontent.com/29240649/194685403-9ab55dab-db53-4c79-8420-b03b8176496a.png">

We used the Auto-ARIMA function to find the optimal non-seasonal hyperparameters for each model. Then, we transformed the ARIMA models into Seasonal-ARIMA (SARIMA) by applying P= 0, D = 1, Q = 1, and seasonal intervals of 24 hours. These seasonal hyperparameters were derived from experimentation using the decomposition plots (see Exhibit B). 

We then examined the model fit by the AUC and BIC measures and tested the model on a 1-week validation set. Finally, we concluded that the two-week history (14 days, or 336 hour-observations) produced the lowest Absolute Percentage Error (MAPE) in testing and the most optimal fit from AUCs and BIC Measures – in comparison to the other four models.

<img width="496" alt="image" src="https://user-images.githubusercontent.com/29240649/194685410-9aff1266-0306-47aa-a8f3-ac773469de8b.png">

## B)	Final Selected SARIMA Models 

- SARIMA (4, 0, 1) (0 ,1, 1) 24
- 1-week Prediction from 4-week history

<img width="252" alt="image" src="https://user-images.githubusercontent.com/29240649/194685452-2afd9a69-dfaa-49ce-a1e7-bb0e48fb76cf.png"> <img width="250" alt="image" src="https://user-images.githubusercontent.com/29240649/194685459-01e37da7-d350-495a-b97c-848b03454f61.png">

### Evaluating Model Performance
We evaluated the final SARIMA model on various forecasting prediction windows – ranging from one day to 8 weeks. We also compared the performance measures at each test against a Naïve model. The naive model assumes that the hourly stress score is the same as it was 24hrs ago for the same hour. For comparison with a naïve model, we produced a SARIMA-Naïve RMSE ratio. The following figure shows the ratio of SARIMA over Naïve for each prediction window. Hence, we concluded that our SARIMA model could forecast the next 5 to 14 days' stress scores with ~20-25% less error (RMSE) than the Naive assumption using a training history of 14 days.
<img width="368" alt="image" src="https://user-images.githubusercontent.com/29240649/194685474-79e5ff3f-d053-4dfa-9869-ae26a86c30d2.png">



 
 










