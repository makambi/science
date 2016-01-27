library(ggplot2)
library(zoo)
library(xts)
library(dplyr)
library(forecast)
library(tseries)

data_path <- "Desktop/DataAnalysis/Soft/time-series.csv"
data <- read.csv(data_path, header = TRUE, sep = '\t')

# look like white noise
tsdisplay(data$Dat1)
dat1 = data$Dat1
dat1_ts <- ts(dat1)

# Null hypothesys rejected, series is stationary
adf.test(data$Dat1, alternative = c("stationary"))

kpss.test(data$Dat1)

#Canova-Hansen test for seasonality
nsdiffs(dat1_ts, test="ch")

dat1_mod_a <- auto.arima(dat1_ts)
# selected model ARIMA(0,0,0) proves time series to be white noise
summary(dat1_mod_a)

#forecast based on previous value of sequence + confidnce intervals
forecast_1a <- forecast(dat1_mod_a, 100)
plot(forecast_1a)

dat1_diff <- diff(dat1)
tsdisplay(dat1_diff)
dat1_diff_ts <- ts(dat1_diff)
dat1_diff_mod_a <- auto.arima(dat1_diff_ts)

#Coefficients are not significant
summary(dat1_diff_mod_a)

forecast_diff_1a <- forecast(dat1_diff_mod_a, 100)
plot(forecast_diff_1a)

nn_fit_1 <- nnetar(dat1_ts)
plot(forecast(nn_fit_1, 200))

#non-stationar data with trend, random walk
tsdisplay(data$Dat2)
dat2 <- data$Dat2

#stationar(?) time series after getting difference for time series
dat2_diff <- diff(dat2)
tsdisplay(dat2_diff)

dat2_ts <- ts(dat2)

adf.test(dat2, alternative = c("stationary"))

kpss.test(dat2)

dat2_mod_a <- auto.arima(dat2_ts)
# selected movel ARIMA(1,1,2) with drift proves required diff
# all coef non zero - they are significant
# diff removed positive trend drift 0.0138
summary(dat2_mod_a)

#forecast shows positive trend
forecast_2a <- forecast(dat2_mod_a, 100)
plot(forecast_2a)

nn_fit <- nnetar(dat2)
plot(forecast(nn_fit, 200))
