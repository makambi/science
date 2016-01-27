#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
#http://www.slideshare.net/rdatamining/time-series-analysis-and-mining-with-r

#http://habrahabr.ru/post/207160/
#https://habrahabr.ru/post/210530/

#https://onlinecourses.science.psu.edu/stat510/?q=node/67

#http://stackoverflow.com/questions/10302261/forecasting-time-series-data

#Forecasting: principles and practice » ARIMA models
#https://www.otexts.org/fpp/8/1  


data_path <- "Desktop/DataAnalysis/Soft/time-series.csv"
data <- read.csv(data_path, header = TRUE, sep = '\t')

dat1 <- data[,c("Dat1")]
dat2 <-data[,c("Dat2")]
data <- data[,c("Dat1", "Dat2")]

#find actual number of periods (frequency)
dat1_ts <- ts(dat1) #, frequency = 12)
dat2_ts <- ts(dat2)
data_ts <- ts(data)

plot.ts(data_ts)

par(mfrow=c(1,1))
hist(data$Dat1)

hist(data$Dat2)

library("TTR")
dat1_sma25 <- SMA(dat1_ts, 5)

par(mfrow=c(2,1))
plot.ts(dat1_sma25)
plot.ts(dat1_ts)

#also check stl for decomposing
dat1_components <- decompose(dat1_ts)

plot(dat1_components)

#Seasonally Adjusting
components_adjusted <- dat1_ts - dat1_components$seasonal

par(mfrow=c(2,1))
plot.ts(components_adjusted)
plot.ts(dat1_ts)


library(stats)

#the ACF plot is also useful for identifying non-stationary time series

#For a stationary time series, the ACF will drop to zero relatively quickly
acf(dat2_ts)

#he ACF of non-stationary data decreases slowly
acf(dat1_ts)


#http://www.r-bloggers.com/unit-root-tests/
library(tseries)

#http://stats.stackexchange.com/questions/27332/how-to-know-if-a-time-series-is-stationary-or-non-stationary

# The null-hypothesis for an ADF test is that the data are non-stationary. So large p-values are indicative of non-stationarity, 
# and small p-values suggest stationarity. Using the usual 5% threshold, differencing is required if the p-value is greater than 0.05.
adf.test(dat1_ts, alternative = "stationary")
adf.test(dat2_ts, alternative = "stationary")

kpss.test(dat1_ts)
kpss.test(dat2_ts)


fit <- auto.arima(dat1_ts,seasonal=FALSE,stationary = TRUE)
fit
plot(forecast(fit,h=50),include=200)

fit <- auto.arima(dat1_ts,stepwise=FALSE, approximation=FALSE)
fit
plot(forecast(fit,h=50),include=200)

par(mfrow=c(1,2))
Acf(dat1_ts,main="ACF")
Pacf(dat1_ts,main="PACF")


plot(dat2_ts)
lg_dat2_ts <- log(dat2_ts)
plot(lg_dat2_ts)


tsdisplay(diff(lg_dat2_ts,12),
          main="Seasonally differenced H02 scripts", xlab="Year")

par(mfrow=c(2,1))
plot(dat2_ts, ylab="Data 2", xlab="Observation")
plot(lg_dat2_ts, ylab="Log Data 2", xlab="Observation")

x <- xts(x=data$Dat1, order.by = as.Date(seq(1, nrow(data))))
x.ts = ts(x, freq=365)
plot(forecast(ets(x.ts), 10))

library(quantmod)
periodicity(data_ts)

help("periodicity")

data$dates = seq(as.Date("1970/01/01"), by = "day", length.out = nrow(data))
data_ts <- ts(data[,c("Dat1", "Dat2", "dates")], frequency = 12)
data_sma <- SMA(data_ts)
plot.ts(data_sma)
components <- decompose(data_sma)

plot(components)

plot.ts(data_sma)



library(forecast)


data <- xts(x=data$Dat1, order.by = data$dates)
data.ts <- ts(data, frequency = 12)
plot(forecast(ets(data.ts), 3))

