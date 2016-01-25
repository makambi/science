data_path <- "Desktop/DataAnalysis/Soft/time-series.csv"
data <- read.csv(data_path, header = TRUE, sep = '\t')
library(ggplot2)
library(zoo)
library(xts)
library(dplyr)
library(forecast)

# look like white noise
tsdisplay(data$Dat1)
dat1 = data$Dat1
dat1_ts <- ts(dat1)

dat1_mod_a <- auto.arima(dat1_ts)
# selected model ARIMA(0,0,0) proves time series to be white noise
summary(dat1_mod_a)

#forecast based on previous value of sequence + confidnce intervals
forecast_1a <- forecast(dat1_mod_a, 100)
plot(forecast_1a)

#non-stationar data with trend
tsdisplay(data$Dat2)
dat2 <- data$Dat2

#stationar(?) time series after getting difference for time series
dat2_diff <- diff(dat2)
tsdisplay(dat2_diff)

dat2_ts <- ts(dat2)

dat2_mod_a <- auto.arima(dat2_ts)
# selected movel ARIMA(1,1,2) with drift proves required diff
# all coef non zero - they are significant
# diff removed positive trend drift 0.0138
summary(dat2_mod_a)

#forecast shows positive trend
forecast_2a <- forecast(dat2_mod_a, 100)
plot(forecast_2a)

plot(ts(data$Dat2[1:600]))

plot(dat1_ts)
plot(dat2_ts)

stl(dat1_ts, s.window = 5)
help(stl)

# http://stats.stackexchange.com/questions/1207/period-detection-of-a-generic-time-series
find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}
find.freq(dat2)

fit <- stl(dat2, s.window = 5)
plot(fit)

plot(dat2_ts)
lines(ma(dat2_ts, order = 9), col="blue")

