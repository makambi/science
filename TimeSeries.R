#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
#http://www.slideshare.net/rdatamining/time-series-analysis-and-mining-with-r


data_path <- "Desktop/DataAnalysis/Soft/time-series.csv"
data <- read.csv(data_path, header = TRUE, sep = '\t')

dat1 <- data[,c("Dat1")]
dat2 <-data[,c("Dat2")]
data <- data[,c("Dat1", "Dat2")]

#find actual number of periods (frequency)
dat1_ts <- ts(dat1, frequency = 12)
dat2_ts <- ts(dat2)
data_ts <- ts(data)

plot.ts(data_ts)

library("TTR")
dat1_sma25 <- SMA(dat1_ts, 5)

par(mfrow=c(2,1))
plot.ts(dat1_sma25)
plot.ts(dat1_ts)

#also check stl for decomposing
decompose(dat1_ts)


f <- decompose(data_ts[,c('N', 'Dat1')])

library("TTR")
dat1_sma <- SMA(data_ts[,'Dat1'],n=10)



plot.ts(dat1_sma)

decompose(dat1_sma)
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


#http://stackoverflow.com/questions/10302261/forecasting-time-series-data
library(forecast)


data <- xts(x=data$Dat1, order.by = data$dates)
data.ts <- ts(data, frequency = 12)
plot(forecast(ets(data.ts), 3))

