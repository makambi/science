#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
#http://www.slideshare.net/rdatamining/time-series-analysis-and-mining-with-r

#http://habrahabr.ru/post/207160/
#https://habrahabr.ru/post/210530/

#https://onlinecourses.science.psu.edu/stat510/?q=node/67

#http://stackoverflow.com/questions/10302261/forecasting-time-series-data

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

