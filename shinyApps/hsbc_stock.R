# plot the trend of HSBC stock
stock <- read.csv("HSBC.csv")
# head(stock)

stock <- stock[c("Date", "Close")]
stock$Date <- as.Date(stock$Date)
with(data = stock, 
     plot(Date, Close, type = "l", 
          main = "HSBC Stock",
          xlab = "12 Months",
          ylab = "$ Close"))

# forecast next 10 days price
library(fpp2)
library(dplyr)
arima.hsbc <- auto.arima(stock["Close"])
autoplot(forecast(arima.hsbc, h = 10))

autoplot(ts(stock["Close"]))

library(astsa)
sarima(ts(stock["Close"]), 0, 1, 0)
sarima.for(ts(stock["Close"]), 10, 0, 1, 0)

length(stock["Close"])
dim(stock)[1]
