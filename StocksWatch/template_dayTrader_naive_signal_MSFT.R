# reference from youtube 
# https://www.youtube.com/channel/UC0Amh8UkHvIVxPbxsrEBRMg
# load packages
library(quantmod)
library(PerformanceAnalytics)
# Stock watch list --------------------------------------------------------

# a collection of stocks
mystocks <- c("GE", "JNJ", "MSFT", "WMT", "GS", "SBUX", "ZM", "^DJI")

# read data
getSymbols(mystocks, from = "2015-01-01", src = "yahoo")

# Daily return and charts -------------------------------------------------


dret <- dailyReturn(MSFT)
head(dret, n = 5)


# Add moving average 

lineChart(MSFT, subset = '2015::2020')
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")

lineChart(MSFT, subset = '2020')
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")

lineChart(MSFT, subset = '2020')
addSMA(n = 10, col = "blue")
addSMA(n = 50, col = "red")
addSMA(n = 200, col = "white")

lineChart(MSFT, subset = '2020-05::2020-06')
addSMA(n = 10, col = "blue")
addSMA(n = 50, col = "red")
addSMA(n = 200, col = "white")

# Setup -------------------------------------------------------------------

# naive method
price <- window(Cl(MSFT), start = "2020-05-01")
r <- price/Lag(price) - 1
delta <- 0.005 # threshold
signal <- c(0)


# Loop over all days

for (i in 2:length(price)) {
  if (r[i] > delta) {
    signal[i] <- 1
  } else
    signal[i] <- 0
}

# reclass signal
signal <- reclass(signal, price)


# Charting with trading rule ----------------------------------------------

lineChart(price)
addTA(signal, type = 'S', col = 'red')

# Performance evaluation --------------------------------------------------

# generate the daily gains and losses

# day trading based on yesterday's buy signal
# buy at open
# sell at close
# trading size - all in

trade <- Lag(signal, 1) # lag1 means yesterday's signal
ret1 <- dailyReturn(price) * trade
names(ret1) <- "filter"

charts.PerformanceSummary(ret1, main = "naive rule")

# use SMA to generate both buy signal
price <- window(Cl(MSFT), start = "2020-03-01")
S <- 10
L <- 50
r <- SMA(price, S) / SMA(price, L) - 1
delta <- 0.005 # threshold
signal <- c()
signal[1:L] <- 0

for (i in (L+1):length(price)) {
  if (r[i] > delta) {
    signal[i] <- 1
  } else
    signal[i] <- 0
}

signal <- reclass(signal, price)

lineChart(price)
addTA(signal, type = 'S', col = 'red')

trade <- Lag(signal, 1) # lag1 means yesterday's signal
ret <- dailyReturn(price) * trade
names(ret) <- "SMA"

charts.PerformanceSummary(ret, main = "SMA")
table.Stats(ret)
plot(ret)

lineChart(MSFT, subset = '2020-03::2020-06')
addSMA(n = 10, col = "blue")
addSMA(n = 50, col = "red")
# addSMA(n = 200, col = "white")
