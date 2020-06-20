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
tail(dret, n = 5)

# Add moving average 

lineChart(MSFT, subset = '2015::2020')
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")


# Add confidence bands 

lineChart(MSFT, subset = '2015::2020')
addBBands(n = 20, sd = 2)


# Add momentum trend 
lineChart(MSFT, subset = '2015::2020')
addMomentum(n = 1)


# Add ROC 
lineChart(MSFT, subset = '2015::2020')
addROC(n = 7)


# Add MACD 
lineChart(MSFT, subset = '2015::2020')
addMACD(fast = 12, slow = 26, signal = 9, type = "EMA")


# Add RSI 

lineChart(MSFT, subset = '2015::2020')
addRSI(n = 14, maType = "EMA")


# Setup -------------------------------------------------------------------

# naive method
price <- Cl(MSFT)
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

lineChart(MSFT, subset = '2020-02::2020-06')
addTA(signal, type = 'S', col = 'red')



# Performance evaluation --------------------------------------------------

# generate the daily gains and losses

# day trading based on yesterday's buy signal
# buy at open
# sell at close
# trading size - all in

trade <- Lag(signal, 1) # lag1 means yesterday's signal
ret1 <- dailyReturn(MSFT) * trade
names(ret1) <- "filter"

charts.PerformanceSummary(ret1, main = "naive rule")

# use SMA to generate both buy signal
price <- Cl(MSFT)
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

lineChart(MSFT, subset = '2020-02::2020-06')
addTA(signal, type = 'S', col = 'red')

trade <- Lag(signal, 1) # lag1 means yesterday's signal
ret <- dailyReturn(MSFT) * trade
names(ret) <- "SMA"

charts.PerformanceSummary(ret, main = "SMA")
