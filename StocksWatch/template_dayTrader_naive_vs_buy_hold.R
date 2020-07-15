# reference from youtube 
# https://www.youtube.com/channel/UC0Amh8UkHvIVxPbxsrEBRMg
# load packages
library(quantmod)
library(PerformanceAnalytics)
# Stock watch list --------------------------------------------------------

# a collection of stocks
mystocks <- c("MSFT", "ZM")

# read data
getSymbols(mystocks, from = "2020-01-01", src = "yahoo")

# Daily return and charts -------------------------------------------------

dret <- dailyReturn(MSFT)
head(dret, n = 5)
tail(dret, n = 5)
plot(window(dret, start = "2020-01-01"))
sd(window(dret, start = "2020-01-01"))
sd(dret)

# Add moving average 
candleChart(MSFT, subset = "2020") # 
addSMA(n = 10, col = "white")
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")
addRSI(n = 14, maType = "EMA")
addBBands(n = 20, sd = 2)
addMACD(fast = 12, slow = 26, signal = 9, type = "EMA")
lineChart(MSFT)


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

candleChart(MSFT)
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

candleChart(MSFT)
addTA(signal, type = 'S', col = 'red')

trade <- Lag(signal, 1) # lag1 means yesterday's signal
ret <- dailyReturn(MSFT) * trade
names(ret) <- "SMA"

charts.PerformanceSummary(ret, main = "SMA")
