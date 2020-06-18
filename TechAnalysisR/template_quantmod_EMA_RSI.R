# 7.8 Combining two indicators: EMA and RSI -------------------------------

library(quantmod)
library(PerformanceAnalytics)

getSymbols("MSFT", from = "2019-01-01")

# Test the following strategy using EMA and RSI based on day trading:
#   
# Buy signal based on EMA rule.
# Sell signal based on RSI rule.
# Tie-breaking: buy-signal has priority
# We use 14-day RSI and use 70 as threshold for selling.

n <- 14
delta<-0.005
price <- Cl(MSFT)         
r <- price/Lag(price) - 1    
rsi <- RSI(price, n) 
signal <-c()    # first signal is NA
signal[1:n] <-0


# Generate Trading Signal
for (i in (n+1):length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (rsi[i] > 70){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,price)

# Apply Trading Rule
trade3 <- Lag(signal)
ret3<-dailyReturn(MSFT)*trade3 
names(ret3) <- 'Combine'
retall <- cbind(ret3)

charts.PerformanceSummary(
  retall, main="RSI",
  colorset=bluefocus)
