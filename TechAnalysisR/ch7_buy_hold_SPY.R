# Non-Day Trading ---------------------------------------------------------

# https://kochiuyu.github.io/programming/r-programming/ta-with-r-ch7/
# Trading signal:
#   
#   Buy signal arises if 14-day RSI < 30
# 
#   Sell signal arises if 14-day RSI > 50
# 
# Trading Rule
# 
# Buy 300 units under buy signal
# 
# Sell all when sell signal appears
# 
# Initial wealth: 10,000
# 
# Note that we need to keep track of both cash and stock holdings.

library(quantmod)
library(PerformanceAnalytics)
getSymbols("SPY")

lineChart(window(SPY, start = "2021-01-01"))
addSMA(n=50,on=1,col = "blue")
addSMA(n=20,on=1,col = "red")
addSMA(n=200,on=1,col = "white")
addRSI(n=14, maType="EMA")
addMACD(fast=12,slow=26,signal=9,type="EMA")

qty <-10
day <-14

signal <- c()   #trade signal
signal[1:(day+1)] <- 0 

price <- window(Cl(SPY), start = "2021-01-01")

stock <- c()  #stock holding
stock[1:(day+1)] <-0

cash <-c()
cash[1:(day+1)] <- 10000  

# Trading signal is based on simple RSI:
rsi <- RSI(price, day)  #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (rsi[i] < 30){  #buy one more unit if rsi < 30
    signal[i] <- 1
  } else if (rsi[i] < 50){ #no change if rsi < 50
    signal[i] <- 0
  } else {         #sell  if rsi > 50
    signal[i] <- -1
  }
}

signal<-reclass(signal,price)

# Assume buying at closing price, keep track of how cash and stock changes:
trade <- Lag(signal)    #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (trade[i]>=0){
    stock[i] <- stock[i-1] + qty*trade[i]
    cash[i] <- cash[i-1] - 
      qty*trade[i]*price[i]
  } else{
    stock[i] <- 0
    cash[i] <- cash[i-1] + 
      stock[i-1]*price[i]
  }
}
stock<-reclass(stock,price)
cash<-reclass(cash,price)

# calculate equity using cash and stock holdings
equity <-c()
equity[1:(day+1)] <- 10000 

return<-c()                  
return[1:(day+1)] <- 0

for (i in (day+1): length(price)){
  equity[i] <- stock[i] * price[i] + cash[i]
  return[i] <- equity[i]/equity[i-1]-1
}

equity<-reclass(equity,price)
return<-reclass(return,price)

# Performance Charts
charts.PerformanceSummary(return, 
                          main="Non-Day-Trading")

# how is the performance of the strategy:
chart_Series(equity, main="equity line")

# cash account over time:
chart_Series(cash, name="Cash Holding")

# stock holdings:
chart_Series(stock, name="Stock Holding")

# return distributions
dret <- dailyReturn(window(SPY, start = "2022-01-01"))
hist(dret)
plot(dret)
UP = dret > 0

# percentage postive returns
sum(UP) / length(dret)

