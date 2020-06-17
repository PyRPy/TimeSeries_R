# 7.7 Evaluating Trading Rules --------------------------------------------
# Simple filter Buy
library(quantmod)
library(PerformanceAnalytics)
getSymbols("MSFT")

price <- Cl(MSFT) # close price
r <- price/Lag(price) - 1 # % price change
delta <-0.005 #threshold
signal <-c(0) # first date has no signal

#Loop over all trading days (except the first)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else
    signal[i]<- 0
}

# Assign time to action variable using reclass;
signal<-reclass(signal,price)

# Charting with Trading rule
chartSeries(MSFT,
            type = 'line',
            subset="2020-01::2020-06",
            theme=chartTheme('white'))
addTA(signal,type='S',col='red')

# consider trading based on yesterday indicator:
trade <- Lag(signal,1) # trade based on yesterday signal

# To keep it simple, we evaluate using day trading:
#   
# buy at open
# sell at close
# trading size: all in

ret<-dailyReturn(MSFT)*trade
names(ret)<-"filter"

#Performance Summary
charts.PerformanceSummary(ret, main="Naive Buy Rule")
zoomChart(subset = "2020-01::2020-06")


# 7.7.2 Simple fiter buy-sell ---------------------------------------------

price <- Cl(MSFT)
r <- price/Lag(price) - 1
delta<-0.005
signal <-c(NA) # first signal is NA

for (i in 2: length(Cl(MSFT))){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(MSFT))

trade1 <- Lag(signal)
ret1<-dailyReturn(MSFT)*trade1
names(ret1) <- 'Naive'
charts.PerformanceSummary(ret1)


# 7.7.3 An example using RSI ----------------------------------------------

# Consider following day-trading strategy based on 14-day RSI:
#   
# buy one unit if RSI <30 and
# otherwise no trade

day <-14
price <- Cl(MSFT)
signal <- c()                    #initialize vector
rsi <- RSI(price, day)     #rsi is the lag of RSI
signal[1:day+1] <- 0            #0 because no signal until day+1

for (i in (day+1): length(price)){
  if (rsi[i] < 30){             #buy if rsi < 30
    signal[i] <- 1
  }else {                       #no trade all if rsi > 30
    signal[i] <- 0
  }
}
signal<-reclass(signal,Cl(MSFT))
trade2 <- Lag(signal)

#construct a new variable ret1
ret1 <- dailyReturn(MSFT)*trade1
names(ret1) <- 'Naive'
# construct a new variable ret2
ret2 <- dailyReturn(MSFT)*trade2
names(ret2) <- 'RSI'

# compare strategies with the filter rule:
retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall, 
                          main="Naive v.s. RSI")

# A more efficient but less readable code is to avoid counting:

# for (i in 1:length(price)){
#   signal[i] <- 0
#   if (isTRUE(rsi[i] < 30)){             
#     signal[i] <- 1
#   }
# }