# Chapter 7 Quantmod ------------------------------------------------------
library(quantmod)

# 7.1 Downloading data
getSymbols("AAPL")

Open <- Op(AAPL)   #Open Price
High <- Hi(AAPL)    # High price
Low <- Lo(AAPL)  # Low price
Close<- Cl(AAPL)   #Close Price
Volume <- Vo(AAPL)   #Volume
AdjClose <- Ad(AAPL) # Adjusted close

getSymbols("AAPL", from='2000-01-01',to='2015-09-25')

AAPL <- last(AAPL,'1 year')
AAPL <- first(AAPL,'3 years')

stocklist <- c("AAPL","GOOG")
getSymbols(stocklist)

WeekVoYa<- apply.weekly(Vo(AAPL),sum)
# sum from Monday to Friday
MonthVoYa <- apply.monthly(Vo(AAPL),sum)
# sum to month
QuarterVoYa <- apply.quarterly(Vo(AAPL),sum)
# sum to quarter
YearVoYa <- apply.yearly(Vo(AAPL),sum)
# sum to year

WeekAveVoClYa<- apply.weekly(Vo(AAPL),mean)

# 7.2 Charting
chartSeries(AAPL,
            type="line",
            subset='2020',
            theme=chartTheme('white'))

chartSeries(AAPL,
            type="bar",
            subset='2020-05::2020-06',
            theme=chartTheme('white'))

chartSeries(AAPL,
            type="candlesticks",
            subset='2020-06',
            theme=chartTheme('white'))

chartSeries(AAPL,
            type="auto",
            subset='2020-05-10::2020-05-30',
            theme=chartTheme('white'))

# 7.3 Technical Indicators
library(TTR)

sma <-SMA(Cl(AAPL),n=20)
ema <-EMA(Cl(AAPL),n=20)
bb <-BBands(Cl(AAPL),s.d=2)
M <- momentum(Cl(AAPL), n=2)
ROC <- ROC(Cl(AAPL),n=2)
macd <- MACD(Cl(AAPL), nFast=12, nSlow=26,
             nSig=9, maType=SMA)
rsi = RSI(Cl(AAPL), n=14)

# 7.4 Charting with Indicators
chartSeries(AAPL,
            #subset='2007-05::2009-01',
            theme=chartTheme('white'))
addSMA(n=30,on=1,col = "blue")
addSMA(n=200,on=1,col = "red")


chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addEMA(n=30,on=1,col = "blue")
addEMA(n=200,on=1,col = "red")

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addBBands(n=20,sd=2)

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addMomentum(n=1)

chartSeries(AAPL,
            subset='2007-05::2008-01',
            theme=chartTheme('white'))
addROC(n=7)

chartSeries(AAPL,
            subset='2007-05::2008-01',
            theme=chartTheme('white'))
addMACD(fast=12,slow=26,signal=9,type="EMA")

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addRSI(n=14,maType="EMA")

# Charting Custom TA
sma <- SMA(Cl(AAPL),n=14)
chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addTA(sma, on=1, col="red")

# 7.5 Charting Price changes of two stocks
getSymbols(c("AAPL", "GOOG", "MSFT"))
NS <- function(xdat) xdat / coredata(xdat)[1]
a <- NS(Cl(AAPL))-1
g <- NS(Cl(GOOG))-1
m <- NS(Cl(MSFT))-1
chartSeries(a,
            subset = '2007',
            theme=chartTheme('white'))
addTA(g, on=1, col="red", lty="dotted")
addTA(m, on=1, col="blue", lty="dashed")
