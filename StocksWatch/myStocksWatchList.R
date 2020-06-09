# load packages
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(fpp2)


# Stock watch list --------------------------------------------------------

# a collection of stocks
mystocks <- c("GE", "JNJ", "MSFT", "WMT", "GS", "SBUX", "ZM", "^DJI")

# read data
getSymbols(mystocks, src = "yahoo")

# plot the stocks in observation
autoplot(window(DJI$DJI.Close, start = "2019-01-01"))
autoplot(window(DJI$DJI.Close, start = "2020-01-01"))

autoplot(window(GE$GE.Close, start = "2019-01-01"))

autoplot(window(JNJ$JNJ.Close, start = "2019-01-01"))
autoplot(window(JNJ$JNJ.Close, start = "2020-01-01"))

autoplot(window(MSFT$MSFT.Close, start = "2019-01-01"))

autoplot(window(WMT$WMT.Close, start = "2019-01-01"))
autoplot(window(WMT$WMT.Close, start = "2020-01-01"))

autoplot(window(GS$GS.Close, start = "2019-01-01"))

autoplot(window(SBUX$SBUX.Close, start = "2019-01-01"))
autoplot(window(SBUX$SBUX.Close, start = "2020-05-01"))

autoplot(window(ZM$ZM.Close, start = "2019-01-01"))
autoplot(window(ZM$ZM.Close, start = "2020-01-01"))


# Moving average indicators -----------------------------------------------
# use WMT as an example 
sma200 <- SMA(x = Cl(WMT), n = 200)
sma50 <- SMA(x = Cl(WMT), n = 50)
plot(window(Cl(WMT), start = "2015-01-01"))
lines(window(sma200, start = "2015-01-01"), col = "red")
lines(window(sma50, start = "2015-01-01", col = "blue"))


sma200 <- SMA(x = Cl(GE), n = 200)
sma50 <- SMA(x = Cl(GE), n = 50)
plot(window(Cl(GE), start = "2015-01-01"))
lines(window(sma200, start = "2015-01-01"), col = "red")
lines(window(sma50, start = "2015-01-01", col = "blue"))
      