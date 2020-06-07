# load packages
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(fpp2)

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
