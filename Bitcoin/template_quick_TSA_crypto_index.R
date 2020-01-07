# Quick Time Series Analysis of the CCI30 Crypto Index

# packages ----------------------------------------------------------------
# not very helpful
# install.load::install_load(
#   "tidyquant",
#   "timetk",
#   "tibbletime",
#   "sweep",
#   "anomalize",
#   "caret",
#   "forecast",
#   "funModeling",
#   "xts",
#   "fpp",
#   "lubricate",
#   "tidyverse",
#   "urca",
#   "prophet"
# )
library(fpp2)

# data --------------------------------------------------------------------
dat <- read.csv("cci30_OHLCV.csv")
dat$Date <- as.Date(dat$Date)

# plot the data
plot(dat$Date, dat$Close, type = "l")

# select data from 2017
dat2017 <- subset(dat, Date >= as.Date("2017-01-01"))
plot.ts(dat2017[, 2:5])

plot(dat2017$Date, dat2017$Close, type = "l")


# fitt a ARIMA model ------------------------------------------------------

auto.arima(dat2017[, "Close"])
# ARIMA(5,1,5) 

forecast(auto.arima(dat2017[, "Close"]), h = 10)
tail(dat2017)
plot(dat$Close)


# reference
# https://www.r-bloggers.com/quick-time-series-analysis-of-the-cci30-crypto-index/