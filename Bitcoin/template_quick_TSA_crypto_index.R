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
head(dat)
# plot the data
plot(dat$Date, dat$Close, type = "l")

# select data from 2017
dat2017 <- subset(dat, Date >= as.Date("2017-01-01"))
plot(dat2017$Date, dat2017$Close, type = "l") # correct trend

# fitt a ARIMA model ------------------------------------------------------

auto.arima(dat2017[, "Close"])
# ARIMA(5,1,5) 
# the model does not show reasonable prediction

# try data 2019 till current
dat2019 <- subset(dat, Date >= as.Date("2019-01-01"))
auto.arima(dat2019[, "Close"]) # ARIMA(1, 1, 0)
forecast(auto.arima(dat2019[, "Close"]), h = 5)


# use astsa library --------------------------------------------------------
library(astsa)
sarima(dat2019[,"Close"], 1, 1, 0) # not normal
plot(dat2019$Date, dat2019$Close, type = "l")
acf2(dat2019$Close) # suggest AR(1)
plot(diff(dat2019$Close)) # still not stable ?

sarima.for(dat2019$Close, 5, 1, 1, 0)
# reference
# https://www.r-bloggers.com/quick-time-series-analysis-of-the-cci30-crypto-index/