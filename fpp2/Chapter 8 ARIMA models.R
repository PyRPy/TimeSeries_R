# Chapter 8 ARIMA models
# 8.1 Stationarity and differencing
# Differencing
Box.test(diff(goog200), lag=10, type="Ljung-Box")
ggAcf(goog200)
ggAcf(diff(goog200))

# Random walk model
