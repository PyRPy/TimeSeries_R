library(forecast)
fit <- auto.arima(sunspots)
fit
forecast(fit, 3)
accuracy(fit)
plot(forecast(fit, 6), xlab="Time", ylab="Number")

library(astsa)
sarima(sunspots, 2, 1, 2)
plot(sunspots)
acf2(sunspots, 200)

diff1 <- diff(sunspots, 1)
plot(diff1)
acf2(diff1)
sarima(sunspots, 0, 1, 2)
sarima(sunspots, 2, 1, 2)
sarima(sunspots, 3, 1, 2)

# seasonal model
diff128and1 <- diff(diff1, 128) # crazy
acf2(diff128and1)
sarima(sunspots, 0, 1, 1, 1, 0, 0, 128) # crazy
