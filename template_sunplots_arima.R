library(forecast)
str(sunspots)
plot(sunspots)
fit <- auto.arima(sunspots)
fit
forecast(fit, 3)
accuracy(fit)
plot(forecast(fit, 6), xlab="Time", ylab="Number")

library(astsa)
sarima(sunspots, 2, 1, 2, 0, 1, 0, 12)
plot(sunspots)
acf2(sunspots, 200)
diff12 <- diff(sunspots, 12)
plot(diff12)

m <- decompose(sunspots)
plot(m)


diff1 <- diff(sunspots, 1)
plot(diff1)
acf2(diff1)
sarima(sunspots, 0, 1, 2)
sarima(sunspots, 2, 1, 2)
sarima(sunspots, 3, 1, 2)

# seasonal model
diff128and1 <- diff(diff1, 128) # crazy
acf2(diff128and1)
sarima(sunspots, 0, 1, 1, 1, 0, 0, 12) # crazy


require(graphics)
plot(sunspots, main = "sunspots data", xlab = "Year",
     ylab = "Monthly sunspot numbers")
