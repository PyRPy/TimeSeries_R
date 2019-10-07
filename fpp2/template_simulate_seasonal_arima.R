# Simulating from a specified seasonal ARIMA model
# https://robjhyndman.com/hyndsight/simulating-from-a-specified-seasonal-arima-model/
library(forecast)
model <- Arima(ts(rnorm(250),freq=4), order=c(0,0,1), seasonal=c(0,0,1),
               fixed=c(theta=-0.4, Theta=-0.2))
foo <- simulate(model, nsim=250)
fit <- Arima(foo, order=c(0,0,1), seasonal=c(0,0,1))

library(astsa)
acf2(foo, 30)
plot(foo, type="l")
