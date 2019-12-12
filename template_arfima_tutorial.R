
# arfima tutorial ---------------------------------------------------------

# https://cran.r-project.org/web/packages/arfima/arfima.pdf

library(arfima)
library(astsa)
set.seed(6533)
sim <- arfima.sim(1000, model = list(phi=0.2, dfrac=0.3, dint = 2))
plot(sim, type="l")

fit <- arfima(sim, order = c(1, 2, 0))
fit
resids = resid(fit)
sarima(resids[[1]], 0, 0, 0) # seems fine
# find a arima model ------------------------------------------------------

acf(sim)
pacf(sim) # suggest a AR(1)

sarima(sim, 1, 2, 0) # not working

# try auto.arima
library(forecast)
auto.arima(sim) # ARIMA(1,2,2) 

# try again
sarima(sim, 1, 2, 2) # seems fine
