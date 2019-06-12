# examples for VARs
# https://www.r-econometrics.com/timeseries/varintro/

# set the seed for simulated data
library(vars)
set.seed(123)
t <- 200
k <- 2
p <- 2

# coef matrix
A.1 <- matrix(c(-0.3, 0.6, -0.4, 0.5), k)
A.2 <- matrix(c(-0.1, -0.2, -0.1, 0.05), k)
A <- cbind(A.1, A.2)

# generate time series
series <- matrix(0, k, t+2*p)

for(i in (p+1):(t+2*p)){
  series[, i] <- A.1%*%series[, i-1] + A.2%*%series[, i-2] + rnorm(k, 0, 0.5)
}

series <- ts(t(series[, -(1:p)]))
names <- c("V1", "V2")
plot.ts(series)

# check assumptions
acf(series)
pacf(series)

# model for VARS
var.aic <- VAR(series, type="none", lag.max = 5, ic="AIC")
summary(var.aic)

# true A values
A

# extract coef
est_coefs <- coef(var.aic)
est_coefs <- rbind(est_coefs[[1]][, 1], est_coefs[[2]][, 1])
round(est_coefs, 2)
