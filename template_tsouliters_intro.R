# Detecting Outliers in Time Series (LS/AO/TC) using tsoutliers in R
library(tsoutliers)
library(expsmooth)
library(fma)


# chicken data set --------------------------------------------------------


outlier.chicken <- tsoutliers::tso(chicken,types = c("AO","LS","TC"),
                                   maxit.iloop=10)
outlier.chicken
plot(outlier.chicken)

plot(chicken) # original


# Nile river example ------------------------------------------------------
plot(Nile)
auto.arima(Nile)

# detect outliers
nile.outliers <- tso(Nile, types = c("AO", "LS", "TC"))
nile.outliers
plot(nile.outliers)


# temporary change, TC ----------------------------------------------------
tc <- rep(0, 50)
tc[20] <- 1
tc1 <- filter(tc, filter = 0, method = "recursive")
tc2 <- filter(tc, filter = 0.3, method = "recursive")
tc3 <- filter(tc, filter = 0.7, method = "recursive")
tc4 <- filter(tc, filter = 1, method = "recursive")
par(mfrow = c(2,2))
plot(tc1, main = "TC delta = 0")
plot(tc2, main = "TC delta = 0.3")
plot(tc3, main = "TC delta = 0.7")
plot(tc4, main = "TC delta = 1", type = "s")
dev.off()

# unit impulse
m1 <- ts(outliers.effects(outlier.chicken$outliers, n = length(chicken), weights = FALSE))
tsp(m1) <- tsp(chicken)
plot(m1)

# weighted by the estimated coefficients
m2 <- ts(outliers.effects(outlier.chicken$outliers, n = length(chicken), weights = TRUE))
tsp(m2) <- tsp(chicken)
plot(m2)

# https://stats.stackexchange.com/questions/104882/detecting-outliers-in-time-series-ls-ao-tc-using-tsoutliers-package-in-r-how