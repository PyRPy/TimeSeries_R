# vars tutorials in vars package
library(vars)
data("Canada")

# overview of data
summary(Canada)
dim(Canada)

# plot
plot(Canada, nc=2, xlab="")

# ADF test
library(urca)
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1

# ADF test on lags = 1
adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags = 1))
adf2

# select p
VARselect(Canada, lag.max = 8, type = "both")

Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct <- VAR(Canada, p=1, type = "both")
p1ct

# equations for regression
summary(p1ct, equation = "e")

# plot the results
plot(p1ct, names = "e")

# diagnosis tests
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial

norm1 <- normality.test(p1ct)
norm1$jb.mul

arch1 <- arch.test(p1ct, lags.multi = 5)
arch1$arch.mul

plot(arch1, names="e")
plot(stability(p1ct), nc=2)
