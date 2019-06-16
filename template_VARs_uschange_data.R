# 11.2 Vector autoregressions
library(vars)
library(fpp2)
head(uschange)

VARselect(uschange[, 1:2], lag.max = 8, type = "const")[["selection"]]

# check residules correlations
var1 <- VAR(uschange[, 1:2], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")

var2 <- VAR(uschange[, 1:2], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

# check p-value > 0.05
var3 <- VAR(uschange[, 1:2], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

# forecast
forecast(var3) %>% autoplot() + xlab("Year")
