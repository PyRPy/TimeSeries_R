# tsDyn package -----------------------------------------------------------

library(tsDyn)

# Linearity ---------------------------------------------------------------

obj <- llar(log(lynx), m = 3)
plot(obj)


# Case study --------------------------------------------------------------
# Canadian lynx data set
str(lynx)
summary(lynx)
plot(lynx)

# Explorative analysis ----------------------------------------------------
# transform the data
x <- log10(lynx)
par(mfrow=c(2,1), mar=c(0,0,0,0))
plot(x, ax=F)
box()
plot(x[length(x):1], type = "l", ax = F)
box()

par(mfrow=c(2,1), mar=c(2,2,0,0))
autopairs(x, lag = 3, type = "regression") # not working

hist(x, br=13)

par(mfrow = c(2,1), mar=c(2,4,0,0))
acf(x)
pacf(x)

library(tseriesChaos)
mutual(x)
dev.off()

par(mfrow = c(1,1), mar=c(0,0,0,0))
recurr(x, m=3, d=1, levels=c(0,0.2, 1))

lag.plot(x, lags = 3, layout = c(1,3))
delta.test(x)
delat.lin.test(x)

# Model selection ---------------------------------------------------------
mod.ar <- linear(x, m = 2)
mod.ar

mod.setar <- setar(x, m=2, mL=2, mH=2, thDelay = 1)
mod.setar
