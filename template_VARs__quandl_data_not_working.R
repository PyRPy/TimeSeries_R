# obtain data on GDP, Deflator and S&P 500 index
# https://rstudio-pubs-static.s3.amazonaws.com/274358_9fbc895fea2b443aaa60ad1a75c75687.html

library(Quandl)
Quandl.api_key("XzdSwkDsE98Mxj3ixQzG")

rgdp.q <- Quandl("FRED/GDPC96", type="zoo")
def.q <- Quandl("FRED/GDPDEF", type="zoo")
Quandl.api_key('rxVQhZ8_nxxeo2yy4Uz4')
sp.q <- Quandl("YAHOO/INDEX_GSPC/CLOSE", collapse="quarterly",type="zoo")

par(mfrow=c(3,3))
plot(rgdp.q, xlab="", ylab="", main="Real GDP", col="blue")
plot(def.q, xlab="", ylab="", main="GDP Deflator", col="red")
plot(sp.q, xlab="", ylab="", main="S&P 500 index", col="red")

plot(log(rgdp.q), xlab="", ylab="", main="Real GDP (log)", col="blue")
plot(log(def.q), xlab="", ylab="", main="GDP Deflator", col="red")
plot(log(sp.q), xlab="", ylab="", main="S&P 500 index", col="red")

# log change in house price index
dl.rgdp <- diff(log(rgdp.q))
dl.def <- diff(log(def.q))
dl.sp <- diff(log(sp.q))

plot(dl.rgdp, xlab="", ylab="", main="Real GDP (Diff log)", col="blue")
plot(dl.def, xlab="", ylab="", main="GDP Deflator", col="red")
plot(dl.sp, xlab="", ylab="", main="S&P 500 index", col="red")


y1 <- 400*dl.rgdp
y2 <- 400*(dl.sp - dl.def)

par(mfrow=c(1,1))
plot(y1, xlab="", ylab="", main="Growth Rate of Real GDP vs. S&P 500 return", col="blue", ylim=c(-140, 80))
legend("topright",c("GDP Growth Rate", "Adjusted Return of S&P 500"), bty = "n", col = c(4,2), lty = c(1,2))
lines(y2, col="red", lty="dashed")


# form dataset for VAR model
y.Q <- cbind(y1, y2)
y.Q <- window(y.Q, start="1961 Q1", end="2016 Q4")


# load package that allows to estimate and analyze VAR models
# install.packages("vars")
library(vars)


#Q.a
# selection criteria summary
VARselect(y.Q, lag.max=8, type="const")

# estimate a reduced form VAR(2) : Matrix A <- based on SC criteria
# var1$varresult$y1$coefficients[1]
varp <- VAR(y.Q, ic="AIC", lag.max=9, type="const")
varp
summary(varp)


# using stargazer package to report results of VAR estimation
lmp <- varp$varresult


library(stargazer)
stargazer(lmp$y1, lmp$y2, type="text", dep.var.labels.include=FALSE)


# plot residuals and their ACF and PACF
#plot(varp)
#plot(varp, names="y1")

# QQ plot for residuals
#str(varp)
#e.y1 <- varp$varresult$y1$residuals
#qqnorm(e.y1)
#qqline(e.y1)

# multivariate Jarque-Bera test
#normality.test(varp)


# Q.b
# Granger causality
causality(varp, cause="y1")
causality(varp, cause="y2")



# Q.c
# estimate restricted VAR - based on Granger causality test eliminate lags of y2 from the equation for  y1
# define a  matrix with restictions
mat.r <- matrix(1, nrow=2, ncol=5)
mat.r[2, c(1,3)] <- 0
mat.r
varp.r <- restrict(varp, method="manual", resmat=mat.r)
varp.r
summary(varp.r)
varp.r$restrictions
Acoef(varp.r)

# estimate restricted VAR - keep only variables with t-value larger than 2.0
#varp.r.ser <- restrict(varp, method="ser", thresh=2.0)
#varp.r.ser
#summary(varp.r.ser)
#varp.r.ser$restrictions
#Acoef(varp.r.ser)


# forecasting
varp.f <- predict(varp, n.ahead=12)
plot(varp.f)
fanchart(varp.f)

# Q.d

# IRFs - based on Choleski decomposition of var(e)
varp.irfs <- irf(varp, n.ahead=20)
par(mfcol=c(2,2), cex=0.6)
plot(varp.irfs, plot.type="single")

# FEVD - based on Choleski decomposition of var(e)
varp.fevd <- fevd(varp, n.ahead=40)
varp.fevd[[1]][c(1,4,8,40),]
varp.fevd[[2]][c(1,4,8,40),]
plot(varp.fevd)
plot(varp.fevd, addbars=8)


# note: ordering of variables matters for IRF and FEVD

# ordering 1: y1 before y2
y.Q.ord1 <- cbind(y1, y2)
y.Q.ord1 <- window(y.Q.ord1, start="1961 Q1", end="2016 Q4")

# ordering 2: y2 before y1
y.Q.ord2 <- cbind(y2, y1)
y.Q.ord2 <- window(y.Q.ord2, start="1961 Q1", end="2016 Q4")

# reduced form VAR(1)
var1.ord1 <- VAR(y.Q.ord1, p=2, type="const")
var1.ord2 <- VAR(y.Q.ord2, p=2, type="const")

# IRF based on Choleski decomposition of var(e)
var1.irfs.ord1 <- irf(var1.ord1, n.ahead=20)
var1.irfs.ord2 <- irf(var1.ord2, n.ahead=20)
par(mfcol=c(2,2), cex=0.6)
plot(var1.irfs.ord1, plot.type="single")
plot(var1.irfs.ord2, plot.type="single")

# FEVD based on Choleski decomposition of var(e)
var1.fevd.ord1 <- fevd(var1.ord1, n.ahead=20)
var1.fevd.ord2 <- fevd(var1.ord2, n.ahead=20)
plot(var1.fevd.ord1)
plot(var1.fevd.ord2)