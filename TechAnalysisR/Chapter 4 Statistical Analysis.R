# Chapter 4 Statistical Analysis ------------------------------------------
# https://bookdown.org/kochiuyu/Technical-Analysis-with-R/random-number.html
# 4.1 Basic Statistics

# Central Tendency
x <- 1:40 
mean(x)
median(x)
summary(x)

# dispersion 
range(x)

IQR(x)
var(x)
sd(x)
(round(var(x), 4) == round(sd(x)^2, 4)) # true


# 4.2 Random number
sample(2:10, 8, replace = F)
sample(2:10, 7, replace = T)

namelist<-c("Mary", "Peter", "John", "Susan")
sample(namelist,2)  # sample from this list

runif(2, 4.6, 7.8) # random uniform distribution.

x <- rnorm(4, mean=0, sd=1) # standard nromal
x
sd(x)

# 4.3 Regression

x <- c(1,2,3,5,6,7,10,12,13)
y <- c(1,4,5,6,7,8,9,10,15)
z <- c(2,3,7,8,9,12,8,7,6)
df <-data.frame(x=x,y=y,z=z)

SLR <- lm(y~x, df)
summary(SLR)

# 4.3.2 Muliple linear regression
MLR <- lm(y ~ x + z, df)
summary(MLR)

# 4.3.3 Interaction terms
MLR2 <- lm(y~ x + z + x:z, df)
summary(MLR2)

# 4.3.4 Robust standard error
install.packages(c("sandwich","lmtest"))
library(sandwich)
library(lmtest)

MLR <- lm(y ~ x + z, df)
coeftest(MLR, vcov = vcovHC(MLR, "HC1")) 

# 4.4 Time Series Analysis

# 4.4.1 Time Series data
price <- c(2,3,7,8,9,12,8,7)
price <- ts(price)
price
plot(price)

df <- data.frame(a = c(3,4,5),
                 b = c(1,2,4),
                 d = c(1,2,3))
df <- ts(df)
df

# 4.4.2 Extensible Time Series Data
library(xts) 
date <- c("2007-01-03","2007-01-04",
          "2007-01-05","2007-01-06",
          "2007-01-07","2007-01-08",
          "2007-01-09","2007-01-10")
time <- c("08:00:01","09:00:01",
          "11:00:01","13:00:01",
          "14:00:01","15:00:01",
          "15:10:01","15:20:03")
price <- c(2,3,7,8,9,12,8,7)

df <-data.frame(date=date, time=time, price=price)
df$datetime <-paste(df$date, df$time)
df$datetime <-as.POSIXct(df$datetime,
                         format="%Y-%m-%d %H:%M:%S")
df <- xts(df,order.by=df$datetime) 

df # cannot plot it anymore

# 4.4.3 ARMA
price <- c(2,3,7,8,9,12,8,7)
AR1 <- arima(price,c(1,0,0))  # AR(1) model
AR1

AR2 <- arima(price,c(2,0,0))  # AR(2) model
AR2

MA1 <- arima(price,c(0,0,1))  
MA1

MA2 <- arima(price,c(0,0,2)) 
MA2

ARMA11 <- arima(price,c(1,0,1))
ARMA11
coef(ARMA11)

acf(price)  # suggests MA(1)
pacf(price)

plot.ts(ARMA11$residuals)  # residual plot

#  forecast package
library(forecast)
autoarma <-auto.arima (price) 
autoarma

theForecast <-predict(ARMA11,
                      n.ahead=2,
                      se.fit=TRUE) 
plot.ts(theForecast$pred)

# 4.4.4 GARCH
library(rugarch)

# estimate GARCH(1,1) with ARMA(1,1) on generate random number
a <- runif(1000, min = 0, max = 100) 
Spec <- ugarchspec(variance.model = list(model="sGARCH",
                                    garchorder = c(1,1)),
                   mean.model = list(armaOrder = c(1,1)),
                   distribution.model = "std")
Garch <- ugarchfit(spec = spec, data = a)
Garch

coefficient <-coef(Garch)
volatility <- sigma(Garch)
long.run.variance <- uncvariance(Garch)

# 4.4.5 VAR

a<- c(3.1,3.4,5.6,7.5,5,6,6,7.5,4.5,
      6.7,9,10,8.9,7.2,7.5,6.8,9.1)
b <- c(3.2,3.3,4.6,8.5,6,6.2,5.9,7.3,
       4,7.2,3,12,9,6.5,5,6,7.5)
d <-c(4,6.2,5.3,3.6,7.5,6,6.2,6.9,
      8.2,4,4.2,5,11,3,2.5,3,6)
df <-data.frame(a,b,d)

library(vars) 
abvar <- VAR(df, p=2) # run VAR(2)
coef(abvar)

summary(abvar) 

# coefficient plot
library(coefplot)
coefplot(abvar$varresult$a)
coefplot(abvar$varresult$b)
coefplot(abvar$varresult$d)

# 4.5 Panel Data
library(plm)
time <- c(1,2,1,2,1,2,1,2,1,2,1,2)
firm <-c(1,1,2,2,3,3,4,4,5,5,6,6)
x <- c(1, 2, 4, 3, 5, 4, 2, 5,5,6,7,8)
y <- c(2, 3, 5, 4, 3, 2, 4, 7,7,8,5,7)
p.df <- data.frame(time, firm, x, y)
p.df <- pdata.frame(p.df, index = c("firm", "time"), 
                    drop.index = F, row.names = T)

# 4.5.1 Clustering
pooled.plm <- plm(formula=y~x, data=p.df, model="pooling") # pooled OLS
coeftest(pooled.plm, vcov=vcovHC(pooled.plm, 
                                 type="sss", 
                                 cluster="group"))  

coeftest(pooled.plm, vcov=vcovHC(pooled.plm, 
                                 type="sss", 
                                 cluster="time")) 

# Two-way clustering of both time and group:
coeftest(pooled.plm, vcov=vcovDC(pooled.plm, 
                                 type="sss"))

# 4.5.2 Fixed Effect Model
fe.plm <- plm(formula=y~x, data=p.df, model="within")
coeftest(fe.plm, vcov=vcovHC(fe.plm, 
                             type="sss",
                             cluster="group")) 

coeftest(fe.plm, vcov=vcovHC(fe.plm, 
                             type="sss", 
                             cluster="time")) 

# 4.6 Simulation
# 4.6.1 Random Walk
RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}
# mu is the drift

P1<-RW(100,10,0,0.0004)
P2<-RW(100,10,0,0.0004)
plot(P1, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="red")
par(new=T)  #to draw in the same plot
plot(P2, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="blue")

# check the acf & pacf
acf(P1)
acf(P2)
pacf(P1)
pacf(P2)

# 4.6.2 Roll Model
require(zoo)

trial <-1000  #Number of trial

cost <-c() #cost each trial
sd <-c()   #sd each trial

true.cost = 2
true.sd = 1

time = 1:trial

for (i in 1:trial) {
  
  #Simulated Price Series
  epsilon = rnorm(time,sd=true.sd)
  prices = cumsum(epsilon)
  m_t = zoo(prices)
  a_t = m_t + true.cost
  b_t = m_t - true.cost
  
  #simulated trade prices 
  q_t = sign(rnorm(time))
  p_t = m_t + (true.cost * q_t)
  
  #1st difference of prices
  delta_p <- p_t-lag(p_t)  
  #omit n.a. entry
  delta_p <- na.omit(delta_p) 
  
  gamma_0 <- var(delta_p)
  gamma_1 <- cov(delta_p[1:length(delta_p)-1], 
                 delta_p[2:length(delta_p)])
  sigma_2 <- gamma_0 + 2 * gamma_1
  
  if(gamma_1 > 0){
    print("Error: Positive Autocovariance!")
  } else {
    cost <- append(cost,sqrt(-1*gamma_1))
    sd <-append(sd,sigma_2)
  }
}

# Stimulated Cost Plot

plot(cost)
est.cost <- mean(cost)

plot(sd)

est.sd <- mean(sd)

# Final Result
cat("True cost and sd are", true.cost," and ", true.sd)
cat("Estimated cost and sd are", est.cost," and ", est.sd)
