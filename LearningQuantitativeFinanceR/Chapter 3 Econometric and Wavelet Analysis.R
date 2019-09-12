####
####Chapter3 Econometric and Wavelet Analysis
####
# Simple linear regression

Data <-read_excel("DataForRegression.xlsx") # data not right
head(Data)
YPrice <-  Data$`DJI INDEX`
XPrice <-  Data$Visa
plot(YPrice, XPrice, xlab="XPrice", ylab="YPrice")

LinearR.lm <-  lm(YPrice ~ XPrice, data=Data)
coeffs <-  coefficients(LinearR.lm); coeffs
YPrice <- 92.7051345 + -0.1680975*(XPrice)
summary(LinearR.lm)$r.squared
summary(LinearR.lm)
Predictdata = data.frame(XPrice=75)
predict(LinearR.lm, Predictdata, interval="confidence") 

LinearR.res = resid(LinearR.lm)
plot(XPrice, LinearR.res, 
ylab="Residuals", xlab="XPrice", 
main="Residual Plot")

LinearRSTD.res = rstandard(LinearR.lm)
plot(XPrice, LinearRSTD.res, 
ylab="Standardized Residuals", xlab="XPrice", 
main="Residual Plot")

qqnorm(LinearRSTD.res, 
ylab="Standardized Residuals", 
xlab="Normal Scores", 
main="Error Normal Distribution plot") 
qqline(LinearRSTD.res)

### Multiple linear regression

DataMR <-  read.csv("DataForMultipleRegression.csv") #not used in book
head(DataMR)
MultipleR.lm <- lm(StockYPrice ~  StockX1Price + StockX2Price +  StockX3Price + StockX4Price,  data=DataMR)
summary(MultipleR.lm)


StockYPrice <-  88.42137 +(-0.16625)*DataMR$StockX1Price + (-0.00468) * DataMR$StockX2Price +(.03497)*DataMR$StockX3Price +(.02713)*DataMR$StockX4Price
StockYPrice


newdata <- data.frame(StockX1Price=70, StockX2Price=90, StockX3Price=60, StockX4Price=80)
predict(MultipleR.lm, newdata)

summary(MultipleR.lm)$r.squared 
summary(MultipleR.lm)$adj.r.squared 
predict(MultipleR.lm, newdata, interval="confidence")

library(car)
vif(MultipleR.lm)

##ANOVA

DataANOVA = read.csv("DataAnova.csv")
head(DataANOVA)
boxplot(DataANOVA$Returns ~ DataANOVA$Stock) 

oneway.test(DataANOVA$Returns ~ DataANOVA$Stock, var.equal=TRUE)

#Feature selection
library(MASS)
DataMR <- read.csv("DataForMultipleRegression.csv")
head(DataMR)
correlationMatrix<- cor(DataMR[,1:4])
correlationMatrix

MultipleR.lm <- lm(StockYPrice ~ StockX1Price + StockX2Price + 
                     StockX3Price + StockX4Price, data=DataMR)
step <- stepAIC(MultipleR.lm, direction="both")
step$anova 

# Variable Selection 
library(mlbench)
library(caret)
DataVI <- read.csv("DataForMultipleRegression1.csv")
head(DataVI)

control<- rfeControl(functions=rfFuncs, method="cv", number=10)
Output <- rfe(DataVI[,1:4], DataVI[,0:1], sizes=c(1:4), rfeControl=control)
predictors(Output)
plot(Output, type=c("g", "o"))

# install.packages("wavelets")
library(wavelets)

par(mfrow=c(2,1))
dji <- read.csv("DJI_5_Yr.csv")
plot(dji$Close, type="l")
# head(dji)
ret_dji <- diff(log(dji$Close))
plot(ret_dji, type="l")
dev.off()

# transform it into ts
dji_ts <- as.ts(dji$Close)

model <- dwt(dji_ts, filter="la8", n.levels = 3)

# model
plot(model)

model <- dwt(dji_ts, filter="haar", n.levels = 3)
plot(model)
