# Logistic regression -----------------------------------------------------
# https://github.com/PacktPublishing/Learning-Quantitative-Finance-with-R
# import data
library("quantmod")
getSymbols("^DJI",src="yahoo")
dji<- DJI[,"DJI.Close"]

# build indicators
avg10<- rollapply(dji,10,mean)
avg20<- rollapply(dji,20,mean)
std10<- rollapply(dji,10,sd)
std20<- rollapply(dji,20,sd)
rsi5<- RSI(dji,5,"SMA")
rsi14<- RSI(dji,14,"SMA")
macd12269<- MACD(dji,12,26,9,"SMA")
macd7205<- MACD(dji,7,20,5,"SMA")
bbands<- BBands(dji,20,"SMA",2)

# Up and Down direction
direction<- NULL
direction[dji> Lag(dji,20)] <- 1
direction[dji< Lag(dji,20)] <- 0

# create a table
dji<- cbind(dji,avg10,avg20,std10,std20,rsi5,rsi14,
            macd12269,macd7205,bbands,direction)

dm <- dim(dji)
dm

colnames(dji)[dm[2]]

# name a column as direction
colnames(dji)[dm[2]] <- "Direction"

# prepare and slip data
issd<- "2010-01-01"
ised<- "2014-12-31"
ossd<- "2015-01-01"
osed<- "2015-12-31"

# get row index
isrow<- which(index(dji) >= issd& index(dji) <= ised)
osrow<- which(index(dji) >= ossd& index(dji) <= osed)

# in-sample and out-sample data set
isdji<- dji[isrow,]
osdji<- dji[osrow,]

# mean and sd
isme<- apply(isdji,2,mean)
isstd<- apply(isdji,2,sd)

# identity matrix - in sample data
isidn<- matrix(1,dim(isdji)[1],dim(isdji)[2])

# standardize or normlize data
norm_isdji<- (isdji - t(isme*t(isidn))) / t(isstd*t(isidn))

# correct the last column - direction
dm<- dim(isdji)
norm_isdji[,dm[2]] <- direction[isrow]
head(norm_isdji)

# logistic regression model
formula<- paste("Direction ~ .",sep="")
model<- glm(formula,family="binomial",norm_isdji)

summary(model)

# prediction
pred<- predict(model,norm_isdji)

prob<- 1 / (1+exp(-(pred)))

# compare
par(mfrow=c(2,1))
plot(pred,type="l")
plot(prob,type="l")

head(prob)

# convert to 1 or 0
pred_direction<- NULL
pred_direction[prob> 0.5] <- 1
pred_direction[prob<= 0.5] <- 0

# confusion matrix - accuracy and more
library(caret)

# convert to factor somehow
matrix<- confusionMatrix(as.factor(pred_direction),as.factor(norm_isdji$Direction))
matrix

# test on out-sample data
osidn<- matrix(1,dim(osdji)[1],dim(osdji)[2])
norm_osdji<- (osdji - t(isme*t(osidn))) / t(isstd*t(osidn))
norm_osdji[,dm[2]] <- direction[osrow]

# check dim
dim(osidn)
dim(norm_osdji)

# out-sample prediction
ospred<- predict(model,norm_osdji)
osprob<- 1 / (1+exp(-(ospred)))

ospred_direction<- NULL
ospred_direction[osprob> 0.5] <- 1
ospred_direction[osprob<= 0.5] <- 0
# ospred_direction <- as.factor(ospred_direction)
# norm_osdji$Direction <- as.factor(norm_osdji$Direction)
osmatrix<- confusionMatrix(as.factor(ospred_direction),as.factor(norm_osdji$Direction))
osmatrix
