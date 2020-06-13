# The Stock Market Data

library(ISLR)
library(tidyverse)
# --- EDA --- 
glimpse(Smarket)
head(Smarket)

# names(Smarket)
# dim(Smarket)
# summary(Smarket)

pairs(Smarket, col=Smarket$Direction)

cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

# Use ggplot and others
qplot(data=Smarket, y=Volume)

Smarket %>% ggplot(aes(x=seq(1,1250), y=Volume, color=Direction)) +
  geom_point()

# --- Logistic Regression ---

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.126000   0.240736  -0.523    0.601
# Lag1        -0.073074   0.050167  -1.457    0.145
# Lag2        -0.042301   0.050086  -0.845    0.398
# Lag3         0.011085   0.049939   0.222    0.824
# Lag4         0.009359   0.049974   0.187    0.851
# Lag5         0.010313   0.049511   0.208    0.835
# Volume       0.135441   0.158360   0.855    0.392

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

# Prediction
glm.probs <- predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Smarket$Direction)

table(Smarket$Direction)

glm.pred <- rep("Down",1250)
glm.pred[glm.probs>.5] <- "Up"
# glm.pred <- ifelse(glm.prob>0.5, "UP", "Down")

table(glm.pred,Smarket$Direction)
(507+145)/1250  # 0.5216, slightly better than random guess

# Accuracy
mean(glm.pred==Smarket$Direction)

# Split into "train" and "test" data set
train <- (Smarket$Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)

Direction.2005 <- Smarket$Direction[!train]

# fit the model again
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket,family=binomial,subset=train)
glm.probs <- predict(glm.fits,Smarket.2005,type="response")
glm.pred <- ifelse(glm.probs >0.5, "Up", "Down")
# glm.pred <- rep("Down",252)
# glm.pred[glm.probs>.5] <- "Up"

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits <- glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs <- predict(glm.fits,Smarket.2005,type="response")
glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) #  0.5595238
106/(106+76)
summary(glm.fits)

predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")
