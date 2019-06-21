# Chapter 5 Time series regression models
# 5.1 linear model
# simple linear model
library(fpp2)
autoplot(uschange[, c("Consumption", "Income")]) +
  ylab("% change") +
  xlab("year")

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

tslm(Consumption ~ Income, data = uschange)

# multiple linear regression
uschange %>% 
  as.data.frame() %>% 
  GGally::ggpairs()

# 5.2 least squares estimation
fit.constMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings,
  data = uschange)

summary(fit.constMR)

# fitted values
autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(fit.constMR), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit.constMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

# 5.3 Evaluating the regression model
checkresiduals(fit.constMR)

# Residual plots against predictors
df <- as.data.frame(uschange)
df[,"Residuals"]  <- as.numeric(residuals(fit.constMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

# Residual plots against fitted values
cbind(Fitted = fitted(fit.constMR),
      Residuals=residuals(fit.constMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

# Outliers and influential observations
# Spurious regression
aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)

checkresiduals(fit)

# 5.4 Some useful predictors
# Trend
# Dummy variables
# Seasonal dummy variables
# Example: Australian quarterly beer production
beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + xlab("Year") + ylab("Megalitres")

fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)

autoplot(beer2, series="Data") +
  autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")

cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

# Fourier series
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)
# A regression model containing Fourier terms is 
# often called a harmonic regression