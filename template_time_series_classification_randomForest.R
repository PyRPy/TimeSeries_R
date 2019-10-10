# Using a Random Forest for Time Series Data
# https://stats.stackexchange.com/questions/384924/using-a-random-forest-for-time-series-data
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ranger)
library(MetricsWeighted) # AUC

# Import
raw <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/daily-min-temperatures.csv")
write.csv(raw, "raw.csv")

# Explore
str(raw)
head(raw)
summary(raw)
hist(raw$Temp, breaks = "FD")

# Prepare and add binary response
prep <- raw %>% 
  mutate(Date = ymd(Date),
         y = year(Date),
         m = month(Date),
         d = day(Date),
         increase = 0 + (Temp > lag(Temp)))

with(prep, table(y))
summary(prep)

# Plot full data -> year as seasonality
ggplot(data = prep, aes(x = Date, y = Temp))+
  geom_line() +
  scale_x_date()

# No visible within year seasonality
prep %>% 
  filter(y == 1987) %>% 
  ggplot(aes(x = Date, y = Temp))+
  geom_line() +
  scale_x_date()

# Add some lags and diffs & remove incomplete rows
prep <- prep %>% 
  mutate(lag1 = lag(Temp),
         lag2 = lag(Temp, 2L),
         lag3 = lag(Temp, 3L),
         dif1 = lag1 - lag2,
         dif2 = lag2 - lag3) %>% 
  filter(complete.cases(.))
head(prep)

# Train/valid split in blocks
valid <- prep %>% 
  filter(y == 1990)
train <- prep %>% 
  filter(y < 1990)

# Models
y <- "increase" # response
x <- c("lag1", "lag2", "lag3", "dif1", "dif2", "y", "m", "d") # covariables
form <- reformulate(x, y) # formula
class(form)

# Logistic model: Linear dependence between difs and lags
fit_glm <- glm(form, 
               data = train, 
               family = binomial()) 
summary(fit_glm)
sum(is.na(train))

# Random forest
fit_rf <- ranger(form, 
                 data = train,
                 seed = 345345, 
                 importance = "impurity", 
                 probability = TRUE)
fit_rf
barplot(-sort(-importance(fit_rf))) # Variable importance

# Evaluate on 1990 for glm by looking at ROC AUC
pred_glm <- predict(fit_glm, valid, type = "response")
AUC(valid[[y]], pred_glm) # 0.684 ROC AUC

# Then for rf
pred_rf <- predict(fit_rf, valid)$predictions[, 2]
AUC(valid[[y]], pred_rf)    # 0.702 ROC AUC

# view OOB residuals of rf within one month to see if structure is left over
random_month <- train %>% 
  mutate(residuals = increase - fit_rf$predictions[, 2]) %>% 
  filter(y == 1987, m == 3) 

ggplot(random_month, aes(x = Date, y = residuals))+
  geom_line(color = "#00AFBB", size = 2) +
  scale_x_date()
