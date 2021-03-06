# Chapter 09 Forecasting with Linear Regression

# The linear regression ---------------------------------------------------

# Features engineering of the series components
# -------- Code Chank 1 --------
library(TSstudio)

data("USgas")

ts_plot(USgas,
        title = "US Monthly Natural Gas consumption",
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year")
# -------- Code Chank 2 --------
ts_info(USgas)

# -------- Code Chank 3 --------
ts_decompose(USgas)

# transform a ts object to a data.frame object
# -------- Code Chank 4 --------
USgas_df <- ts_to_prophet(USgas)

# -------- Code Chank 5 --------
head(USgas_df)
# trend
# -------- Code Chank 6 --------
USgas_df$trend <- 1:nrow(USgas_df)

# -------- Code Chank 7 --------
library(lubridate)

# seasons
USgas_df$seasonal <- month(USgas_df$ds, label = T)
str(USgas_df)

# -------- Code Chank 8 --------
head(USgas_df)

# setting a testing partition length
# -------- Code Chank 9 --------
h <- 12 

train <- USgas_df[1:(nrow(USgas_df) - h), ]

test <- USgas_df[(nrow(USgas_df) - h + 1):nrow(USgas_df), ]


# Modeling the series trend and seasonal ----------------------------------

# -------- Code Chank 10 --------
md_trend <- lm(y ~ trend, data = train)

summary(md_trend)

# -------- Code Chank 11 --------
train$yhat <- predict(md_trend, newdata = train)

test$yhat <- predict(md_trend, newdata = test)

# -------- Code Chank 12 --------
library(plotly)
plot_lm <- function(data, train, test, title = NULL){
  p <- plot_ly(data = data, 
               x = ~ ds, 
               y = ~ y, 
               type = "scatter",
               mode = "line",
               name = "Actual") %>%
    add_lines(x =  ~ train$ds,
              y = ~ train$yhat,
              line = list(color = "red"),
              name = "Fitted") %>%
    add_lines(x =  ~ test$ds,
              y = ~ test$yhat,
              line = list(color = "green", dash = "dot", width = 3),
              name = "Forecasted") %>%
    layout(title = title,
           xaxis = list(title = ""),
           yaxis = list(title = "Billion Cubic Feet"),
           legend = list(x = 0.05, y = 0.95))
  return(p)
}

# -------- Code Chank 13 --------
plot_lm(data = USgas_df, 
        train = train, 
        test = test,
        title = "Predicting the Trend Component of the Series")

# -------- Code Chank 14 --------
mape_trend <- c(mean(abs(train$y - train$yhat) / train$y),
                mean(abs(test$y - test$yhat) / test$y))

mape_trend

# -------- Code Chank 15 --------
md_seasonal <- lm(y ~ seasonal, data = train)

summary(md_seasonal)

# -------- Code Chank 16 --------
train$yhat <- predict(md_seasonal, newdata = train)
test$yhat <- predict(md_seasonal, newdata = test)

plot_lm(data = USgas_df, 
        train = train, 
        test = test,
        title = "Predicting the Seasonal Component of the Series")

# -------- Code Chank 17 --------
mape_seasonal <- c(mean(abs(train$y - train$yhat) / train$y),
                   mean(abs(test$y - test$yhat) / test$y))

mape_seasonal

# combined trend and season factors
# -------- Code Chank 18 --------
md1 <- lm(y ~ seasonal + trend, data = train)

summary(md1)

# -------- Code Chank 19 --------
train$yhat <- predict(md1, newdata = train)
test$yhat <- predict(md1, newdata = test)


plot_lm(data = USgas_df, 
        train = train, 
        test = test,
        title = "Predicting the Seasonal Component of the Series")

# -------- Code Chank 20 --------
mape_md1 <- c(mean(abs(train$y - train$yhat) / train$y),
              mean(abs(test$y - test$yhat) / test$y))
mape_md1

# -------- Code Chank 21 --------
md2 <- lm(y ~ seasonal + trend + I(trend^2), data = train)

summary(md2)

# -------- Code Chank 22 --------
train$yhat <- predict(md2, newdata = train)
test$yhat <- predict(md2, newdata = test)


plot_lm(data = USgas_df, 
        train = train, 
        test = test,
        title = "Predicting the Seasonal Component of the Series")

mape_md2 <- c(mean(abs(train$y - train$yhat) / train$y),
              mean(abs(test$y - test$yhat) / test$y))

mape_md2

# tslm function (forecast)-------------------------------------------------


# -------- Code Chank 23 --------
USgas_split <- ts_split(USgas, sample.out = h)

train.ts <- USgas_split$train

test.ts <- USgas_split$test

# -------- Code Chank 24 --------
library(forecast)

md3 <- tslm(train.ts ~ season + trend + I(trend^2))

summary(md3)


# Modeling single events and non-seasonal events --------------------------
# such as holidays
# -------- Code Chank 25 --------
r <- which(USgas_df$ds == as.Date("2014-01-01"))
USgas_df$s_break <- ifelse(year(USgas_df$ds) >= 2010, 1, 0)
USgas_df$s_break[r] <- 1
md3 <- tslm(USgas ~ season + trend + I(trend^2) + s_break, data = USgas_df)
summary(md3)


# Forecasting a series with multiseasonality ------------------------------
library(UKgrid)

UKdaily <- extract_grid(type = "data.frame",
                        columns = "ND",
                        aggregate = "daily")

head(UKdaily)


ts_plot(UKdaily,
        title = "The UK National Demand for Electricity",
        Ytitle = "MW",
        Xtitle = "Year")

# -------- Code Chank 26 --------
ts_heatmap(UKdaily[which(year(UKdaily$TIMESTAMP) >= 2016),],
           title = "UK the Daily National Grid Demand Heatmap")


# Preprocessing and feature engineering of the ----------------------------


# -------- Code Chank 27 --------
library(dplyr)
UKdaily <- UKdaily %>%
  mutate(wday = wday(TIMESTAMP, label = TRUE),
         month = month(TIMESTAMP, label = TRUE),
         lag365 = dplyr::lag(ND, 365)) %>%
  filter(!is.na(lag365)) %>%
  arrange(TIMESTAMP)
str(UKdaily)

# -------- Code Chank 28 --------
start_date <- min(UKdaily$TIMESTAMP)


UK_ts <- ts(UKdaily$ND, 
            start = c(year(start_date), yday(start_date)),
            frequency = 365)
ts_acf(UK_ts, lag.max = 365 * 4)

# -------- Code Chank 29 --------
h <-  365
UKpartitions <- ts_split(UK_ts, sample.out = h)
train_ts <- UKpartitions$train
test_ts <- UKpartitions$test

train_df <- UKdaily[1:(nrow(UKdaily) - h), ]
test_df <- UKdaily[(nrow(UKdaily) - h + 1):nrow(UKdaily), ]


# Training and testing the forecasting model ------------------------------

# -------- Code Chank 30 --------
md_tslm1 <- tslm(train_ts ~ season + trend)
fc_tslm1 <- forecast(md_tslm1, h = h)
test_forecast(actual = UK_ts,
              forecast.obj = fc_tslm1,
              test = test_ts)

accuracy(fc_tslm1, test_ts)

# add day of the week, month of the year as features
# -------- Code Chank 31 --------
md_tslm2 <- tslm(train_ts ~ season + trend + wday, data = train_df)
fc_tslm2 <- forecast(md_tslm2, h = h, newdata = test_df)
test_forecast(actual = UK_ts,
              forecast.obj = fc_tslm2,
              test = test_ts)
accuracy(fc_tslm2, test_ts)

# add lag365
# -------- Code Chank 32 --------
md_tslm3 <- tslm(train_ts ~ season + trend + wday + month + lag365, data = train_df)
fc_tslm3 <- forecast(md_tslm3, h = h, newdata = test_df)
test_forecast(actual = UK_ts,
              forecast.obj = fc_tslm3,
              test = test_ts)
accuracy(fc_tslm3, test_ts) 


# Model selection ---------------------------------------------------------
# -------- Code Chank 33 --------
summary(md_tslm3)$coefficients %>% tail(1)

# -------- Code Chank 34 --------
anova(md_tslm3)

# -------- Code Chank 35 --------
final_md <- tslm(UK_ts ~ season + trend + wday + month + lag365, 
                 data = UKdaily)

# Residuals analysis ------------------------------------------------------

# -------- Code Chank 36 --------
checkresiduals(final_md)
# the residuals are not white noise, as some autocorrelation exists between 
# the residuals series and their lags. This is technically an indication that 
# the model did not capture all the patterns or information that exists in 
# the series.


# Finalizing the forecast -------------------------------------------------

# -------- Code Chank 37 --------
UK_fc_df <- data.frame(date = seq.Date(from = max(UKdaily$TIMESTAMP) + days(1), 
                                       by = "day", 
                                       length.out = h))
# -------- Code Chank 38 --------
UK_fc_df$wday <- factor(lubridate::wday(UK_fc_df$date, label = TRUE), ordered = FALSE)

UK_fc_df$month <- factor(month(UK_fc_df$date, label = TRUE), ordered = FALSE)

UK_fc_df$lag365 <- tail(UKdaily$ND, h)

# -------- Code Chank 39 --------
UKgrid_fc <- forecast(final_md, h = h, newdata = UK_fc_df)

# -------- Code Chank 40 --------
plot_forecast(UKgrid_fc,
              title = "The UK National Demand for Electricity Forecast",
              Ytitle = "MW",
              Xtitle = "Year")
