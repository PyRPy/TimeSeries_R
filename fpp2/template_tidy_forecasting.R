# Tidy forecasting in R
library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)
tourism


# abbreviate the state names ----------------------------------------------

tourism <- tourism %>%
  mutate(
    State = recode(State,
                   "Australian Capital Territory" = "ACT",
                   "New South Wales" = "NSW",
                   "Northern Territory" = "NT",
                   "Queensland" = "QLD",
                   "South Australia" = "SA",
                   "Tasmania" = "TAS",
                   "Victoria" = "VIC",
                   "Western Australia" = "WA"
    )
  )

# Forecasting a single time series ----------------------------------------

snowy <- tourism %>%
  filter(
    Region == "Snowy Mountains",
    Purpose == "Holiday"
  )
snowy

snowy %>% autoplot(Trips)

fit <- snowy %>%
  model(
    snaive = SNAIVE(Trips ~ lag("year")),
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  )
fit
fc <- fit %>%
  forecast(h = 12)
fc

fc %>%
  autoplot(snowy, level = NULL) +
  ggtitle("Forecasts for Snowy Mountains holidays") +
  xlab("Year") +
  guides(colour = guide_legend(title = "Forecast"))

hilo(fc, level = 95)


# Forecasting many series -------------------------------------------------
# this section not working properly
fit <- tourism %>%
  model(
    snaive = SNAIVE(Trips ~ lag("year")),
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  )
fit

fit %>%
  filter(Region == "Snowy Mountains", Purpose == "Holiday") %>%
  select(arima) %>%
  report()

fc <- fit %>%
  forecast(h = "3 years")
fc

fc %>%
  filter(Region == "Snowy Mountains") %>%
  autoplot(tourism, level = NULL) +
  xlab("Year") + ylab("Overnight trips (thousands)")


# Forecast accuracy calculations ------------------------------------------
train <- tourism %>%
  filter(year(Quarter) <= 2014)

fit <- train %>%
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips),
    snaive = SNAIVE(Trips)
  ) %>%
  mutate(mixed = (ets + arima + snaive) / 3)

fc <- fit %>% forecast(h = "3 years")
fc %>%
  filter(Region == "Snowy Mountains") %>%
  autoplot(tourism, level = NULL)

accuracy(fc, tourism)

fc_accuracy <- accuracy(fc, tourism,
                        measures = list(
                          point_accuracy_measures,
                          interval_accuracy_measures,
                          distribution_accuracy_measures
                        )
)
fc_accuracy %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE),
    MAE = mean(MAE),
    MASE = mean(MASE),
    Winkler = mean(winkler),
    CRPS = mean(CRPS)
  ) %>%
  arrange(RMSE)


# Moving from forecast to fable -------------------------------------------


