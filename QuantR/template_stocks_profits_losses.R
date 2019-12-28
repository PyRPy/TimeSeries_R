# stocks profits and losses

# profits and losses ------------------------------------------------------

library(tidyverse)
library(tidyquant)
stock_symbols <- c("AAPL", "MSFT", "NFLX", "TSLA")

stock_prices <- tq_get(stock_symbols, from="2010-01-01") %>% 
                select(symbol, date, close)
stock_prices
head(stock_prices)
tail(stock_prices)

# theme
theme_set(theme_tq())
ggplot(stock_prices, aes(date, close, color=symbol)) +
  geom_line() +
  theme_tq()

# orgnize data
library(tidyr)
stock_prices %>% 
  inner_join(stock_prices, by="symbol",
             suffix = c("", "_future"))

stock_prices %>% 
  inner_join(stock_prices,
             by = "symbol",
             suffix = c("", "_future")) %>% 
  filter(date_future >= date) %>% 
  mutate(change = close_future - close) %>% 
  group_by(symbol) %>% 
  summarize(largest_gain = max(change),
            largest_loss = min(change))

# cummean and cummax
stock_prices %>% 
  arrange(symbol, desc(date)) %>% 
  group_by(symbol) %>% 
  mutate(highest_future = cummax(close), lowest_future =  cummin(close))

stock_prices %>% 
  arrange(symbol, desc(date)) %>% 
  group_by(symbol) %>% 
  summarize(highest_gain = max(cummax(close) - close), 
            biggest_loss = min(cummin(close) - close))

# lags
stock_prices %>% 
  arrange(symbol, desc(date)) %>% 
  group_by(symbol) %>% 
  mutate(highest_future = lag(cummin(close), 100),
         lowest_future = lag(cummin(close), 100)) %>% 
  summarise(highest_gain = max(highest_future - close, na.rm = TRUE),
            biggest_loss = min(lowest_future - close, na.rm = TRUE))

biggest_changes <- stock_prices %>% 
  crossing(N = seq(0, 1000, 10)) %>% 
  arrange(N, symbol, desc(date)) %>% 
  group_by(N, symbol) %>% 
  mutate(highest_future = lag(cummax(close), N[1]), 
         lowest_future = lag(cummin(close), N[1])) %>% 
  summarize(highest_gain = max(highest_future - close, na.rm = TRUE),
            biggest_loss = min(lowest_future - close, na.rm = TRUE))

biggest_changes %>% 
  ggplot(aes(N, biggest_loss, color = symbol)) +
  geom_line() +
  labs(x = "Gap that you have to wait before selling a stock",
       y = "Biggest loss possible in 2010-2019")
  