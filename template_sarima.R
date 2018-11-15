# how to config a season arima
# https://stackoverflow.com/questions/37046275/is-there-a-way-to-force-seasonality-from-auto-arima
set.seed(1)
foo <- ts(rnorm(60),frequency=12)
plot.ts(foo)

auto.arima(foo)

# define D=1
auto.arima(foo,D=1)
