# practical TSA
# install.packages("pageviews") # for data set


# find the data set -------------------------------------------------------

library(pageviews)
df_wiki = article_pageviews(project = "en.wikipedia",
                            article = "Facebook",
                            start = as.Date('2015-11-01'),
                            end = as.Date("2018-11-02"),
                            user_type = c("user"),
                            platform = c("mobile-web"))
colnames(df_wiki)
head(df_wiki)
# write.csv(df_wiki, "wiki_pageviews.csv")

# prepare the data for model input ----------------------------------------

df = df_wiki[, c("date", "views")]
colnames(df) = c("ds", "y")
plot(df, type='l')
df$y = log(df$y) # transform due to large extreme values
plot(df, type="l") # much better view

# make predictions --------------------------------------------------------
# install.packages("prophet")
library(prophet)
m = prophet(df)
future = make_future_dataframe(m, periods = 365)
tail(future)

forecast = predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


# plot the results and inspect --------------------------------------------

plot(df$ds, df$y, col=1, type="l", xlim = range(forecast$ds),
     main = "actual and predicted wikipedia pageviews of 'facebook'")
points(forecast$ds, forecast$yhat, type = "l", col=2)

# decomposition
prophet_plot_components(m, forecast)
