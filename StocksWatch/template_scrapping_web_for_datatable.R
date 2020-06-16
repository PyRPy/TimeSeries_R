# scrapping the web -------------------------------------------------------

# install.packages("rvest")
library(rvest)
library(quantmod)

link <-paste0("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
webpage <- read_html(link)
data <- html_nodes(webpage,".wikitable")
table<- html_table(data[[1]],header = TRUE)
# write.csv(table, 'symbols500.csv')

head(table)
symbols500 <- table[c("Symbol", "Security", "GICS Sector")]
head(symbols500)
class(symbols500) # still a data.frame in R

numberStocks <- nrow(symbols500)
idx <- sample(1:numberStocks, 5)
idx
sampledStocks <- symbols500[idx, "Symbol"]
sampledStocks
symbols500[idx, ]


stocksEnv <- new.env()
getSymbols(sampledStocks, env = stocksEnv, from = "2015-01-01", src = "yahoo")


for (stock in sampledStocks) {
  lineChart(stocksEnv[[stock]])
  # addSMA(n = 50, col = "blue")
  # addSMA(n = 200, col = "red")
}

symbols500[idx, ]
