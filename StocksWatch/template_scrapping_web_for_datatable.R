# scrapping the web -------------------------------------------------------

# install.packages("rvest")
library(rvest)

link <-paste0("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
webpage <- read_html(link)
data <- html_nodes(webpage,".wikitable")
table<- html_table(data[[1]],header = TRUE)
# write.csv(table, 'symbols500.csv')
head(table)
symbols500 <- table[c("Symbol", "Security")]

class(symbols500) # still a data.frame in R

numberStocks <- nrow(symbols500)
idx <- sample(1:numberStocks, 10)
idx
sampledStocks <- symbols500[idx, "Symbol"]
sampledStocks

stocksEnv <- new.env()
getSymbols(sampledStocks, env = stocksEnv, from = "2015-01-01", src = "yahoo")


for (stock in sampledStocks) {
  lineChart(stocksEnv[[stock]])
}

symbols500[idx, ]
