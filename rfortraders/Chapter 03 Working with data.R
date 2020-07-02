# CHAPTER 3 Working with Data

#######################
# Getting data into R #
#######################
# In Windows
aapl <- read.table("clipboard")

# On Mac/Linux
# aapl <- read.table(pipe("pbpaste"))

head(aapl)
##     V1
## 1 104.08
## 2 110.26
## 3  96.80
## 4  88.74
## 5  89.79
## 6  89.16

class(aapl)
## [1] "data.frame"

aapl <- aapl[rev(rownames(aapl)), , drop = FALSE]

prices <- aapl$V1
plot(prices, main = "AAPL plot", type = 'l')

# Load the .csv file
aapl_2 <- read.csv(file = "Chapter_03/aapl.csv", header = TRUE, stringsAsFactors = FALSE)

# Reverse the entries
aapl_2 <- aapl_2[rev(rownames(aapl_2)), ]

aapl_close <- aapl_2[, "Close"]

summary(aapl_close)
## Min.   1st Qu. Median   Mean   3rd Qu.  Max.
## 12.94   24.69   38.13   46.80   53.61  199.80

############################
# Installing packages in R #
############################
install.packages(pkgs, lib, repos = getOption("repos"),
  contriburl = contrib.url(repos, type),
  method, available = NULL, destdir = NULL,
  dependencies = NA, type = getOption("pkgType"),
  configure.args = getOption("configure.args"),
  configure.vars = getOption("configure.vars"),
  clean = FALSE, Ncpus = getOption("Ncpus", 1L),
  verbose = getOption("verbose"),
  libs_only = FALSE, INSTALL_opts, quiet = FALSE,
  keep_outputs = FALSE, ...)

#################################
# Storing and transmitting data #
#################################
{
  "CVX":
  {
    "Currency": "USD",
    "Sector": "Basic Materials",
    "Industry": "Major Integrated Oil & Gas"
  },
  "GOOG":
  {
    "Currency": "USD",
    "Sector": "Technology",
    "Industry": "Internet Information Providers"
  } 
}

# Install and load the package
# install.packages("RJSONIO")
library(RJSONIO)

# Read the file
out <- fromJSON(content = "Chapter_03/sample_json_file.json" )

# Look at the structure of the resulting object
str(out)
## List of 2
## $ CVX : Named chr [1:3] "USD" "Basic Materials...
##  ..- attr(*, "names")= chr [1:3] "Currency"...
## $ GOOG: Named chr [1:3] "USD" "Technology"...
##  ..- attr(*, "names")= chr [1:3] "Currency"...

# write.csv(aapl_2, file = "Chapter_03/aapl_2.csv")
# 
# save(aapl_2, file = "Chapter_03/aapl_2.rdata")

aapl_old <- aapl_2
rm(aapl_2)
# load(file = "Chapter_03/aapl_2.rdata")

identical(aapl_old, aapl_2)
## [1] TRUE

######################################
# Extracting data from a spreadsheet #
######################################
# library(XLConnect)
# Create a workbook object
# Needs Java 6 runtime as a default

library(readxl)
signals <- read_excel("Chapter_03/strategy.xlsx", 
                       sheet = "signals", 
                       col_types = c("date", "numeric", "numeric"))

# book <- loadWorkbook("Chapter_03/strategy.xlsx")
# 
# # Convert it into a data frame
# signals <- readWorksheet(book, sheet = "signals", header = TRUE)

signals
##     time signal1 signal2
## 1 08:30:00  0.43   -0.20
## 2 08:31:00  0.54    0.33
## 3 08:32:00  0.32   -0.21

strength <- read_excel("Chapter_03/strategy.xlsx", 
                       sheet = "strength")
# strength <- readWorksheet(book, sheet = "strength", header = TRUE)

strength
## intensity score
## 1 2 7.5
## 2 3 8.4
## 3 6 5.4

# remove the codes that cannot be run 

#####################
# The dplyr package #
#####################
# remove the codes that cannot run

# load the library xts
library(xts)

# Load a small dataset that comes along with xts.
# We could have used our original .csv file as well.
data(sample_matrix)

# Look at the data
head(sample_matrix)
## [1] "matrix"

# What is the type of this object?
class(sample_matrix)
## [1] "matrix"

# Use the str() command to get more details about this object.
str(sample_matrix)
## num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
## - attr(*, "dimnames")=List of 2
## ..$ : chr [1:180] "2007-01-02" "2007-01-03"
## "2007-01-04" "2007-01-05" ...
## ..$ : chr [1:4] "Open" "High" "Low" "Close"

xts_matrix <- as.xts(sample_matrix, descr = 'my new xts object')

str(xts_matrix)
## An 'xts' object on 2007-01-02/2007-06-30 containing: ## Data: num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
## - attr(*, "dimnames")=List of 2
## ..$ : NULL
## ..$ : chr [1:4] "Open" "High" "Low" "Close"
## Indexed by objects of class: [POSIXct,POSIXt] TZ: ## xts Attributes:
## List of 3
## $ tclass: chr [1:2] "POSIXct" "POSIXt"
## $tzone:chr""
## $ descr : chr "my new xts object"

# Simple plot
plot(xts_matrix[,1], main = "Our first xts plot",
 cex.main = 0.8)

# remove the 'candles' part in plot()

range <- "2007-03-15::2007-06-15"
plot(xts_matrix[range]) # corrected to [] from ()

start_date <- "2007-05-05"
end_date <- "2007-12-31"

plot(xts_matrix[paste(start_date, "::",
  end_date, sep = "")])

# Defaults to space separator
paste("Hello", "World", "in R")
## [1] "Hello World in R"

paste("Hello", "Again", sep = "**")
## [1] "Hello**Again"

paste(c(1,2,3,4,5), collapse = "oooo")
## [1] "1oooo2oooo3oooo4oooo5"

# Create a vector of 10 fictitious stock prices along with
# a time index in microsecond resolution.
price_vector <- c(101.02, 101.03, 101.03, 101.04, 101.05,
  101.03, 101.02, 101.01, 101.00, 100.99)

dates <- c("03/12/2013 08:00:00.532123",
  "03/12/2013 08:00:01.982333",
  "03/12/2013 08:00:01.650321",
  "03/12/2013 08:00:02.402321",
  "03/12/2013 08:00:02.540432",
  "03/12/2013 08:00:03.004554",
  "03/12/2013 08:00:03.900213",
  "03/12/2013 08:00:04.050323",
  "03/12/2013 08:00:04.430345",
  "03/12/2013 08:00:05.700123")

# Allow the R console to display the microsecond field
options(digits.secs = 6)

# Create the time index with the correct format
time_index <- strptime(dates, format = "%d/%m/%Y %H:%M:%OS")

# Pass the time index into the its object
xts_price_vector <- xts(price_vector, time_index)

# Plot the price of the fictitious stock
plot(xts_price_vector, main = "Fictitious price series",
  cex.main = 0.8)

# Add a horizontal line where the mean value is
abline(h = mean(xts_price_vector), lwd = 2)

# Add a vertical blue line at a specified time stamp
my_time <- as.POSIXct("03/12/2013 08:00:03.004554",
  format = "%d/%m/%Y %H:%M:%OS")

abline(v = my_time, lwd = 2, lty = 2)

es_price <- c(1700.00, 1700.25, 1700.50, 1700.00, 1700.75,
    1701.25, 1701.25, 1701.25, 1700.75, 1700.50)

es_time  <- c("09/12/2013 08:00:00.532123",
  "09/12/2013 08:00:01.982333",
  "09/12/2013 08:00:05.650321",
  "09/12/2013 08:10:02.402321",
  "09/12/2013 08:12:02.540432",
  "09/12/2013 08:12:03.004554",
  "09/12/2013 08:14:03.900213",
  "09/12/2013 08:15:07.090323",
  "09/12/2013 08:16:04.430345",
  "09/12/2013 08:18:05.700123")

# create an xts time series object
xts_es <- xts(es_price, as.POSIXct(es_time,
  format = "%d/%m/%Y %H:%M:%OS"))

names(xts_es) <- c("price")

time_diff <- difftime(index(xts_es)[2], index(xts_es)[1],
  units = "secs")

time_diff
## Time difference of 1.45021 secs

diffs <- c()
for(i in 2:length(index(xts_es))) {
  diffs[i] <- difftime(index(xts_es)[i], index(xts_es)[i - 1],
    units = "secs")
}

diffs <- index(xts_es)[-1] - index(xts_es)[-length(index(xts_es))]

diffs
## Time differences in secs
## [1]   1.4502099   3.6679881 596.7520001
## [4] 120.1381109   0.4641221 120.8956590
## [7]  63.1901100  57.3400221 121.2697780
## attr(,"tzone")

class(diffs)
## [1] "difftime"

es_times <- index(xts_es)
diffs <- es_times[-1] - es_times[-length(es_times)]

diffs
## Time differences in secs
## [1]   1.4502099   3.6679881 596.7520001
## [4] 120.1381109   0.4641221 120.8956590
## [7]  63.1901100  57.3400221 121.2697780
## attr(,"tzone")

par(mfrow = c(2, 1))
diffs <- as.numeric(diffs)
plot(diffs, main = "Time difference in seconds for ES trades",
  xlab = "", ylab = "Time differences",
  cex.lab = 0.8,
  cex.main = 0.8)
grid()

hist(diffs, main = "Time difference in seconds for ES trades",
  xlab = "Time difference (secs)", ylab = "Observations",
  breaks = 20,
  cex.lab = 0.8,
  cex.main = 0.8)
grid()

##############################
# Using the quantmod package #
##############################
# Load the quantmod packages after installing it locally.
library(quantmod)

AAPL <- getSymbols("AAPL", auto.assign=FALSE)
head(AAPL)

##########################
# Charting with quantmod #
##########################
# Adding some technical indicators on top of the original plot
chartSeries(AAPL, subset='2010::2010-04',
  theme = chartTheme('white'),
  TA = "addVo(); addBBands()")

reChart(subset='2009-01-01::2009-03-03')

chartSeries(AAPL, subset='2011::2012',
  theme = chartTheme('white'),
  TA = "addBBands(); addDEMA()")

addVo()
addDPO()

# Initial chart plot with no indicators
chartSeries(AAPL, theme = chartTheme('white'), TA = NULL)

# Custom function creation
my_indicator <- function(x) {
   return(x + 90)
}

add_my_indicator <- newTA(FUN = my_indicator, preFUN=Cl,
  legend.name = "My Fancy Indicator", on = 1)

add_my_indicator()

#########################
# Graphing wiht ggplot2 #
#########################
# Create a matrix with price and volume
df <- AAPL[, c("AAPL.Adjusted", "AAPL.Volume")]
names(df) <- c("price", "volume")

# Create
df$return <- diff(log(df[, 1]))
df <- df[-1, ]

df$cuts <- cut(abs(df$return),
  breaks = c(0, 0.02, 0.04, 0.25),
  include.lowest = TRUE)

# Create another column for the mean
df$means <- NA
for(i in 1:3) {
  group <- which(df$cuts == i)
  if(length(group) > 0) {
    df$means[group] <- mean(df$volume[group])
  }
}

# Load ggplot2
library(ggplot2)

ggplot(df) +
geom_histogram(aes(x=volume)) +
facet_grid(cuts ~ .) +
geom_vline(aes(xintercept=means), linetype="dashed", size=1)
