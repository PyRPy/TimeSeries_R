#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("12 Month Stock Price for HSBC"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("n",
                     "Number of Days",
                     min = 1,
                     max = 253,
                     value = 253)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("trendPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$trendPlot <- renderPlot({
     stock <- read.csv("HSBC.csv")
     stock <- stock[c("Date", "Close")]
     stock$Date <- as.Date(stock$Date)
     
     library(fpp2)
     require(gridExtra)
     p1 <- autoplot(ts(stock[1:input$n, "Close"])) +
       ggtitle("Close Prices in 12 Month")
  
     end = dim(stock)[1]
     start = end - 100
     
     arima.hsbc <- auto.arima(stock[start : end, "Close"])
     p2 <- autoplot(forecast(arima.hsbc, h = 10)) + 
       ggtitle("Forecast for next 10 Days based on past 100 Days Price")
     
     grid.arrange(p1, p2, ncol=1)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

