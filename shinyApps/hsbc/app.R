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
        # select time range to display 
        sliderInput("n",
                     "Number of Days",
                     value = c(153, 253),
                     min = 1,
                     max = 253
                     ),
        # days for prediction ahead
        numericInput("h", "Days to predict", value = 10),
        
        # add options for prediction method
        radioButtons("model", "Model to select",
                     choices = c("naive", "ARIMA", "NeuralNet"),
                     choiceValues = "ARIMA")
         
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
     p1 <- autoplot(ts(stock[input$n[1]:input$n[2], "Close"])) +
       ggtitle("Close Prices in 12 Month")
  
     end = dim(stock)[1]
     start = end - 100
     
     if (input$model == "naive"){
       mod <- naive(stock[start : end, "Close"])
     } else if (input$model == "ARIMA"){
       mod <- auto.arima(stock[start : end, "Close"])
     } else {
       mod <- nnetar(stock[start : end, "Close"])
     }
     data <- forecast(mod, h = input$h)
     p2 <- autoplot(forecast(mod, h = input$h)) + 
       ggtitle("Forecast for next 10 Days based on past 100 Days Price")
     
     grid.arrange(p1, p2, ncol=1)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

