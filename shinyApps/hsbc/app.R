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
                     max = 365,
                     value = 365)
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
     with(data = stock[1:input$n, ], 
          plot(Date, Close, type = "l", 
               main = "HSBC Stock",
               xlab = "Up to 12 Months",
               ylab = "$ Close",
               ylim = c(30, 50),
               col = "blue"))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

