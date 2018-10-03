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
   titlePanel("Playing with Shiny"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("cid", "Column", choices = colnames( iris ) ),
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 12)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # pie chart if Species column is selected
      if( input$cid == "Species") {
         counts <- table(iris$Species)
         pie( counts ) 
      } else { 
          
         # generate bins based on input$bins from ui.R
         x <- iris[ , input$cid ] 
         bins <- seq(min(x), max(x), length.out = input$bins + 1)

         # draw the histogram with the specified number of bins
         h <- hist(x, breaks = bins, col = rainbow(10), border = 'white', main = input$cid )
         # draw normal distribution line
         yfit <- dnorm(bins, mean = mean(x), sd = sd(x)) 
         yfit <- yfit *  diff( h$mids[1:2]) * length(x) 
         lines(bins, yfit, col = "blue")
        }
   
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

