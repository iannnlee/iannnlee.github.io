#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Add UI for user input
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Add UI components for user input. For example, a slider input.
      sliderInput("obs", "Number of observations:", min = 0, max = 1000, value = 500)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Add server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Generate plot. For example, a histogram of a normal distribution.
    x    <- rnorm(input$obs)
    bins <- seq(min(x), max(x), length.out = 50)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)

