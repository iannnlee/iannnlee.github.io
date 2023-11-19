# Load the necessary libraries
library(shiny)
library(ggplot2)
library(tidyr)

# Read the first CSV file
data1 <- read.csv("clean_data5.csv")
colnames(data1) <- c("Year", "Single_30_39", "Non_Single_30_39")
data_long1 <- gather(data1, key = "Status", value = "Percentage", -Year)

# Read the second CSV file
data2 <- read.csv("clean_data6.csv")
colnames(data2) <- c("Year", "Single_40_49", "Non_Single_40_49")
data_long2 <- gather(data2, key = "Status", value = "Percentage", -Year)

# Define the UI
ui <- fluidPage(
  titlePanel("Singles Averages by Age Group"),
  sidebarLayout(
    sidebarPanel(
      selectInput("averageDuration", "Select Average Duration", 
                  choices = c("10-year", "5-year", "2-year"), selected = "10-year"),
      textOutput("averageNumbers")
    ),
    mainPanel(
      plotOutput("comparisonPlot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Function to calculate average percentages
  calculateAverages <- function(data, status, minYear, maxYear) {
    subset_data <- subset(data, Status == status & Year >= minYear & Year <= maxYear)
    return(mean(subset_data$Percentage))
  }
  
  observe({
    input$averageDuration
    updateSelectInput(session, "averageDuration", selected = input$averageDuration)
  })
  
  # Calculate average percentages based on user input
  output$averageNumbers <- renderText({
    if (input$averageDuration == "10-year") {
      minYear <- 2013
      maxYear <- 2022
    } else if (input$averageDuration == "5-year") {
      minYear <- 2018
      maxYear <- 2022
    } else if (input$averageDuration == "2-year") {
      minYear <- 2021
      maxYear <- 2022
    }
    
    average_single_30_39 <- calculateAverages(data_long1, "Single_30_39", minYear, maxYear)
    average_single_40_49 <- calculateAverages(data_long2, "Single_40_49", minYear, maxYear)
    
    paste("Average Single (30-39):", sprintf("%.2f", average_single_30_39), "\n",
          "Average Single (40-49):", sprintf("%.2f", average_single_40_49))
  })
  
  output$comparisonPlot <- renderPlot({
    if (input$averageDuration == "10-year") {
      minYear <- 2013
      maxYear <- 2022
    } else if (input$averageDuration == "5-year") {
      minYear <- 2018
      maxYear <- 2022
    } else if (input$averageDuration == "2-year") {
      minYear <- 2021
      maxYear <- 2022
    }
    
    average_single_30_39 <- calculateAverages(data_long1, "Single_30_39", minYear, maxYear)
    average_single_40_49 <- calculateAverages(data_long2, "Single_40_49", minYear, maxYear)
    
    comparison_data <- data.frame(
      Status = c("Single (30-39)", "Single (40-49)"),
      Average_Percentage = c(average_single_30_39, average_single_40_49)
    )
    
    p <- ggplot(comparison_data, aes(x = Status, y = Average_Percentage, fill = Status)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.2f", Average_Percentage)), position = position_dodge(width = 0.9), vjust = -0.5) +
      labs(title = paste("Comparison of Average Single Status -", input$averageDuration), 
           y = "Average Percentage", x = "Status") +
      scale_fill_manual(values = c("#B19CF9", "#CDB6FA"), labels = c("30-39", "40-49")) +
      scale_x_discrete(labels = c("Single (30-39)", "Single (40-49)")) +
      theme_minimal()
    
    print(p)
  })
}

# Run the Shiny app
shinyApp(ui, server)
