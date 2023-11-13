library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Education Level vs Singlehood"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age_group", "Select Age Group", c("30-39", "40-49")),
      selectInput("education_level", "Select Education Level", c("Below Secondary", "Secondary", "Post Secondary..Non.Tertiary.", "Diploma...Professional.Qualification", "University"))
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Function to generate plots
  generate_plot <- function(data, age_group, education_level) {
    ggplot(data, aes(x = Year)) +
      geom_line(aes(y = .data[[education_level]], color = education_level, group = 1)) +
      labs(
        title = paste("How does Education level affect Singlehood?\n(", age_group, " Years Old)"),  
        x = "Year",
        y = "% of Population, Single"
      ) +
      scale_color_manual(values = setNames(love_colors, legend_order),
                         breaks = legend_order) +
      theme_void() +  
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.margin = margin(b = 40, t = 20),
        axis.title.y = element_text(size = 10, angle = 90, margin = margin(r = 10)),
        axis.title.x = element_text(size = 10, angle = 0, margin = margin(r = 10)),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.1, 'cm')
      )
  }
  ?to
  
  # Render plot1
  output$plot1 <- renderPlot({
    age_group <- input$age_group
    education_level <- input$education_level
    data <- switch(age_group,
                   "30-39" = clean_data3,
                   "40-49" = clean_data4)
    generate_plot(data, age_group, education_level)
  })
}

# Run the app
shinyApp(ui, server)
