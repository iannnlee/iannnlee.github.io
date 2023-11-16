# Load libraries
library(shiny)
library(ggplot2)

# Read data
clean_data3 <- read.csv("clean_data3.csv")
clean_data4 <- read.csv("clean_data4.csv")

# Rename columns
colnames(clean_data3) <- c("Year", "Below_Secondary", "Secondary", "Post_Secondary", "Diploma_Professional_Qualification", "University")
colnames(clean_data4) <- c("Year", "Below_Secondary", "Secondary", "Post_Secondary", "Diploma_Professional_Qualification", "University")

# Define legend_order
legend_order <- c("Below_Secondary", "Secondary", "Post_Secondary", "Diploma_Professional_Qualification", "University")

# Love-themed color palette
love_colors <- c("#E4CDD3", "#E48397", "#E24767", "#B51A3A", "#5E081E")

# Define UI
ui <- fluidPage(
  titlePanel("Education Level vs Singlehood"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age_group", "Select Age Group", c("30-39", "40-49")),
      selectizeInput("education_type", "Select Education Type", choices = legend_order, multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Function to generate plots
  generate_plot <- function(data, age_group, education_types) {
    ggplot(data, aes(x = Year)) +
      lapply(education_types, function(education_type) {
        geom_line(aes(y = get(education_type), color = education_type, group = education_type))
      }) +
      labs(
        title = paste("How does Education level affect Singlehood?\n(", age_group, " Years Old)"),  
        x = "Year",
        y = "% of Population, Single"
      ) +
      scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1)) +
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
  
  # Render plot1
  output$plot1 <- renderPlot({
    age_group <- input$age_group
    education_types <- input$education_type
    data <- switch(age_group,
                   "30-39" = clean_data3,
                   "40-49" = clean_data4)
    generate_plot(data, age_group, education_types)
  })
}

# Run the app
shinyApp(ui, server)
