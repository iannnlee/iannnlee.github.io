library(shiny)
library(ggplot2)

clean_data3 <- read.csv("clean_data3.csv")
clean_data4 <- read.csv("clean_data4.csv")

colnames(clean_data3) <- c("Year", "Below Secondary", "Secondary", "Post-Secondary (Non-Tertiary)", "Diploma & Professional Qualification", "University")
colnames(clean_data4) <- c("Year", "Below Secondary", "Secondary", "Post-Secondary (Non-Tertiary)", "Diploma & Professional Qualification", "University")

legend_order <- c("Below Secondary", "Secondary", "Post-Secondary (Non-Tertiary)", "Diploma & Professional Qualification", "University")

love_colors <- c("#E4CDD3", "#E48397", "#E24767", "#B51A3A", "#5E081E")

ui <- fluidPage(
  titlePanel("Highest Qualification Attained vs Singlehood"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age_group", "Select Age Group", c("30-39", "40-49")),
      checkboxGroupInput("education_type", "Select Highest Qualification Attained", choices = legend_order, selected = character(0))
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- function(input, output) {
  
  generate_plot <- function(data, age_group, education_types) {
    ggplot(data, aes(x = Year)) +
      lapply(education_types, function(education_type) {
        geom_line(aes(y = get(education_type), color = education_type, group = education_type))
      }) +
      labs(
        title = paste("How does Highest Qualification Attained affect Singlehood?\n(", age_group, "Years Old)"),  
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