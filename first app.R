library(shiny)

# Define UI for the app
ui <- fluidPage(
  
  # App title
  titlePanel("interactive app with a slider and a scatter plot."),
  
  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "num",
                  label = "Choose a number of points:",
                  min = 1,
                  max = 100,
                  value = 50)
    ),
    
    # Show a plot of the generated data
    mainPanel(
      plotOutput(outputId = "scatterPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Create scatter plot based on slider input
  output$scatterPlot <- renderPlot({
    # Generate random data based on slider value
    data <- data.frame(x = rnorm(input$num), y = rnorm(input$num))
    plot(data$x, data$y, main = "Scatter Plot", xlab = "X Axis", ylab = "Y Axis")
  })
}

# Run the application
shinyApp(ui = ui, server = server)


