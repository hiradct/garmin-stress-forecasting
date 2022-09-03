library(shiny)

# Define UI for the app
ui <- fluidPage(
  # App title
  navbarPage("Team Chernoff",
             tabPanel("Charts"),
             tabPanel("Model"),
             tabPanel("Summary"),
             tabPanel("Other")
  )
)

# Define server logic for the app
server <- function(input, output) {
}

# Create Shiny app
shinyApp(ui = ui, server = server)
