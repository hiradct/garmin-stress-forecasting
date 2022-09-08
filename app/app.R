library(shiny)

# Define UI for the app
ui <- fluidPage(
  # App title
  titlePanel("Forecasting Garmin by Team Chernoff 😎"),
  br(),
  sidebarLayout(
    sidebarPanel(
    selectInput("metric", "Metrics:",
                  list("Heart Rate" = "heart rate", 
                       "Stress" = "stress level",
                       "Body Battery" = "body battery")),
      numericInput("ahead", "Hours (STC) to forecast:", 1),
      
      submitButton("Update view")
    ),
    mainPanel(
      h2(textOutput("panelTitle")),
      tabsetPanel(
        tabPanel("Actual plot", plotOutput("actualPlot")), 
        tabPanel("Forecasted plot", plotOutput("forecastPlot")), 
        tabPanel("Overlay plot", plotOutput("overlayPlot")), 
        tabPanel("Model Summary", dataTableOutput("summary"))
    ))
  )
)

# Define server logic for the app
server <- function(input, output) {
  output$panelTitle <- renderText({
    paste("Plotting ", input$metric)
  })
}


# Create Shiny app
shinyApp(ui = ui, server = server)
