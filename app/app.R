library(shiny)
library(dplyr)
library(forecast)
library(ggplot2)
library(lubridate)

df <- read.csv("demo_hist_df.csv")

stressLevel.train <- ts(df %>%
  filter(X < "2022-09-09") %>%
  select(Historical.Stress))

model <- arima(stressLevel.train,
  order = c(4, 0, 1),
  seasonal = list(
    order = c(0, 1, 1),
    period = 24
  )
)

# Define UI for the app
ui <- fluidPage(
  # App title
  titlePanel("Forecasting stress by Team Chernoff 😎"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "metric", "Metrics:",
        list(
          "Stress" = "stress level"
        )
      ),
      numericInput("ahead", "Hours to forecast:", 48),
      submitButton("Update view")
    ),
    mainPanel(
      h2(textOutput("panelTitle")),
      tabsetPanel(
        tabPanel("Actual plot", plotOutput("actualPlot")),
        tabPanel("Forecasted plot", plotOutput("forecastPlot")),
        tabPanel("Overlay plot", plotOutput("overlayPlot",
          dblclick = "overlay_dblclick",
          brush = brushOpts(
            id = "overlay_brush",
            resetOnNew = TRUE
          )
        )),
        tabPanel("Model Summary", dataTableOutput("summary"))
      )
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  activeDataset <- reactive({
    return(switch(input$metric,
      "heart rate" = {
        s1
      },
      "stress level" = {
        stressLevel.train
      },
      "body battery" = {
        bodyBattery
      }
    ))
  })

  TestDataset <- reactive({
    stressLevel.test <- ts(df %>%
                             filter(X >= "2022-09-09",
                                    X <= ymd("2022-09-09") + hours(input$ahead+4)) %>%
                             select(Historical.Stress),
                           start = 192
    )
    return(stressLevel.test)
  })

  output$panelTitle <- renderText({
    paste("Plotting ", input$metric)
  })

  output$actualPlot <- renderPlot({
    plot.ts(activeDataset())
  })

  output$forecastPlot <- renderPlot({
    autoplot(forecast(model, h = input$ahead)) +
      labs(x = NULL, y = input$metric, title = "ARIMA Model Forecast") +
      theme_light()
  })

  ranges <- reactiveValues(x = NULL, y = NULL)

  output$overlayPlot <- renderPlot({
    autoplot(forecast(model, h = input$ahead)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
      autolayer(TestDataset(), series = input$metric) +
      guides(
        colour = guide_legend(title = "Actual values"),
        fill = guide_legend(title = "Prediction interval")
      ) +
      labs(x = NULL, y = input$metric, title = "ARIMA Model Forecast + Actual") +
      theme_light()
  })

  output$summary <- renderDataTable({
    cbind(
      Metric = rownames(t(accuracy(model))),
      t(accuracy(model))
    )
  })

  observeEvent(input$overlay_dblclick, {
    brush <- input$overlay_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
