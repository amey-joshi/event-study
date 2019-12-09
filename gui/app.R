library(shiny)

source("es.R")

ui <- fluidPage(titlePanel(title = "Event Study"),
                sidebarLayout(
                  sidebarPanel(
                    textInput(inputId = "stockName", label = "Stock Ticker"),
                    textInput(inputId = "indexName", label = "Index Ticker"),
                    dateInput(inputId = "eventDate", label = "Event date"),
                    actionButton(inputId = "submit", "Submit")
                  ),
                  mainPanel(
                    plotOutput(outputId = "pricePlot"),
                    verbatimTextOutput(outputId = "arStats")
                  )
                ))

server <- function(input, output, session) {
  stockTicker <- reactive(input$stockName)
  indexTicker <- reactive(input$indexName)
  eventDate <- reactive(input$eventDate)
  
  observeEvent(input$submit, {
    stock.name <- toupper(stockTicker())
    index.name <- toupper(indexTicker())
    event.date <- eventDate()
    
    WINDOW_SIZE <- 90
    
    startDate <- as.Date(event.date) - WINDOW_SIZE
    endDate <- as.Date(event.date) + WINDOW_SIZE
    
    data.list <- get_closing_data(
      stock.name = stock.name,
      index.name = index.name,
      from = startDate,
      to = endDate
    )
    stock.xts <- data.list[[1]]
    index.xts <- data.list[[2]]
    
    event.time <- which(index(stock.xts) == event.date)
    
    output$pricePlot <-
      renderPlot(
        compare_plot(
          stock.prices = stock.xts,
          index.prices = index.xts,
          stock.name = stock.name,
          index.name = index.name,
          event.time = event.time
        )
      )
    
    bhar.result <- compute_bhar(stock = stock.xts,
                                index = index.xts,
                                event.time = event.time)
    car.result <- compute_car(stock = stock.xts,
                              index = index.xts,
                              event.time = event.time)
    
    stats <- paste(
      " BHAR (before):",
      bhar.result[1],
      "\n",
      "BHAR (after):",
      bhar.result[2],
      "\n",
      "Signficant change in AR:",
      car.result[1],
      "\n",
      "Significant change in CAR:",
      car.result[2]
    )
    
    output$arStats <- renderText(stats)
  })
}

shinyApp(ui = ui, server = server)