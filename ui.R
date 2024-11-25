library(shiny)

ui <- fluidPage(
  titlePanel("Global Air Pollution Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = "Select a Country:",
        choices = c("United States", "China", "Russian Federation", "Brazil", "United Kingdom"),
        selected = "United States",
        multiple = TRUE
      ),
      selectInput(
        inputId = "viewType",
        label = "Select View Type:",
        choices = c("Line Chart", "Bar Chart"),
        selected = "Line Chart"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("pollutionPlot"), 
                 plotOutput("renewablePlot")  # Added renewable energy plot
        ),
        tabPanel("Summary", textOutput("summaryText"))
      )
    )
  )
)
