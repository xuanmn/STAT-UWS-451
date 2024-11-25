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
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plots", 
          plotOutput("pollutionPlot"), 
          plotOutput("renewablePlot"), 
          plotOutput("airQualityPlot")  # Added air quality plot
        ),
        tabPanel("Summary", textOutput("summaryText"))
      )
    )
  )
)
