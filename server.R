library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)

data <- read.csv("/Users/jadenjensen/Desktop/UW_Classes/D.Fall2024/Cse451/data/ghg_emissions.csv", skip = 4)

countries_of_interest <- c("United States", "China", "Russian Federation", "Brazil", "United Kingdom")
years <- paste0("X", 2002:2022)

selected_data <- data %>%
  filter(Country.Name %in% countries_of_interest) %>%
  select(Country.Name, all_of(years))
colnames(selected_data) <- gsub("^X", "", colnames(selected_data))

selected_data_long <- selected_data %>%
  pivot_longer(cols = -Country.Name, names_to = "Year", values_to = "Emissions") %>%
  mutate(Year = as.integer(Year))

server <- function(input, output) {
  
  filtered_data <- reactive({
    selected_data_long %>%
      filter(Country.Name %in% input$country) %>%
      group_by(Country.Name) %>%
      mutate(TotalEmissions = sum(Emissions, na.rm = TRUE)) %>%
      arrange(desc(TotalEmissions)) %>%  # Order by highest to lowest emissions
      ungroup()
  })
  
  output$pollutionPlot <- renderPlot({
    data_to_plot <- filtered_data()
    
    if (input$viewType == "Line Chart") {
      ggplot(data_to_plot, aes(x = Year, y = Emissions, color = reorder(Country.Name, -TotalEmissions))) +
        geom_line(linewidth = 1) +
        labs(
          title = "Greenhouse Gas Emissions Over Time",
          x = "Year (2002-2022)",
          y = "Emissions (Million metric tons of CO2 equivalent)",
          color = "Country"
        ) +
        theme_minimal() +
        theme(
          legend.title = element_text(face = "bold"),
          legend.position = "right"
        )
    } else {
      ggplot(data_to_plot, aes(x = reorder(Country.Name, Emissions), y = Emissions, fill = reorder(Country.Name, Emissions))) +
        geom_bar(stat = "identity") +
        labs(
          title = "Total Greenhouse Gas Emissions by Country from 2002-2022",
          x = "Country",
          y = "Emissions (Million metric tons of CO2 equivalent)",
          fill = "Country"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),  # Diagonalized labels
          legend.position = "none"
        )
    }
  })
  
  output$summaryText <- renderText({
    data_summary <- filtered_data() %>%
      group_by(Country.Name) %>%
      summarise(TotalEmissions = sum(Emissions, na.rm = TRUE)) %>%
      arrange(desc(TotalEmissions))  # Order by highest to lowest emissions
    
    paste("Summary of Total Emissions (Million metric tons):\n", 
          paste(data_summary$Country.Name, data_summary$TotalEmissions, sep = ": ", collapse = "\n"))
  })
}

