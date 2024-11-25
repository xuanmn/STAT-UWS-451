library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)

# Load greenhouse gas emissions data
ghg_data <- read.csv("/Users/jadenjensen/Desktop/UW_Classes/D.Fall2024/Cse451/data/ghg_emissions.csv", skip = 4)

countries_of_interest <- c("United States", "China", "Russian Federation", "Brazil", "United Kingdom")
years_ghg <- paste0("X", 2002:2022)

ghg_selected_data <- ghg_data %>%
  filter(Country.Name %in% countries_of_interest) %>%
  select(Country.Name, all_of(years_ghg))
colnames(ghg_selected_data) <- gsub("^X", "", colnames(ghg_selected_data))

ghg_selected_data_long <- ghg_selected_data %>%
  pivot_longer(cols = -Country.Name, names_to = "Year", values_to = "Emissions") %>%
  mutate(Year = as.integer(Year))

# Load renewable energy data
renewable_file_path <- "/Users/jadenjensen/Desktop/UW_Classes/D.Fall2024/Cse451/data/API_EG.FEC.RNEW.ZS_DS2_en_csv_v2_223.csv"
renewable_data <- read_csv(renewable_file_path, skip = 4)

years_renewable <- as.character(2003:2023)
renewable_selected_data <- renewable_data %>%
  filter(`Indicator Code` == "EG.FEC.RNEW.ZS" & `Country Name` %in% countries_of_interest) %>%
  select(`Country Name`, all_of(years_renewable)) %>%
  pivot_longer(cols = all_of(years_renewable), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(Year))

server <- function(input, output) {
  
  # Reactive function for GHG emissions
  filtered_ghg_data <- reactive({
    ghg_selected_data_long %>%
      filter(Country.Name %in% input$country) %>%
      group_by(Country.Name) %>%
      mutate(TotalEmissions = sum(Emissions, na.rm = TRUE)) %>%
      arrange(desc(TotalEmissions)) %>%
      ungroup()
  })
  
  # Reactive function for renewable energy data
  filtered_renewable_data <- reactive({
    renewable_selected_data %>%
      filter(`Country Name` %in% input$country) %>%
      group_by(`Country Name`) %>%
      arrange(desc(Value))  # Order by highest consumption
  })
  
  # Greenhouse gas emissions plot
  output$pollutionPlot <- renderPlot({
    data_to_plot <- filtered_ghg_data()
    
    ggplot(data_to_plot, aes(x = Year, y = Emissions, color = Country.Name)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c("United States" = "blue", "China" = "orange", 
                                    "Russian Federation" = "red", 
                                    "Brazil" = "cyan", "United Kingdom" = "purple")) +
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
  })
  
  # Renewable energy consumption plot
  output$renewablePlot <- renderPlot({
    data_to_plot <- filtered_renewable_data()
    
    ggplot(data_to_plot, aes(x = Year, y = Value, color = reorder(`Country Name`, -Value))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c("United States" = "blue", "China" = "orange", 
                                    "Russian Federation" = "red", 
                                    "Brazil" = "cyan", "United Kingdom" = "purple")) +
      labs(
        title = "Renewable Energy Consumption Over Time",
        x = "Year (2003-2023)",
        y = "Renewable Energy Consumption (%)",
        color = "Country"
      ) +
      theme_minimal() +
      theme(
        legend.title = element_text(face = "bold"),
        legend.position = "right"
      )
  })
  
  # Bar chart for renewable energy in 2023
  output$renewableBarChart <- renderPlot({
    data_2023 <- renewable_selected_data %>%
      filter(Year == 2023 & `Country Name` %in% input$country) %>%
      arrange(Value)  # Order from lowest to highest for diagonal labels
    
    ggplot(data_2023, aes(x = reorder(`Country Name`, Value), y = Value, fill = `Country Name`)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("United States" = "blue", "China" = "orange", 
                                   "Russian Federation" = "red", 
                                   "Brazil" = "cyan", "United Kingdom" = "purple")) +
      labs(
        title = "Renewable Energy Consumption in 2023",
        x = "Country",
        y = "Renewable Energy Consumption (%)",
        fill = "Country"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)  # Diagonal labels
      )
  })
  
  output$summaryText <- renderText({
    data_summary <- filtered_ghg_data() %>%
      group_by(Country.Name) %>%
      summarise(TotalEmissions = sum(Emissions, na.rm = TRUE)) %>%
      arrange(desc(TotalEmissions))
    
    paste("Summary of Total Emissions (Million metric tons):\n", 
          paste(data_summary$Country.Name, data_summary$TotalEmissions, sep = ": ", collapse = "\n"))
  })
}
