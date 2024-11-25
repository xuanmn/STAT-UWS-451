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

# Load air quality data
air_quality_data <- read.csv("/Users/jadenjensen/Desktop/UW_Classes/D.Fall2024/Cse451/data/aqi.csv")
colnames(air_quality_data) <- as.character(air_quality_data[2, ])
rownames(air_quality_data) <- NULL
air_quality_data <- air_quality_data[-2, ] %>%
  mutate(
    FactValueNumeric = as.numeric(FactValueNumeric),
    Period = as.numeric(Period),
    Location = recode(Location,
                      "United States of America" = "United States",
                      "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom")
  ) %>%
  filter(Location %in% countries_of_interest, Dim1 == "Total") %>%
  select(Location, Period, FactValueNumeric)

server <- function(input, output) {
  
  # Reactive function for GHG emissions
  filtered_ghg_data <- reactive({
    ghg_selected_data_long %>%
      filter(Country.Name %in% input$country) %>%
      group_by(Country.Name) %>%
      mutate(TotalEmissions = sum(Emissions, na.rm = TRUE)) %>%
      ungroup()
  })
  
  # Reactive function for renewable energy data
  filtered_renewable_data <- reactive({
    renewable_selected_data %>%
      filter(`Country Name` %in% input$country)
  })
  
  # Reactive function for air quality data
  filtered_air_quality_data <- reactive({
    air_quality_data %>%
      filter(Location %in% input$country)
  })
  
  # Greenhouse gas emissions plot
  output$pollutionPlot <- renderPlot({
    data_to_plot <- filtered_ghg_data()
    
    ggplot(data_to_plot, aes(x = Year, y = Emissions, color = Country.Name)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
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
    
    ggplot(data_to_plot, aes(x = Year, y = Value, color = `Country Name`)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
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
  
  # Air quality plot
  output$airQualityPlot <- renderPlot({
    data_to_plot <- filtered_air_quality_data()
    
    ggplot(data_to_plot, aes(x = Period, y = FactValueNumeric, color = Location, group = Location)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = "Total Concentrations of Fine Particulate Matter (PM2.5)",
        x = "Year",
        y = "PM2.5 Concentration",
        color = "Country"
      ) +
      theme_minimal()
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
