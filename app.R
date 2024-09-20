#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(countrycode)
library(DT)  # For the data table

# Load the data
data <- read_csv("NAFTA_pesticide_use_2024.csv")

# Convert the area to country codes and standardize naming
data_coded <- data %>%
  mutate(
    area = ifelse(area == "United States of America", "United States", area),  # Standardize naming
    country_code = countrycode(area, origin = "country.name", destination = "iso3c")
  )

# Load the world shapefile

NAFTA_shapefile <- st_read("north_america_boundaries.geojson")


# Filter shapefile for NAFTA countries
#NAFTA_countries <- c("Canada", "United States", "Mexico")
#NAFTA_shapefile <- NAFTA_shapefile %>%
#  filter(shapeName %in% NAFTA_countries)

# Join data with the shapefile
merged_data <- NAFTA_shapefile %>%
  left_join(data_coded, by = c("name_long" = "area"))

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("NAFTA Pesticide Use Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Countries:",
                  choices = unique(data_coded$area),  # Use standardized country names
                  selected = unique(data_coded$area),
                  multiple = TRUE),
      
      selectInput("element", "Select Element:",
                  choices = unique(data$element),
                  selected = "Agricultural Use"),
      
      selectInput("item", "Select Item:",
                  choices = unique(data$item),
                  selected = "Pesticides (total)"),
      
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Trend", plotOutput("lineGraph")),
        tabPanel("Data Table", DTOutput("dataTable"))  # Add data table tab
      )
    )
  )
)

# Define the Shiny server
server <- function(input, output, session) {
  
  # Reactive data filtered by selected countries, element, and item for all years
  filtered_data_all_years <- reactive({
    data_coded %>%
      filter(
        area %in% input$country,
        element == input$element,
        item == input$item
      )
  })
  
  # Reactive data filtered by selected countries, element, item, and year 2022 for the map
  filtered_data_2022 <- reactive({
    filtered_data_all_years() %>%
      filter(year == 2022)
  })
  
  # Prepare country polygons for Leaflet, merged with pesticide data for 2022
  map_data <- reactive({
    NAFTA_shapefile %>%
      left_join(filtered_data_2022(), by = c("name_long" = "area"))
  })
  
  # Create a more dramatic color palette from yellow to red
  color_pal <- reactive({
    colorNumeric(
      palette = colorRampPalette(c("yellow", "orange", "red"))(100), 
      domain = filtered_data_2022()$value, 
      na.color = "transparent"
    )
  })
  
  # Render the map showing data for 2022
  output$map <- renderLeaflet({
    leaflet(map_data()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color_pal()(value),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste("Country: ", name_long, "<br>",
                       "Year: ", year, "<br>",
                       "Pesticide Use: ", value, " ", unit)
      ) %>%
      addLegend(pal = color_pal(), values = ~value, 
                title = "Pesticide Use",
                position = "topright")
  })
  
  # Render the line graph showing data for all years
  output$lineGraph <- renderPlot({
    ggplot(filtered_data_all_years(), aes(x = year, y = value, color = area)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Pesticide Use Over Time",
           x = "Year",
           y = paste("Pesticide Use (", unique(filtered_data_all_years()$unit), ")", sep = ""),
           color = "Country") +
      theme_minimal()
  })
  
  # Render the data table showing data for all years
  output$dataTable <- renderDT({
    datatable(filtered_data_all_years())
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "country", selected = unique(data_coded$area))
    updateSelectInput(session, "element", selected = "Agricultural Use")
    updateSelectInput(session, "item", selected = "Pesticides (total)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
