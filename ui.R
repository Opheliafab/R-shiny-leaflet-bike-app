# Load required libraries
library(shiny)
library(leaflet)

# Create the Shiny UI
shinyUI(
  fluidPage(
    padding = 5,
    titlePanel("Bike-sharing demand prediction app"), 
    
    # Create a side-bar layout
    sidebarLayout(
      # Main panel to show cities on a leaflet map
      mainPanel(
        # leaflet output with id = 'city_bike_map', height = 1000
        leafletOutput("city_bike_map", width = "100%", height = 1000)
      ),
      
      # Sidebar panel to show detailed plots for a city
      sidebarPanel(
        # Dropdown list to select city
        selectInput(
          inputId = "city_dropdown", 
          label = "Cities:", 
          choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris")
        ),
        
        # Temperature plot
        plotOutput("temp_line", width = "100%", height = "400px"),
        
        # Bike demand plot with click event
        plotOutput("bike_line", width = "100%", height = "400px", click = "plot_click"),
        
        # Bike demand date output
        verbatimTextOutput("bike_date_output"),
        
        # Humidity prediction plot
        plotOutput("humidity_pred_chart", width = "100%", height = "400px")
      )
    )
  )
)
