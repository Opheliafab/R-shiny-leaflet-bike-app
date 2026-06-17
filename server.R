# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)

# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")

test_weather_data_generation <- function(){
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df) > 0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row containing city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII, LAT, LNG, BIKE_PREDICTION, BIKE_PREDICTION_LEVEL, LABEL, DETAILED_LABEL, FORECASTDATETIME, TEMPERATURE) %>%
    summarize(count = n(), 
              max = max(BIKE_PREDICTION, na.rm = TRUE)) %>%
    ungroup()  # Ensure the result is still a data frame
  
  print(cities_max_bike)
  
  # Function to get map color based on bike prediction level
  myFirstFun <- function(cities_max_bike_row) {
    if (cities_max_bike_row$BIKE_PREDICTION_LEVEL == 'small') {
      mapcol = "green"
    } else if (cities_max_bike_row$BIKE_PREDICTION_LEVEL == 'medium') {
      mapcol = "yellow"
    } else {
      mapcol = "red"
    }
    return(mapcol)
  }
  
  # Function to get map radius based on bike prediction level
  myFirstFun1 <- function(cities_max_bike_row) {
    if (cities_max_bike_row$BIKE_PREDICTION_LEVEL == 'small') {
      mapradius = 6
    } else if (cities_max_bike_row$BIKE_PREDICTION_LEVEL == 'medium') {
      mapradius = 10
    } else {
      mapradius = 12
    }
    return(mapradius)
  }
  
  output$city_bike_map <- renderLeaflet({
    # Render the leaflet map with circle markers based on bike prediction levels
    map <- leaflet(data = cities_max_bike) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = cities_max_bike$LNG, 
        lat = cities_max_bike$LAT,
        color = sapply(1:nrow(cities_max_bike), function(i) myFirstFun(cities_max_bike[i, ])),
        radius = sapply(1:nrow(cities_max_bike), function(i) myFirstFun1(cities_max_bike[i, ])),
        popup = cities_max_bike$LABEL
      )
    map
  })
  
  # Reactive data filtered based on selected city
  filteredData <- reactive({
    req(input$city_dropdown)  # Ensure input$city_dropdown is available before proceeding
    if (input$city_dropdown == "All") {
      return(cities_max_bike)
    } else {
      return(cities_max_bike %>% filter(CITY_ASCII == input$city_dropdown))
    }
  })
  
  filteredData1 <- reactive({
    req(input$city_dropdown)  # Ensure input$city_dropdown is available before proceeding
    if (input$city_dropdown == "All") {
      return(city_weather_bike_df)
    } else {
      return(city_weather_bike_df %>% filter(CITY_ASCII == input$city_dropdown))
    }
  })
  
  observeEvent(input$city_dropdown, {
    if (input$city_dropdown == 'All') {
      output$city_bike_map <- renderLeaflet({
        map <- leaflet(data = cities_max_bike) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = cities_max_bike$LNG, 
            lat = cities_max_bike$LAT,
            color = sapply(1:nrow(cities_max_bike), function(i) myFirstFun(cities_max_bike[i, ])),
            radius = sapply(1:nrow(cities_max_bike), function(i) myFirstFun1(cities_max_bike[i, ])),
            popup = cities_max_bike$LABEL
          )
        map
      })
      
      # Time transformation
      t.str <- strptime(city_weather_bike_df$FORECASTDATETIME, "%Y-%m-%d %H:%M:%S")
      h.str <- as.numeric(format(t.str, "%H"))
      t1.str <- as.Date(city_weather_bike_df$FORECASTDATETIME)
      
      # Temperature plot
      output$temp_line <- renderPlot({
        ggplot(data = city_weather_bike_df, aes(x = h.str, y = TEMPERATURE)) +
          geom_point(na.rm = TRUE) +
          geom_line(size = 1.5, color = "yellow") +
          geom_text(aes(label = TEMPERATURE)) +
          labs(x = "Time (3 hrs ahead)", y = "Temperature")
      })
      
      # Bike demand plot
      output$bike_line <- renderPlot({
        ggplot(data = city_weather_bike_df, aes(x = t1.str, y = BIKE_PREDICTION)) +
          geom_point(na.rm = TRUE) +
          geom_line(size = 1.5, color = "blue", linetype = "dashed") +
          geom_text(aes(label = BIKE_PREDICTION)) +
          labs(x = "Time (3 hrs ahead)", y = "Predicted Bike Count")
      })
      
      output$bike_date_output <- renderText({
        paste("Bike Prediction =", input$plot_click$y, "\nTime =", input$plot_click$x)
      })
      
      # Humidity prediction plot
      output$humidity_pred_chart <- renderPlot({
        ggplot(data = city_weather_bike_df, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point(na.rm = TRUE) +
          geom_smooth(method = "lm", formula = y ~ poly(x, 4), col = "red", se = FALSE) +
          labs(x = "Humidity", y = "Bike Prediction")
      })
    } else {
      # Render specific city map
      output$city_bike_map <- renderLeaflet({
        map <- leaflet(data = filteredData()) %>%
          addTiles() %>%
          addMarkers(
            lng = filteredData()$LNG, 
            lat = filteredData()$LAT,
            popup = filteredData()$DETAILED_LABEL
          )
        map
      })
      
      # Time transformation for filtered data
      t.str <- strptime(filteredData1()$FORECASTDATETIME, "%Y-%m-%d %H:%M:%S")
      h.str <- as.numeric(format(t.str, "%H"))
      t1.str <- as.Date(filteredData1()$FORECASTDATETIME)
      
      # Temperature plot for specific city
      output$temp_line <- renderPlot({
        ggplot(data = filteredData1(), aes(x = h.str, y = TEMPERATURE)) +
          geom_point(na.rm = TRUE) +
          geom_line(size = 1.5, color = "yellow") +
          geom_text(aes(label = TEMPERATURE)) +
          labs(x = "Time (3 hrs ahead)", y = "Temperature")
      })
      
      # Bike demand plot for specific city
      output$bike_line <- renderPlot({
        ggplot(data = filteredData1(), aes(x = t1.str, y = BIKE_PREDICTION)) +
          geom_point(na.rm = TRUE) +
          geom_line(size = 1.5, color = "blue", linetype = "dashed") +
          geom_text(aes(label = BIKE_PREDICTION)) +
          labs(x = "Time (3 hrs ahead)", y = "Predicted Bike Count")
      })
      
      output$bike_date_output <- renderText({
        paste("Bike Prediction =", input$plot_click$y, "\nTime =", input$plot_click$x)
      })
      
      # Humidity prediction plot for specific city
      output$humidity_pred_chart <- renderPlot({
        ggplot(data = filteredData1(), aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point(na.rm = TRUE) +
          geom_smooth(method = "lm", formula = y ~ poly(x, 4), col = "red", se = FALSE) +
          labs(x = "Humidity", y = "Bike Prediction")
      })
    }
  })
})
