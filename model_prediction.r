require(tidyverse)
require(httr)

# Function to fetch weather forecast data by cities
get_weather_forecaset_by_cities <- function(city_names) {
  city <- c()
  weather <- c()
  temperature <- c()
  visibility <- c()
  humidity <- c()
  wind_speed <- c()
  seasons <- c()
  hours <- c()
  forecast_date <- c()
  weather_labels <- c()
  weather_details_labels <- c()
  
  for (city_name in city_names) {
    url_get <- 'https://api.openweathermap.org/data/2.5/forecast'
    api_key <- "2ce80a99a936e6fd675376fa4eecb39a"
    forecast_query <- list(q = city_name, appid = api_key, units = "metric")
    response <- GET(url_get, query = forecast_query)
    json_list <- content(response, as = "parsed")
    results <- json_list$list
    
    for (result in results) {
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      temperature <- c(temperature, result$main$temp)
      visibility <- c(visibility, ifelse(is.null(result$visibility), NA, result$visibility))
      humidity <- c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      
      forecast_datetime <- result$dt_txt
      hour <- as.numeric(strftime(forecast_datetime, format = "%H"))
      month <- as.numeric(strftime(forecast_datetime, format = "%m"))
      forecast_date <- c(forecast_date, forecast_datetime)
      
      season <- if (month >= 3 && month <= 5) {
        "SPRING"
      } else if (month >= 6 && month <= 8) {
        "SUMMER"
      } else if (month >= 9 && month <= 11) {
        "AUTUMN"
      } else {
        "WINTER"
      }
      
      weather_label <- paste0("<b>", city_name, "</b><br/><b>", result$weather[[1]]$main, "</b>")
      weather_detail_label <- paste0(
        "<b>", city_name, "</b><br/><b>", result$weather[[1]]$main, "</b><br/>",
        "Temperature: ", result$main$temp, " C<br/>",
        "Visibility: ", ifelse(is.null(result$visibility), "NA", result$visibility), " m<br/>",
        "Humidity: ", result$main$humidity, "%<br/>",
        "Wind Speed: ", result$wind$speed, " m/s<br/>",
        "Datetime: ", forecast_datetime, "<br/>"
      )
      
      weather_labels <- c(weather_labels, weather_label)
      weather_details_labels <- c(weather_details_labels, weather_detail_label)
      seasons <- c(seasons, season)
      hours <- c(hours, hour)
    }
  }
  
  tibble(
    CITY_ASCII = city,
    WEATHER = weather,
    TEMPERATURE = temperature,
    VISIBILITY = visibility,
    HUMIDITY = humidity,
    WIND_SPEED = wind_speed,
    SEASONS = seasons,
    HOURS = hours,
    FORECASTDATETIME = forecast_date,
    LABEL = weather_labels,
    DETAILED_LABEL = weather_details_labels
  )
}

load_saved_model <- function(model_name) {
  model <- read_csv(model_name)
  model <- model %>% mutate(Variable = gsub('"', '', Variable))
  setNames(model$Coef, as.list(model$Variable))
}

predict_bike_demand <- function(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS) {
  model <- load_saved_model("model.csv")
  weather_terms <- model['Intercept'] + TEMPERATURE * model['TEMPERATURE'] + 
    HUMIDITY * model['HUMIDITY'] + WIND_SPEED * model['WIND_SPEED'] + 
    VISIBILITY * model['VISIBILITY']
  season_terms <- sapply(SEASONS, function(season) model[season])
  hour_terms <- sapply(HOURS, function(hour) model[as.character(hour)])
  predictions <- pmax(0, weather_terms + season_terms + hour_terms)
  as.integer(predictions)
}

calculate_bike_prediction_level <- function(predictions) {
  sapply(predictions, function(prediction) {
    if (is.na(prediction)) {
      return("unknown")  # Handle NA case
    } else if (prediction <= 1000) {
      return("small")
    } else if (prediction <= 3000) {
      return("medium")
    } else {
      return("large")
    }
  })
}


generate_city_weather_bike_data <- function() {
  cities_df <- read_csv("selected_cities.csv")
  weather_df <- get_weather_forecaset_by_cities(cities_df$CITY_ASCII)
  print(weather_df)
  weather_df %>%
    mutate(BIKE_PREDICTION = predict_bike_demand(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS)) %>%
    mutate(BIKE_PREDICTION_LEVEL = calculate_bike_prediction_level(BIKE_PREDICTION)) %>%
    left_join(cities_df, by = "CITY_ASCII")
}
