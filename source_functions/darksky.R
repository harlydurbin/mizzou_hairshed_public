library(darksky)
library(readr)
library(purrr)
library(stringr)
library(dplyr)
library(magrittr)
library(tidyr)

setwd(here::here())

# Need to get a darksky API key and set it in the global environment
# https://darksky.net/dev/login?next=/account

Sys.getenv()
darksky::darksky_api_key()


cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

# weather is a 4 column data frame: 
# date_score_recorded, latitude, longitude, date in one of 60 days prior to date_score_recorded
weather <-  
  cleaned %>% 
  filter(!is.na(date_score_recorded)) %>% 
  left_join(read_csv(here::here("data/derived_data/environmental_data/coord_key.csv")) %>% 
              select(farm_id, lat, long)) %>% 
  distinct(date_score_recorded, lat, long) %>% 
  mutate(sixty_prior = map(date_score_recorded,
                           ~ seq(.x - 59, .x, by = "days")),
         sixty_prior = map(sixty_prior, as_tibble)) %>% 
  unnest() 

# Only unique dates
weather2 <-
  weather %>% 
  select(-date_score_recorded) %>% 
  distinct() 

# This established a function to format dates in the timestamp format
# that darkspy requires
timestamp_chr <-
  function(var) {
    stringr::str_c(as.character(var), "T12:00:00-0400")
  }

# This establishes a function to get weather history
# given a date, lat, and long
get_history <-
  function(lat, long, value) {
    darksky::get_forecast_for(latitude = lat,
                              longitude = long,
                              timestamp = timestamp_chr(value),
                              exclude = "currently")
  }


# This applies the function to each row in the data frame
# The result is a new data frame with a new list-column of
# all of the weather data
weather_done <- 
  weather2 %>%
  dplyr::mutate(data = purrr::pmap(list(lat, long, value), .f = get_history))

# Rejoin to date_score_recorded
# Save the data frame to an RDS file
weather %>% 
  left_join(weather_done) %>% 
  readr::write_rds(here::here("data/derived_data/environmental_data/weather.rds"))
