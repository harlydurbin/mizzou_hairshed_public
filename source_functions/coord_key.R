library(readr)
library(tidyr)
library(dplyr)
library(glue)
library(tidygeocoder)

source(here::here("source_functions/coalesce_join.R"))

cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

coord_key <-
  cleaned %>% 
  distinct(farm_id) %>% 
  left_join(read_excel(here::here("data/raw_data/farm_numbers.xlsx")) %>% 
              janitor::clean_names() %>% 
              select(farm_id, street = address, city, state, zip)) %>% 
  mutate(query = case_when(!is.na(street) ~
                             glue("{street}, {city}, {state}, {zip}"),
                           TRUE ~ 
                             glue("{city}, {state}, {zip}"))) %>%
  geocode(address = query,
          method = 'cascade',
          return_addresses = TRUE) %>% 
  select(-query) %>% 
  mutate(address = if_else(is.na(lat), NA_character_, address),
         geo_method = if_else(is.na(lat), NA_character_, geo_method))

coord_key %<>% 
  filter(is.na(lat)) %>% 
  arrange(farm_id) %>% 
  select(-lat, -long, -geo_method, -address) %>% 
  mutate(query = glue("{city}, {state}, {zip}")) %>% 
  geocode(address = query,
          method = 'cascade', 
          return_addresses = TRUE) %>% 
  select(-query) %>% 
  coalesce_join(coord_key,
                join = dplyr::right_join,
                by = c("farm_id", "street", "city", "state", "zip"))

coord_key %>% 
  write_csv(here::here("data/derived_data/environmental_data/coord_key.csv"),
            na = "")