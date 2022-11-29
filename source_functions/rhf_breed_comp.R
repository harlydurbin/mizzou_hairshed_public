# Supplementing breed composition data with data provided by Red Hill Farms

library(readxl)
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)

# Set up

cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

# Gather

rhf_breed <-
  read_excel("~/Box Sync/HairShedding/ReportedData/2019/DataRecording_RHF_2019.xlsx",
             sheet = "Original_heifers") %>% 
  janitor::clean_names() %>% 
  select(animal_id = tattoo, breeds) %>%
  mutate(animal_id = stringr::str_remove_all(animal_id,
                                             "[[:punct:]]|[[:space:]]")) %>% 
  left_join(cleaned %>% 
              filter(year == 2019) %>% 
              filter(farm_id == "RHF")) %>% 
  bind_rows(read_excel("~/Box Sync/HairShedding/ReportedData/2018/DataRecording_RHF_2018.xlsx",
                       sheet = "new") %>% 
              janitor::clean_names() %>% 
              select(animal_id, breeds = comment) %>% 
              mutate(animal_id = stringr::str_remove_all(animal_id,
                                                         "[[:punct:]]|[[:space:]]")) %>% 
              left_join(cleaned %>% 
                          filter(year == 2018) %>% 
                          filter(farm_id == "RHF")))

rhf_breed %<>%
  # Extract breed fractions
  mutate(ar = case_when(breeds %in% c("PB AR", "100% AR") ~
                          "1/1",
                        stringr::str_detect(breeds, "(?<=[[:digit:]]/[[:digit:]] )AR") ~
                          stringr::str_extract(breeds, "[[:digit:]]/[[:digit:]](?= AR)")),
         an = case_when(breeds %in% c("PB AN", "100% AN") ~
                          "1/1",
                        stringr::str_detect(breeds, "(?<=[[:digit:]]/[[:digit:]] )AN") ~
                          stringr::str_extract(breeds, "[[:digit:]]/[[:digit:]](?= AN)")),
         sim = case_when(breeds %in% c("PB SM", "100% SM") ~
                           "1/1",
                         stringr::str_detect(breeds, "(?<=[[:digit:]]/[[:digit:]] )SM") ~
                           stringr::str_extract(breeds, "[[:digit:]]/[[:digit:]](?= SM)"))) %>% 
  # Convert character breed fractions to numeric percentages
  mutate_at(vars("ar", "an", "sim"),
            ~ purrr::map_dbl(.x = .,
                             ~ eval(parse(text = .x)))) %>% 
  # Replace NAs with zeroes
  mutate_at(vars("ar", "an", "sim"),
            ~ tidyr::replace_na(., 0)) %>% 
  # Don't differentiate between Red Angus and Angus since RAAA doesn't
  mutate(ar = case_when(ar > 0 & an > 0 ~ an + ar,
                        TRUE ~ ar),
         ar = if_else(ar > 1, 1, ar),
         cross = case_when(ar == 1 ~ "AR",
                           sim == 1 ~ "SIM",
                           an == 1 ~ "AN",
                           TRUE ~ "CROS"))

# Export

rhf_breed %>% 
  mutate(brd_source = "RHF") %>% 
  select(farm_id, animal_id, registration_number, Lab_ID, temp_id, cross, an, ar, sim, brd_source) %>% 
  write_rds(here::here("data/raw_data/201005.rhf_breed.rds"))
