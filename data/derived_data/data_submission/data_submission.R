#' ---
#' title: "Clean up phenotype & genotype files for submission"
#' author: "Harly Durbin"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(glue)
library(magrittr)
library(lubridate)
library(tidylog)
library(readxl)

source(here::here("source_functions/coalesce_join.R"))

#' 
#' # Setup
#' 
## ---- warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------
sample_table <- 
  read_csv(here::here("data/raw_data/import_join_clean/200820_sample_sheet.csv"),
           trim_ws = TRUE,
           guess_max = 100000)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
coord_key <- read_csv(here::here("data/derived_data/environmental_data/coord_key.csv"))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
weather <-
  read_rds(here::here("data/derived_data/environmental_data/weather.rds")) %>%
  mutate(daily = purrr::map(data, "daily", .default = NA),
         apparent_high = purrr::map_dbl(daily,
                                        ~ .x %>%
                                          dplyr::pull(apparentTemperatureHigh)),
         # 10/9/20 forgot to convert from F to C
         apparent_high = measurements::conv_unit(apparent_high, from = "F", to = "C"),
         sunrise = purrr::map_chr(daily,
                                  ~.x %>%
                                    dplyr::pull(sunriseTime) %>%
                                    as.character(.)),
         sunset = purrr::map_chr(daily,
                                 ~.x %>%
                                   dplyr::pull(sunsetTime) %>%
                                   as.character(.)),
         sunrise = lubridate::as_datetime(sunrise),
         sunset = lubridate::as_datetime(sunset),
         day_length = as.numeric(sunset - sunrise)) %>%
  # Remove the data column
  select(-data)  %>%
  group_by(date_score_recorded, lat, long) %>%
  # Take rows for max 30 days
  slice_max(order_by = value, n = 30) %>%
  summarise(mean_apparent_high = mean(apparent_high),
            mean_day_length = mean(day_length)) %>%
  ungroup()

#' 
#' # Phenotype file
#' 
#' ## Add environmental data
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat <-
  cleaned %>% 
  left_join(coord_key %>%
              select(farm_id, lat, long)) %>%
  assertr::verify(!is.na(lat)) %>%
  assertr::verify(!is.na(long))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat %<>% 
  left_join(weather)

#' 
#' ## Impute calving season
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat %<>%
  # If calving season missing, impute using most recent calving season
  group_by(farm_id, temp_id) %>%
  arrange(date_score_recorded) %>%
  fill(calving_season, .direction = "downup") %>%
  ungroup() %>%
  # If calving season still missing, impute using DOB
  mutate(calving_season = case_when(sex == "M" ~ NA_character_,
                                    farm_id == "UMCT" ~ "SPRING",
                                    farm_id == "UMF" ~ "FALL",
                                    is.na(calving_season) &
                                      between(lubridate::month(dob),
                                              left = 1,
                                              right = 6) ~ "SPRING",
                                    is.na(calving_season) &
                                      between(lubridate::month(dob),
                                              left = 7,
                                              right = 12) ~ "FALL",
                                    TRUE ~ calving_season))

#' 
#' ## Age group
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat %<>% 
  mutate(age_group = case_when(age == 1 ~ "1",
                               age %in% c(2, 3) ~ "2",
                               between(age, 4, 7) ~ "3",
                               age >= 8 ~ "4")) 

#' 
#' ## Score group
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat %<>%
  left_join(bind_rows(read_excel(here::here("data/derived_data/ua_score_groups.xlsx")),
                      read_excel(here::here("data/derived_data/score_groups.xlsx"))) %>%
              select(farm_id, date_score_recorded, score_group) %>%
              mutate(date_score_recorded = lubridate::ymd(date_score_recorded))) %>%
  mutate(score_group = tidyr::replace_na(score_group, 1))

#' 
#' ## Submission ID
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
random_id <-
  dat %>% 
  distinct(farm_id, animal_id, temp_id, Lab_ID) %>% 
  mutate(id = purrr::map_chr(.x = farm_id, 
                             ~ glue_collapse(c("A0", sample(0:9, 9, replace = TRUE)))))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat %<>% 
  group_by(farm_id, temp_id) %>% 
  tidyr::fill(Lab_ID, .direction = "downup") %>% 
  ungroup() %>% 
  left_join(random_id) %>% 
  mutate(id = if_else(!is.na(Lab_ID), as.character(Lab_ID), id)) 

#' 
#' ## Complete file
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat %>% 
  rename(coat_color = color, age_class = age) %>% 
  select(-sold, -temp_id) %>% 
  assertr::verify(!is.na(id)) %>% 
  write_csv(here::here("data/derived_data/data_submission/full.csv"),
            na = "")

#' 
#' ## Remove identifying variables
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
dat %>% 
  select(id, sex, coat_color = color, age_class = age, age_group, calving_season, toxic_fescue, score_group, date_score_recorded, mean_apparent_high, mean_day_length, hair_score) %>% 
  write_csv(here::here("data/derived_data/data_submission/anonymized.csv"),
            na = "")

#' 
#' # Genotype file
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
matched <-
  dat %>%
  left_join(full_ped %>%
              distinct(farm_id, temp_id, full_reg, Lab_ID)) %>%
  mutate(breed_code = case_when(breed_code == "AN" ~ "AAA",
                                breed_code == "ANR" ~ "RAN",
                                breed_code == "BG" ~ "BGR",
                                breed_code == "BRN" ~ "BSW",
                                breed_code == "MAAN" ~ "RDP"),
         full_reg = case_when(is.na(full_reg) &
                                !is.na(registration_number) ~ glue("{breed_code}{registration_number}"),
                              is.na(full_reg) &
                                is.na(registration_number) ~ glue("{farm_id}{animal_id}{temp_id}"),
                              TRUE ~ full_reg))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
# International ID (in original post-imputation file) <--> full_reg decoder 
# Originally used re-naming samples to my ID (since IIDs dumped by Bob were inconsistent between sample/animal tables etc)
submission_samples <-
  read_table2(here::here("data/raw_data/geno_dump/200924_HairShed.850K.update_id.txt"),
              col_names = FALSE) %>% 
  select(iid = X1,
         full_reg = X3) %>%
  # why
  mutate(full_reg = str_remove(full_reg, "AMGV")) %>% 
  left_join(matched %>% 
              distinct(full_reg, farm_id, temp_id, Lab_ID, id)) %>% 
  coalesce_join(full_ped %>% 
                  distinct(full_reg, farm_id, temp_id, Lab_ID),
                by = c("full_reg"),
                join = dplyr::left_join) %>% 
  filter(!is.na(Lab_ID)) %>% 
  filter(!is.na(id)) %>% 
  left_join(sample_table %>% 
              select(Lab_ID = lab_id, date_added, assay)) %>% 
  filter(assay %in% c("BOVG50V1", "BOVG50V2", "GGPF250")) %>% 
  distinct(iid, id)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
submission_samples %>% 
  select(iid) %>% 
  write_tsv(here::here("data/derived_data/data_submission/submission_samples.txt"),
            col_names = FALSE)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------
submission_samples %>% 
  write_delim(here::here("data/derived_data/data_submission/reheader.txt"),
              col_names = FALSE, 
              delim = " ")

#' 
#' srun --account deckerlab -c 12 --mem 12G bcftools view -S data/derived_data/data_submission/submission_samples.txt data/raw_data/geno_dump/200924_HairShed.vcf.gz --threads 12 -O z -o data/derived_data/data_submission/subset.vcf.gz
#' 
#' srun --account animalsci -c 12 --mem 12G bcftools reheader -s data/derived_data/data_submission/reheader.txt data/derived_data/data_submission/subset.vcf.gz --threads 12 -o data/derived_data/data_submission/submission_samples.vcf.gz
#' 
#' srun --account animalsci --mem 12G -c 12 bcftools index -t --threads 12 data/derived_data/data_submission/submission_samples.vcf.gz
