#' ---
#' title: "Basic variance components and parameters"
#' author: "Harly Durbin"
#' output: html_document
#' ---
#'
## ----setup, include=FALSE-----------------------------------------------------------------------------------------------
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(glue)
library(readxl)
library(magrittr)
library(tidylog)

source(here::here("source_functions/cg_tallies.R"))


#'
#' # Notes & questions
#'
#' ## Model key
#'
#' * Model 1
#'     + Exactly the same contemporary grouping as AGI model/April 2020 model
#'     + No breed effect
#'     + Remove contemporary groups with fewer than 5 animals
#'
#' # Setup
#'
## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------
cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

#'
## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------
full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

#'
#' # Contemporary grouping
#'
#' First, remove males
#'
## -----------------------------------------------------------------------------------------------------------------------
dat <-
  cleaned %>%
  # Females only
  filter(sex == "F")

#'
#' ## Calving season
#'
## -----------------------------------------------------------------------------------------------------------------------
dat %<>%
  # If calving season missing, impute using most recent calving season
  group_by(farm_id, temp_id) %>%
  arrange(date_score_recorded) %>%
  fill(calving_season, .direction = "downup") %>%
  ungroup() %>%
  # If calving season still missing, impute using DOB
  mutate(calving_season = case_when(farm_id == "UMCT" ~ "SPRING",
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
## -----------------------------------------------------------------------------------------------------------------------
print("Missing calving season:")
dat %>%
  summarise(missing_cs = sum(is.na(calving_season)),
            has_cs = sum(!is.na(calving_season)))

#'
## -----------------------------------------------------------------------------------------------------------------------
print("Missing calving season, by farm:")
dat %>%
  filter(is.na(calving_season)) %>%
  group_by(farm_id) %>%
  tally(sort = TRUE)

#'
#' ## Age group
#'
#' * Need to add Angus pedigree age updates
#'
## -----------------------------------------------------------------------------------------------------------------------
dat %<>%
  mutate(age_group = case_when(age == 1 ~ "yearling",
                               age == 2 ~ "fch",
                               age == 3 ~ "three",
                               age >= 4 ~ "mature"))

#'
## -----------------------------------------------------------------------------------------------------------------------
print("Missing age group, by farm:")
dat %>%
  filter(is.na(age_group)) %>%
  group_by(farm_id) %>%
  tally(sort = TRUE)


#'
#' ## Score group
#'
## -----------------------------------------------------------------------------------------------------------------------
dat %<>%
  left_join(bind_rows(read_excel(here::here("data/derived_data/ua_score_groups.xlsx")),
                      read_excel(here::here("data/derived_data/score_groups.xlsx"))) %>%
              select(farm_id, date_score_recorded, score_group) %>%
              mutate(date_score_recorded = lubridate::ymd(date_score_recorded))) %>%
  mutate(score_group = tidyr::replace_na(score_group, 1))

#'
#' ## Final contemporary groupings
#'
## -----------------------------------------------------------------------------------------------------------------------
print("Final contemporary grouping tallies:")
dat %>%
  mutate(cg = glue::glue("{farm_id}{year}{calving_season}{age_group}{score_group}{toxic_fescue}"),
         cg_num = as.integer(factor(cg))) %>%
  cg_tallies()

#'
## -----------------------------------------------------------------------------------------------------------------------

dat %<>%
  mutate(cg = glue::glue("{farm_id}{year}{calving_season}{age_group}{score_group}{toxic_fescue}"),
         cg_num = as.integer(factor(cg))) %>%
  group_by(cg) %>%
  # At least 5 animals per CG
  filter(n() >= 5) %>%
  # Remove CGs with no variation
  filter(var(hair_score) != 0) %>%
  ungroup()

#'
#' # Export
#' ## Data
#'
## -----------------------------------------------------------------------------------------------------------------------
matched <-
  dat %>%
  left_join(full_ped %>%
              distinct(farm_id, temp_id, full_reg)) %>%
  mutate(breed_code = case_when(breed_code == "AN" ~ "AAN",
                                breed_code == "ANR" ~ "RAN",
                                breed_code == "BG" ~ "BGR",
                                breed_code == "BRN" ~ "BSW",
                                breed_code == "MAAN" ~ "RDP"),
         full_reg = case_when(is.na(full_reg) &
                                !is.na(registration_number) ~ glue("{breed_code}{registration_number}"),
                              is.na(full_reg) &
                                is.na(registration_number) ~ glue("{farm_id}{animal_id}{temp_id}"),
                              TRUE ~ full_reg)) %>%
  assertr::verify(!is.na(full_reg)) %>%
  assertr::verify(!is.na(cg_num)) %>%
  assertr::verify(!is.na(hair_score))


#'
## -----------------------------------------------------------------------------------------------------------------------
matched %>%
  distinct(Lab_ID,farm_id, animal_id, temp_id, registration_number, full_reg) %>%
  write_csv(here::here("data/derived_data/aireml_varcomp/agi_model/sanity_key.csv"),
            na = "")

#'
## -----------------------------------------------------------------------------------------------------------------------
matched %>%
  select(full_reg, cg_num, hair_score) %>%
  write_delim(here::here("data/derived_data/aireml_varcomp/agi_model/data.txt"),
              col_names = FALSE)

#'
