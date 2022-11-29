#' ---
#' title: "GCTA GWAS"
#' author: "Harly Durbin"
#' output: html_document
#' ---
#'
## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------
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

#'
#' # Notes & questions
#'
#' # Setup
#'
## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------
cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

score_year <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

#'
## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------

genotyped <- read_csv(here::here("data/derived_data/grm_inbreeding/mizzou_hairshed.diagonal.full_reg.csv"))

full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

#'
#' # Score group

#'
## -----------------------------------------------------------------------------------------------------------------------------------
dat <-
  cleaned %>%
  left_join(bind_rows(read_excel(here::here("data/derived_data/ua_score_groups.xlsx")),
                      read_excel(here::here("data/derived_data/score_groups.xlsx"))) %>%
              select(farm_id, date_score_recorded, score_group) %>%
              mutate(date_score_recorded = lubridate::ymd(date_score_recorded))) %>%
  mutate(score_group = tidyr::replace_na(score_group, 1))

#'
## -----------------------------------------------------------------------------------------------------------------------------------

#'
#' # Remove males
#'
## -----------------------------------------------------------------------------------------------------------------------------------
dat %<>%
  filter(sex == "F")

#'
#' # Calving season
#'
## -----------------------------------------------------------------------------------------------------------------------------------
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
#' # Age group
#'
## -----------------------------------------------------------------------------------------------------------------------------------

dat %<>%
  mutate(age_group = case_when(age == 1 ~ "yearling",
                               age %in% c(2, 3) ~ "growing",
                               between(age, 4, 9) ~ "mature",
                               age >= 10 ~ "old"))


#'
#' # Toxic fescue
#'
## -----------------------------------------------------------------------------------------------------------------------------------

dat %<>%
  mutate(toxic_fescue = if_else(farm_id %in% c("BAT", "CRC"),
                                "YES",
                                toxic_fescue))


#'
#' # Contemporary group
#'
## -----------------------------------------------------------------------------------------------------------------------------------
dat %<>%
  mutate(cg = glue("{farm_id}{year}{calving_season}{age_group}{score_group}{toxic_fescue}"),
         cg_num = as.integer(factor(cg)))


# Specified year only
# Take score closest to May 1

dat %<>%
  filter(year == score_year) %>%
  mutate(days_from = date_score_recorded - lubridate::ymd(glue("{score_year}-05-01")),
         days_from = as.numeric(days_from),
         days_from = abs(days_from)) %>%
  group_by(farm_id, temp_id) %>%
  arrange(days_from) %>%
  slice(1) %>%
  ungroup()


#'
#' # Export
#'
#' ## Data
#'
## -----------------------------------------------------------------------------------------------------------------------------------
matched <-
  dat %>%
  left_join(full_ped %>%
              distinct(farm_id, temp_id, full_reg)) %>%
  mutate(breed_code = case_when(breed_code == "AN" ~ "AAA",
                                breed_code == "ANR" ~ "RAN",
                                breed_code == "BG" ~ "BGR",
                                breed_code == "BRN" ~ "BSW",
                                breed_code == "MAAN" ~ "RDP",
                                breed_code == "HFD" ~ "HER",
                                TRUE ~ breed_code),
         full_reg = case_when(is.na(full_reg) &
                                !is.na(registration_number) ~ glue("{breed_code}{registration_number}"),
                              is.na(full_reg) &
                                is.na(registration_number) ~ glue("{farm_id}{animal_id}{temp_id}"),
                              TRUE ~ full_reg)) %>%
  assertr::verify(!is.na(full_reg)) %>%
  assertr::verify(!is.na(hair_score))


matched %<>%
  group_by(full_reg) %>%
  slice(1) %>%
  ungroup()

matched %<>%
  left_join(read_table2(here::here("data/derived_data/grm_inbreeding/mizzou_hairshed.grm.id"),
                        col_names = c("full_reg", "iid"))) %>%
  filter(!is.na(iid))

#'
## -----------------------------------------------------------------------------------------------------------------------------------
matched %>%
  select(full_reg, iid, hair_score) %>%
  write_delim(here::here(glue::glue("data/derived_data/gcta_gwas.years/{score_year}/pheno.txt")),
              col_names = FALSE)

matched %>%
  select(full_reg, iid, cg_num) %>%
  write_delim(here::here(glue::glue("data/derived_data/gcta_gwas.years/{score_year}/covar.txt")),
              col_names = FALSE)
