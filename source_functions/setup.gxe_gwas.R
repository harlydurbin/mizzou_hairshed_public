#' ---
#' title: "GEMMA GxE GWAS"
#' author: "Harly Durbin"
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#'     df_print: paged
#'     code_folding: hide
#' ---
#'
## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(glue)
library(magrittr)
library(lubridate)

source(here::here("source_functions/parse_renf90table.R"))

#'
#' # Notes & questions
#'
#' * Fit fixed effects directly in GEMMA, dummy coded
#'     + Column of 1s for mean
#'     + Calving season, age group, fescue
#' * Need to have phenotype file and genotype file in same order
#'     + Phenotype in fam file - make fam manually then use `--keep` to subset genotypes?
#'
#' # Setup
#'
## -------------------------------------------------------------------------------------------------
geno_prefix <- as.character(commandArgs(trailingOnly = TRUE)[1])

#'
## -------------------------------------------------------------------------------------------------
score_year <- as.character(commandArgs(trailingOnly = TRUE)[2])

#'
## -------------------------------------------------------------------------------------------------
var <- as.character(commandArgs(trailingOnly = TRUE)[3])

#'
## ---- warning=FALSE, message=FALSE----------------------------------------------------------------
cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

#'
## -------------------------------------------------------------------------------------------------
full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

#'
## -------------------------------------------------------------------------------------------------
full_fam <-
  read_table2(here::here(glue("{geno_prefix}.qc.fam")),
              col_names = FALSE)

cg_blues <-
  parse_renf90table(here::here("data/derived_data/aireml_varcomp/gxe_gwas/renf90.tables"),
                    effect_num = 1,
                    effect_key = TRUE) %>% 
  mutate_at(vars(contains("id")), ~ as.numeric(.)) %>% 
  left_join(read_table2(here::here("data/derived_data/aireml_varcomp/gxe_gwas/solutions"),
                        skip = 1,
                        col_names = c("trait", "effect", "id_renamed", "solution", "se"))) %>% 
  select(cg_num = id_original, solution) %>% 
  assertr::verify(!is.na(solution)) %>% 
  left_join(read_table2(here::here("data/derived_data/aireml_varcomp/gxe_gwas/data.txt"),
                        col_names = c("full_reg", "cg_num", "hair_score", "year", "date_score_recorded")))
  

#'
## ---- message=FALSE, warning=FALSE----------------------------------------------------------------
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
## ---- message=FALSE, warning=FALSE----------------------------------------------------------------
coord_key <- read_csv(here::here("data/derived_data/environmental_data/coord_key.csv"))

#'
#' # Filtering & joining
#'
#' ## Remove males
#'
## -------------------------------------------------------------------------------------------------
dat <-
  cleaned %>%
  filter(sex == "F")

#'
#' ## Add coordinates
#'
## -------------------------------------------------------------------------------------------------

dat %<>%
  left_join(coord_key %>%
              select(farm_id, lat, long)) %>%
  assertr::verify(!is.na(lat)) %>%
  assertr::verify(!is.na(long))


#'
#' ## Mean apparent high temperature, mean day length
#'
## -------------------------------------------------------------------------------------------------
dat %<>%
  filter(!is.na(date_score_recorded)) %>%
  left_join(weather) %>%
  assertr::verify(!is.na(mean_apparent_high)) %>%
  assertr::verify(!is.na(mean_day_length))

#'
#' ## ID matching
#'
## -------------------------------------------------------------------------------------------------
dat %<>%
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
  assertr::verify(!is.na(hair_score))

## Subtract off CG BLUE

dat %<>% 
  left_join(cg_blues) %>% 
  filter(!is.na(solution)) %>% 
  mutate(adj_hs = hair_score-solution) %>% 
  # Make sure no duplicate rows made by join
  assertr::verify(length(dat$hair_score) >= length(.$hair_score))

#'
#' ## Specified year only, take random record for animals with multiple records within a year
#'
## -------------------------------------------------------------------------------------------------

dat <-
  if(score_year == "random") {
    dat %>% 
      group_by(full_reg) %>% 
      sample_n(1) %>% 
      ungroup()
  } else {
    dat %>%
      filter(year == as.numeric(score_year)) %>%
      group_by(full_reg) %>%
      sample_n(1) %>%
      ungroup()
  }

#'
#' # Export
#'
#' ## Phenotypes in new `.fam` file
#'
## -------------------------------------------------------------------------------------------------
matched <-
  full_fam %>%
  left_join(dat %>%
              select(X1 = full_reg, adj_hs)) %>%
  select(X1:X5, adj_hs)

#'
## -------------------------------------------------------------------------------------------------
matched %>%
  write_delim(here::here(glue("data/derived_data/gxe_gwas/{var}/{score_year}/gxe_gwas.{var}.{score_year}.fam")),
            col_names = FALSE,
            delim = " ")

#'
#' ## GxE file
#'
## -------------------------------------------------------------------------------------------------

if(var == "day_length") {
  matched %>%
    select(full_reg = X1) %>%
    left_join(dat %>%
              select(full_reg, mean_day_length)) %>%
    select(mean_day_length) %>%
    write_tsv(here::here(glue("data/derived_data/gxe_gwas/{var}/{score_year}/gxe.txt")),
              col_names = FALSE)
  } else if(var == "temp") {
    matched %>%
      select(full_reg = X1) %>%
      left_join(dat %>%
              select(full_reg, mean_apparent_high)) %>%
      select(mean_apparent_high) %>%
      write_tsv(here::here(glue("data/derived_data/gxe_gwas/{var}/{score_year}/gxe.txt")),
                col_names = FALSE)
    }

#'
#' # Range summary
#'
## -------------------------------------------------------------------------------------------------
dat %>%
  summarise_at(vars(c("mean_apparent_high", "mean_day_length", "adj_hs")), ~ range(.))

#'
