#' ---
#' author: "Harly Durbin"
#' output: html_document
#' ---
#' 
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(magrittr)
library(lubridate)
library(purrr)
library(readxl)
library(tidylog)
library(assertr)

# Rscript --vanilla source_functions/import_join_clean.R "PVF_2019" "WAA_2018" &> log/import/200728.log

source(here::here("source_functions/coalesce_join.R"))
source(here::here("source_functions/find_dup.R"))
source(here::here("source_functions/iterative_id_search.R"))
source(here::here("source_functions/first_clean.R"))
source(here::here("source_functions/impute_age.R"))
source(here::here("source_functions/fncols.R"))

#' 
#' # Notes/objectives
#' 
#' 
#' # Setup 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
animal_table <- 
  readr::read_rds(here::here("data/raw_data/import_join_clean/animal_table.rds")) %>% 
  mutate(DOB = if_else(Lab_ID == 140949, lubridate::ymd("2014-02-13"), DOB))

#' 
#' # Excel file import
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
blacklist <- commandArgs(trailingOnly = TRUE)

blacklist %>% 
  purrr::map(~ print(glue::glue("Excluding {.x}")))

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------

farm_list <-
  combine(
    list.files(path = "~/Box Sync/HairShedding/ReportedData/2016", full.names = TRUE),
    list.files(path = "~/Box Sync/HairShedding/ReportedData/2017", full.names = TRUE),
    list.files(path = "~/Box Sync/HairShedding/ReportedData/2018", full.names = TRUE),
    list.files(path = "~/Box Sync/HairShedding/ReportedData/2019", full.names = TRUE),
    list.files(path = "~/Box Sync/HairShedding/ReportedData/2020", full.names = TRUE)
  ) %>% 
  str_extract(., "(?<=_)[[:alnum:]]+(?=_)") %>% 
  # Arkansas is processed separately
  str_subset(., "UofA", negate = TRUE) %>% 
  unique(.)

#' 
#' # First clean
#' 
## --------------------------------------------------------------------------------------------------------------------------------------

joined <-
  farm_list %>% 
  purrr::map(~ first_clean(hs_id = .x, ignore = blacklist)) %>% 
  reduce(bind_rows)

write_rds(joined, here::here("data/derived_data/import_join_clean/joined.rds"))

print("Finshed cleaning all farms")


#' 
#' # Joined clean
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning Mizzou sold codes")
cleaned <-
  joined %>% 
  mutate(date_score_recorded = lubridate::ymd(date_score_recorded)) %>% 
  select(year, farm_id, breed_code, registration_number, animal_id, sex, color, everything(), temp_id, barcode, Lab_ID) %>% 
  ##### SOLD #####
# Sold code standardization
mutate(
  sold = case_when(
    sold %in% c("YES", "SOLD") ~ "YES",
    TRUE ~ "NO"
  )
) %>%
  # If any instances of sold, mark yes in sold column for all instances
  # https://stackoverflow.com/questions/40825037/filter-groups-by-occurrence-of-a-value
  group_by(farm_id, temp_id) %>%
  mutate(sold = any(sold == "YES")) %>% 
  ungroup()

#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning Mizzou age formatting")

cleaned %<>% 
  ##### AGE #####
# Remove strings from age column
mutate(
  age = str_remove_all(age,
                       "YEARS|[[:blank:]]")
) %>% 
  # Infer ages from animal table
  left_join(animal_table %>% 
              select(dob = DOB, Lab_ID)) %>% 
  mutate(
    age_ped =
      case_when(
        !is.na(date_score_recorded) & !is.na(dob) ~ as.integer(date_score_recorded - dob),
        is.na(date_score_recorded) & !is.na(dob) ~ as.integer(ymd(glue::glue("{year}-05-01")) - dob), 
        # If age is birthday string & date score recorded present, 
        # calculate age based on date score recorded
        str_detect(age, "/") & !is.na(date_score_recorded) ~ as.integer(date_score_recorded - mdy(age)),
        str_detect(age, "/") & is.na(date_score_recorded) ~ as.integer(ymd(glue::glue("{year}-05-01")) - mdy(age)),
        # If age is birthday in excel numeric format (larger than 1000),
        # calculate age based on date score recorded
        as.integer(age) > 1000 & !is.na(date_score_recorded) ~ as.integer(ymd(date_score_recorded) - janitor::excel_numeric_to_date(as.numeric(age))),
        as.integer(age) > 1000 & is.na(date_score_recorded) ~ as.integer(ymd(glue::glue("{year}-05-01")) - janitor::excel_numeric_to_date(as.numeric(age)))
      ),
    age_class =
      case_when(
        between(age_ped, 274, 639) ~ 1,
        between(age_ped, 640, 1004) ~ 2,
        between(age_ped, 1005, 1369) ~ 3,
        between(age_ped, 1370, 1734) ~ 4,
        between(age_ped, 1735, 2099) ~ 5,
        between(age_ped, 2100, 2464) ~ 6,
        between(age_ped, 2465, 2829) ~ 7,
        between(age_ped, 2830, 3194) ~ 8,
        between(age_ped, 3195, 3559) ~ 9,
        between(age_ped, 3560, 3924) ~ 10,
        between(age_ped, 3925, 4289) ~ 11,
        between(age_ped, 4290, 4654) ~ 12,
        between(age_ped, 4655, 5019) ~ 13,
        between(age_ped, 5020, 5384) ~ 14,
        between(age_ped, 5385, 5749) ~ 15,
        between(age_ped, 5750, 6114) ~ 16,
        between(age_ped, 6115, 6479) ~ 17,
        between(age_ped, 6480, 6844) ~ 18,
        between(age_ped, 6845, 7209) ~ 19,
        between(age_ped, 7210, 7574) ~ 20,
        between(age_ped, 7575, 7939) ~ 21
      ),
    age =
      case_when(
        !is.na(age_class) ~ as.integer(age_class),
        TRUE ~ as.integer(age)
      )) %>% 
  # Must be at least 274 days old
  filter(age_ped >= 274 | is.na(age_ped)) %>% 
  group_by(farm_id, temp_id, year) %>% 
  fill(age, .direction = "downup") %>% 
  ungroup() %>% 
  # Verify ages in allowable range
  assertr::verify(between(age, 1, 22) | is.na(age)) %>% 
  select(-age_ped, -age_class) 

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning Mizzou hair scores, adding UA data")
cleaned %<>% 
  ##### HAIR SCORE ####
filter(!is.na(hair_score)) %>% 
  mutate(hair_score = cleaner::clean_numeric(hair_score)) %>% 
  bind_rows(read_rds(here::here("data/derived_data/import_join_clean/ua_clean.rds")) %>% 
              # 10/2/2020 added automatic toxic_fescue = yes for UA
              mutate(toxic_fescue = "YES")) %>% 
  # Verify hair score values
  assertr::verify(dplyr::between(hair_score, 1, 5))

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning sex formatting")
cleaned %<>% 
  ##### SEX #####
mutate(
  sex = 
    case_when(
      # Female
      sex %in% c("C", "COW", "F") ~ "F", 
      # Male
      sex %in% c("B", "M") ~ "M",
      # Missing
      sex %in% c("U") ~ NA_character_,
      TRUE ~ as.character(sex)
    )
) %>%
  group_by(farm_id, temp_id) %>% 
  # Fill in missing sex values
  tidyr::fill(sex, .direction = "downup") %>%  
  # If multiple sexes across years, drop
  filter(n_distinct(sex) == 1) %>% 
  ungroup() %>% 
  # Verify sex values
  assertr::verify(sex %in% c("F", "M") | is.na(sex))

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning breed codes")
cleaned %<>% 
  ##### BREED CODE #####
mutate(
  breed_code = case_when(
    breed_code %in% c("ANGUS") ~ "AN",
    breed_code %in% c("SIMAN") ~ "SIM",
    breed_code %in% c("BAL") ~ "GEL",
    breed_code %in% c("Hereford") ~ "HFD",
    TRUE ~ as.character(breed_code)
  )
) %>% 
  # Verify breed codes
  assertr::verify(breed_code %in% c("AN", "ANR", "BG", "CHIA", "CHA", "CROS", "SIM", "HFD", "SIMB", "MAAN", "GEL", "SH", "BRN") | is.na(breed_code))

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning color codes")
cleaned %<>% 
  ##### COLOR #####
mutate(
  color = stringr::str_to_upper(color),
  color = 
    case_when(
      # If BC is AN or color is misspelled, change it to BLACK
      is.na(breed_code) & breed_code == "AN" | color %in% c("BLK","B", "8LK") ~
        "BLACK",
      # If BC is HFD or misspelled, RED WHITE FACE
      is.na(breed_code) & breed_code == "HFD" | color %in% c("REDWHITEFACE", "R/WF", "RWF") ~
        "RED WHITE FACE",
      color %in% c("BWF") ~ 
        "BLACK WHITE FACE",
      color %in% c("REDSPOTTED") ~ 
        "RED SPOTTED",
      color %in% c("SMOKE") ~ 
        "GREY",
      color %in% c("ED", "R") ~ 
        "RED",
      color %in% c("Y") ~ 
        "YELLOW",
      # Color codes from Arkansas, no idea what they're supposed to mean
      color %in% c("GB", "BM", "RM", "YM", "BB", "RB", "RM") ~ NA_character_,
      TRUE ~ 
        as.character(color)
    )
) %>% 
  # Verify color values
  assertr::verify(color %in% c("BLACK", "BLACK ROAN", "BLACK SPOTTED", "BLACK WHITE FACE", "BROWN", "BRINDLE", "RED", "RED ROAN", "RED SPOTTED", "RED WHITE FACE", "WHITE", "GREY", "YELLOW") | is.na(color)) %>% 
  group_by(farm_id, temp_id) %>% 
  arrange(year) %>% 
  # Fill in missing color values
  tidyr::fill(color, .direction = "downup") %>% 
  ungroup()

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("cleaning date_score_recorded")
cleaned %<>% 
  ##### DATE SCORE RECORDED #####
mutate(date_score_recorded = lubridate::ymd(date_score_recorded)) %>% 
  # Verify year == year of date_score_recorded
  assertr::verify(year == lubridate::year(date_score_recorded) | is.na(date_score_recorded))

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning calving season")
cleaned %<>% 
  ###### CALVING SEASON #####
mutate(
  calving_season = stringr::str_to_upper(calving_season),
  calving_season = case_when(
    # Bulls can't have babies
    sex == "M" ~ NA_character_,
    calving_season %in% c("SPRING", "S", "SPRNG") | str_detect(calving_season, "^S|^s") ~ "SPRING",
    calving_season %in% c("FALL", "F") | str_detect(calving_season, "^F|^f") ~ "FALL",
    # Savoy Arkansas cows are fall calving
    #location == "UAS" ~ "SPRING",
    TRUE ~ NA_character_
  )
) %>% 
  # Verify calving_season values
  assertr::verify(calving_season %in% c("SPRING", "FALL") | is.na(calving_season))

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Cleaning toxic fescue")
cleaned %<>% 
  ##### TOXIC_FESCUE #####
# Fescue code standardization
mutate(
  toxic_fescue = case_when(
    toxic_fescue %in% c("Y", "TRUE") ~ "YES",
    toxic_fescue %in% c("N", "FALSE") ~ "NO",
    # Savoy Arkansas cows graze hot fescue
    #location == "UAS" ~ "YES",
    TRUE ~ as.character(toxic_fescue)
  )
) %>% 
  # Verify toxic_fescue values
  assertr::verify(toxic_fescue %in% c("YES", "NO") | is.na(toxic_fescue))  %>% 
  distinct()

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
print("Imputing missing ages")
cleaned %<>% 
  impute_age(id_vars = c("farm_id", "temp_id"))

# Manual lab ID exclusions that I can't get around
print("Manually excluding known incorrect Lab IDs")
cleaned %<>% 
  mutate(Lab_ID = case_when(
    registration_number == "3522910" & Lab_ID == 300804 ~ NA_integer_,
    TRUE ~ as.integer(Lab_ID)
  ))

readr::write_rds(cleaned, here::here("data/derived_data/import_join_clean/cleaned.rds"))

#' 
## --------------------------------------------------------------------------------------------------------------------------------------
cleaned %>% 
  filter(!is.na(Lab_ID)) %>% 
  group_by(Lab_ID) %>% 
  filter(n_distinct(temp_id) > 1 ) %>% 
  arrange(Lab_ID) %>% 
  select(Lab_ID, barcode, registration_number, animal_id, year) %>% 
  left_join(animal_table)

print("Exported cleaned data")


now <- 
  as.character(lubridate::today()) %>% 
  stringr::str_remove_all(., "-")

rmarkdown::render(input = here::here("source_functions/sanity_check.Rmd"),
                  output_file = here::here(glue::glue("log/import_join_clean/{now}.sanity_check.html")))

print("Rendered sanity check")

rmarkdown::render(input = here::here("notebooks/data_summary.Rmd"),
                  output_file = here::here("html/data_summary.html"))

print("Rendered data summary")