#' ---
#' title: "University of Arkansas"
#' author: "Harly Durbin"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyr)
library(purrr)
library(readxl)
library(dplyr)
library(stringr)
library(tidylog)

source(here::here("source_functions/iterative_id_search.R"))
source(here::here("source_functions/coalesce_join.R"))
source(here::here("source_functions/impute_age.R"))

#' 
#' # Notes & questions
#' 
#' File key:
#' 
#' * `DataRecording_UofA_2016.xlsx`
#' * `DataRecording_UofA_2016_v1.xlsx`
#'     + Same thing as `DataRecording_UofA_2016.xlsx`?
#'     + 1168 rows in first sheet
#' * `DataRecordingPt1UniversityArkansas2016.xlsx`
#'     + Looks like hwere Helen was assigning Lab IDs
#' * `GeneSeek plate sheet final computer version 10052016 (2).xlsx` `r emo::ji("check")`
#'     + Plate/well ID
#'     + 1172 rows in data sheet (`Sheet1`)
#' * `DataRecording_UofA_2017`
#'     + I think I made this at some point???
#' * `LFST all genotyped lrm 11142016_JEK.xlsx` `r emo::ji("check")`
#'     + 533 rows in first sheet (`data entry2016`, Batesville 2016 data)
#' * `Savoy Data 1114016 lrm.xlsx` `r emo::ji("check")`
#'     + No idea where this came from, but appears to contain data between 2012 and 2016 for Savoy farm
#'     + For in one column of the 2014 tab, `date_score_recorded` is in 2016???
#' * `UA_Batesville_2017HS_JEK.xlsx` `r emo::ji("check")`
#'     + Contains 2017 data for Batesville farm
#' * `2018 HCS.xlsx`
#'     + Only Savoy data 2018?
#' 
#' ---
#' 
#' Going to call:
#' 
#' * Short four digit id = `animal_id`
#' * ID created for Neogen = `registration_number`
#'   + Should be xx digits: **[M or F]-00UA-[SAV or BAT]-[4-digit animal ID]**
#' 
#' # Setup 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
animal_table2 <-
  read_rds(here::here("data/raw_data/import_join_clean/animal_table.rds")) %>% 
  filter(str_detect(Reg, "SAV|BAT|UA"))

#' 
#' # Import individual files
#' 
#' ## `Savoy Data 1114016 lrm.xlsx`
#' 
## ---- warning = FALSE, message = FALSE-------------------------------------------------------------------------------------------------------------------------------

# tabs in Savoy file containing actual data
sav_all <-
  c("DataEntry 2016",
    "DataEntry2015",
    "DataEntry2014",
    "DataEntry2013",
    "DataEntry2012") %>%
  set_names() %>%
  purrr::map(function(.x) {
    
    # Vector of column names
    (nms <-
       names(read_excel("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/Savoy Data 1114016 lrm.xlsx",
                        sheet = .x)))
    
    # Assign column types based on column name
    (ct <-
        if_else(str_detect(nms, "date|Date"), "date", "text"))
    
    read_excel("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/Savoy Data 1114016 lrm.xlsx",
               sheet = .x,
               col_types = ct,
               trim_ws = TRUE,
               na = ".") %>%
      janitor::clean_names() %>%
      dplyr::select(-one_of("shearing", "farm", "ge_epd")) %>% 
      dplyr::mutate(year = as.integer(str_extract(.x, "[[:digit:]]{4}")),
                    source_file = "Savoy Data 1114016 lrm.xlsx",
                    source_tab = .x)
    }) %>%
  purrr::map(function(df = .x) {
    
    dates <-
      colnames(df) %>%
      str_subset(., "recorded_[[:digit:]]+")
    
    scores <-
      colnames(df) %>%
      str_subset(., "score_[[:digit:]]+")
    
    assertthat::validate_that(assertthat::are_equal(length(dates), length(scores)))
    
    purrr::map2(.x = dates,
                .y = scores,
                ~ df %>%
                  select(breed_code,
                         registration_number,
                         sire_registration,
                         sex,
                         color,
                         animal_id,
                         date_score_recorded = .x,
                         hair_score = .y,
                         age,
                         calving_season,
                         toxic_fescue,
                         comment,
                         year,
                         source_file,
                         source_tab)) %>%
      reduce(bind_rows) %>%
      filter(!is.na(hair_score))
  }) %>%
  reduce(bind_rows) %>%
  select(-registration_number) %>%
  rename(registration_number = animal_id) %>%
  mutate(date_score_recorded = lubridate::ymd(date_score_recorded),
         hair_score = as.integer(hair_score),
         farm_id = "SAV")


#' 
#' ## `LFST all genotyped lrm 11142016_JEK.xlsx`
#' 
## ---- warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------
bat_16 <-
  c("data entry2016",
    "Missing Genotype") %>%
  purrr::map(function(.x) {
    
    # Vector of column names
    (nms <-
       names(read_excel("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/LFST all genotyped lrm 11142016_JEK.xlsx",
                        sheet = .x)))
    
    # Assign column types based on column name
    (ct <-
        if_else(str_detect(nms, "date|Date"), "date", "text"))
    
    read_excel("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/LFST all genotyped lrm 11142016_JEK.xlsx",
               sheet = .x,
               col_types = ct,
               trim_ws = TRUE,
               na = ".") %>%
      janitor::clean_names() %>%
      dplyr::select(-one_of("shearing", "ge_epd", "farm")) %>% 
      dplyr::mutate(source_file = "LFST all genotyped lrm 11142016_JEK.xlsx",
                    source_tab = .x)
    }) %>%
  purrr::map(function(df = .x) {
    
    dates <-
      colnames(df) %>%
      str_subset(., "recorded_[[:digit:]]+")
    
    scores <-
      colnames(df) %>%
      str_subset(., "score_[[:digit:]]+")
    
    assertthat::validate_that(assertthat::are_equal(length(dates), length(scores)))
    
    purrr::map2(.x = dates,
                .y = scores,
                ~ df %>%
                  select(breed_code,
                         registration_number,
                         sire_registration,
                         sex,
                         color,
                         animal_id,
                         date_score_recorded = .x,
                         hair_score = .y,
                         age,
                         calving_season,
                         toxic_fescue,
                         comment,
                         source_file,
                         source_tab)) %>%
      reduce(bind_rows) %>%
      filter(!is.na(hair_score))
    }) %>% 
  reduce(bind_rows) %>% 
  distinct() %>% 
  select(-registration_number) %>% 
  rename(registration_number = animal_id) %>% 
  mutate(date_score_recorded = lubridate::ymd(date_score_recorded),
         hair_score = as.integer(hair_score),
         year = 2016,
         farm_id = "BAT")

#' 
#' ## `GeneSeek plate sheet final computer version 10052016 (2).xlsx`
#' 
## ---- warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------
savbat_16 <-
  c("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/GeneSeek plate sheet final computer version 10052016 (2).xlsx") %>% 
  purrr::map(function(.x) {
    
    # Vector of column names
    (nms <-
       names(read_excel(.x,
                        sheet = "Sheet1")))
    
    # Assign column types based on column name
    (ct <-
        if_else(str_detect(nms, "date|Date"), "date", "text"))
    
    read_excel(.x,
               sheet = "Sheet1",
               col_types = ct,
               trim_ws = TRUE,
               na = ".") %>%
      janitor::clean_names() %>%
      janitor::remove_empty(which = c("rows", "cols")) %>% 
      dplyr::select(-one_of("shearing", "farm", "ge_epd"))
    }) %>% 
  purrr::map(function(df = .x) {
    
    dates <-
      colnames(df) %>%
      str_subset(., "recorded_[[:digit:]]+")
    
    scores <-
      colnames(df) %>%
      str_subset(., "score_[[:digit:]]+")
    
    assertthat::validate_that(assertthat::are_equal(length(dates), length(scores)))
    
    purrr::map2(.x = dates,
                .y = scores,
                ~ df %>%
                  select(dna_plate:notes,
                         date_score_recorded = .x,
                         hair_score = .y)) %>%
      reduce(bind_rows) %>%
      filter(!is.na(hair_score))
    
  }) %>% 
  reduce(bind_rows) %>% 
  select(-animal_id) %>% 
  rename(animal_id = 4, # U of A ID original --> animal ID
         registration_number = 3 # ID for Neogen --> registration_number
         ) %>% 
  mutate(date_score_recorded = lubridate::ymd(date_score_recorded),
         hair_score = as.integer(hair_score),
         farm_id = str_extract(registration_number, "BAT|SAV"),
         year = case_when(str_detect(notes, "20[[:digit:]]{2}") ~ as.integer(str_extract(notes, "20[[:digit:]]{2}")),
                          TRUE ~ as.integer(2016)),
         source_file = "GeneSeek plate sheet final computer version 10052016 (2).xlsx",
         source_tab = "Sheet1")


#' 
#' ## `UA_Batesville_2017HS_JEK.xlsx`
#' 
## ---- warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------

bat_17 <-
  excel_sheets("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/UA_Batesville_2017HS_JEK.xlsx") %>%
  purrr::set_names() %>%
  purrr::map(~ read_excel("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/UA_Batesville_2017HS_JEK.xlsx",
                          sheet = .x,
                          trim_ws = TRUE,
                          na = ".")) %>%
  # Pivot hair score columns from wide to long
  purrr::map(~ tidyr::pivot_longer(.x,
                          # Exclude first two columns
                          cols = matches("[[:digit:]]+"))) %>%
  # Rename as necessary
  purrr::imap(~ select(.x,
                       registration_number = contains("Genotype"),
                       animal_id = contains("#"),
                       date_score_recorded = name,
                       hair_score = value) %>%
      # Convert excel numeric dates to dates
      mutate(date_score_recorded = janitor::excel_numeric_to_date(as.numeric(date_score_recorded)),
             hair_score = as.integer(hair_score),
             # Add source column as comment
             comment = .y,
             source_tab = .y,
             source_file = "UA_Batesville_2017HS_JEK.xlsx") %>%
      mutate_at(vars("registration_number", "animal_id"), ~ as.character(.)) %>%
      filter(!is.na(hair_score))) %>%
  reduce(bind_rows) %>% 
  mutate(farm_id = "BAT",
         sex = "F",
         calving_season = case_when(str_detect(comment, "Sp|sp") ~ "SPRING",
                                    str_detect(comment, "fall|Fall") ~ "FALL"),
         age = case_when(comment == "first calf heifers" ~ "2",
                         comment == "fall breeding heifers-virgin" ~ "1",
                         comment == "Sp6000 heifers for min trial" ~ "1"),
         year = 2017)

#' 
#' ## `2018 HCS.xlsx`
#' 
## ---- warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------
sav_18 <-
  read_excel("/Users/harlyjanedurbin/Box Sync/HairShedding/ReportedData/UniversityofArkansas/2018 HCS.xlsx", 
             na = ".") %>% 
  tidyr::pivot_longer(cols = -ID) %>% 
  # Powell said they scored on approximately the 15th of every month but didn't know exactly whey
  mutate(date_score_recorded = case_when(name == "March" ~ lubridate::ymd("2018-03-15"),
                                         name == "April" ~ lubridate::ymd("2018-04-15"),
                                         name == "May" ~ lubridate::ymd("2018-05-15"),
                                         name == "June" ~ lubridate::ymd("2018-06-15"),
                                         name == "July" ~ lubridate::ymd("2018-07-15")),
         hair_score = as.integer(value),
         farm_id = "SAV",
         sex = "F",
         calving_season = "FALL",
         year = 2018,
         source_file = "2018 HCS.xlsx") %>% 
  filter(!is.na(hair_score)) %>% 
  select(animal_id = ID, everything(), -name, -value)

#' 
#' # Check resulting data frames
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(sav_all)

str(bat_16)

str(savbat_16)

str(bat_17)

#' 
#' # Combine
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ua_join <-
  # Bind rows
  bind_rows(sav_all, bat_16, savbat_16, bat_17, sav_18) %>% 
  # if animal id is missing but reg isn't, pull all alphanumeric after SAV or BAT and call that animal ID
  mutate(animal_id = case_when(is.na(animal_id) & !is.na(registration_number) ~ str_extract(registration_number, "(?<=SAV|BAT)[[:alnum:]]+$"),
                               TRUE ~ animal_id),
         # If sex missing but not reg, pull sex from reg
         sex = case_when(is.na(sex) & !is.na(registration_number) ~ str_extract(registration_number, "F|M"),
                         TRUE ~ sex),
         # Correctly formatted animal ID
         f_animal_id = case_when(!is.na(animal_id) ~ str_pad(animal_id, width = 4, side = "left", pad = "0")),
         # Correctly formatted reg
         f_registration_number = case_when(!is.na(registration_number) ~ glue::glue("{str_extract(registration_number,'F|M')}00UA-{farm_id}{f_animal_id}"),
                                           !is.na(f_animal_id) ~ glue::glue("{sex}00UA-{farm_id}{f_animal_id}")),
         dna_id = if_else(!is.na(dna_plate), glue::glue("{dna_plate} {well}"), NA_character_),
         comment = if_else(!is.na(notes), notes, comment)) %>% 
  # Month must be between March and August
  filter(lubridate::month(date_score_recorded) %in% c(3:8)) %>% 
  # Might be mistakes, drop them just in case
  filter(lubridate::year(date_score_recorded) == year) %>% 
  select(-notes, -well, -dna_plate)

#' 
#' # Sanity check raw combined dataframe
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ua_join %>% 
  group_by(str_length(f_animal_id)) %>% 
  tally()

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ua_join %>% 
  group_by(str_length(animal_id)) %>% 
  tally()

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ua_join %>% 
  filter(str_length(f_animal_id) != 4 & !is.na(f_animal_id)) 

#' 
#' # Match to lab IDs using multiple possible IDs in the Animal Table, further cleaning
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ua_clean <-
  ua_join %>%
  # f_reg --> Reg
  id_search(source_col = f_registration_number,
            search_df = animal_table2,
            search_col = Reg,
            key_col = Lab_ID) %>%
  # dirty reg --> Reg
  id_search(source_col = registration_number,
            search_df = animal_table2,
            search_col = Reg,
            key_col = Lab_ID) %>%
  # DNA ID --> Ref_ID2
  id_search(source_col = dna_id,
            search_df = animal_table2,
            search_col = Ref_ID2,
            key_col = Lab_ID) %>%
  select(farm_id,
         breed_code,
         sex,
         color,
         registration_number = f_registration_number,
         animal_id = f_animal_id,
         date_score_recorded,
         hair_score,
         age,
         calving_season,
         toxic_fescue,
         comment,
         year,
         Lab_ID) %>%
  # Savoy fall calving, grazes hot fescue
  # Assuming Batesville herd does as well
  mutate(color = if_else(registration_number %in% c("F00UA-BAT2240"), "Grey", color),
         calving_season = if_else(farm_id %in% c("SAV"), "FALL", calving_season),
         toxic_fescue = if_else(farm_id %in% c("SAV"), "YES", toxic_fescue),
         age = as.integer(age)) %>%
  # Fill in NA Lab ID, color, breed code 
  group_by(registration_number) %>%
  fill(breed_code, .direction = "downup") %>%
  fill(color, .direction = "downup") %>%
  fill(Lab_ID, .direction = "downup") %>%
  ungroup() %>%
  # Fill in NA age, comment, fescue, calving season
  group_by(registration_number, date_score_recorded) %>%
  fill(age, .direction = "downup") %>%
  fill(comment, .direction = "downup") %>%
  fill(toxic_fescue, .direction = "downup") %>%
  fill(calving_season, .direction = "downup") %>%
  ungroup() %>%
  distinct() %>%
  mutate(breed_code = if_else(is.na(breed_code), "CROS", breed_code)) %>%
  arrange(registration_number, date_score_recorded) %>%
  select(registration_number,  everything()) %>% 
  impute_age(id_vars = c("registration_number", "animal_id")) %>% 
  group_by(farm_id) %>% 
  mutate(temp_id = as.integer(factor(registration_number))) %>% 
  ungroup()

#' 
#' # If more than one breed code across records, call it CROS
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ua_clean %<>% 
  mutate(breed_code = case_when(breed_code %in% c("Hereford", "HFD") ~ "HFD",
                                breed_code %in% c("GEL", "BAL") ~ "GEL",
                                breed_code %in% c("SIM", "SIMAN") ~ "SIM",
                                TRUE ~ breed_code)) %>% 
  group_by(registration_number, farm_id, Lab_ID) %>% 
  mutate(breed_code = case_when(n_distinct(breed_code) > 1 ~ "CROS",
                                TRUE ~ breed_code)) %>% 
  ungroup()

#' 
#' # Export 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ua_clean %>% 
  visdat::vis_miss()

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
readr::write_rds(ua_clean, here::here("data/derived_data/import_join_clean/ua_clean.rds"))


