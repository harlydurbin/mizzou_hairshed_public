#' ---
#' title: "Breed composition"
#' author: "Harly Durbin"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(glue)
library(readxl)
library(magrittr)
library(tidylog)

source(here::here("source_functions/coalesce_join.R"))
source(here::here("source_functions/iterative_id_search.R"))

#' 
#' # Notes & questions
#' 
#' * ~~Send Jordan Bowman ped from April and ask for breed comp~~
#' * Pull together ANR ped from April, add new animals, ask Ryan Bolt for breed comp
#' * Run for breeds with > 1,500 records? AN, SIM, HFD, ANR, BG, CHA
#' 
#' # Setup
#' 
## ---- warning=FALSE, message=FALSE------------------------------------------------------------------------------------------------
cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
rhf_breed <- 
  read_rds(here::here("data/raw_data/breed_key/201005.rhf_breed.rds"))

#' 
#' ## Red Angus key
#' 
#' ### Sent by Ryan Boldt
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
ran_breed <-
  read_csv(here::here("data/raw_data/breed_key/201022.ranusa.brd.code.101520.csv"),
           na = ".",
           skip = 1,
           col_names = c("registration_number", "brd", "pct")) %>% 
  # Rescale breed percentage
  mutate(pct = pct / 1024,
         cross = if_else(pct == 1, brd, "CROS")) %>%
  tidyr::pivot_wider(id_cols = c("registration_number", "cross"),
                     names_from = brd,
                     values_from = pct) %>% 
  janitor::clean_names() %>% 
  select(registration_number, cross, ar, hfd = hp, sim = sm) %>% 
  mutate_at(vars("ar", "hfd", "sim"), ~ replace_na(., 0))


#' 
#' ### Local Adaptation project
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
ran_breed %<>% 
  bind_rows(read_csv(here::here("data/raw_data/breed_key/201005.raaa.animal.20180501.csv"), 
                     na = ".") %>% 
              mutate(brd = "AR") %>% 
              select(registration_number = regisno, brd, pct = frac1) %>% 
              bind_rows(read_csv(here::here("data/raw_data/breed_key/201005.raaa.animal.20180501.csv"), 
                                 na = ".") %>% 
                          select(registration_number = regisno, brd = brd2, pct = frac2)) %>% 
              bind_rows(read_csv(here::here("data/raw_data/breed_key/201005.raaa.animal.20180501.csv"), 
                                 na = ".") %>% 
                          select(registration_number = regisno, brd = brd3, pct = frac3)) %>% 
              filter(!is.na(pct)) %>% 
              group_by(registration_number, brd) %>% 
              # Duplicates for some reason
              filter(n() == 1) %>% 
              ungroup() %>% 
              mutate(pct = pct/1000,
                     cross = if_else(pct == 1, brd, "CROS")) %>% 
              tidyr::pivot_wider(names_from = brd,
                                 values_from = pct) %>% 
              janitor::clean_names() %>% 
              mutate_at(vars(ar:sa), ~ tidyr::replace_na(., 0)) %>% 
              mutate(hfd = hp+hh,
                     ar = case_when(ar > 0 && aa > 0 ~ ar+aa,
                                    TRUE ~ ar),
                     cross = if_else(ar == 1, "AR", cross)) %>% 
              select(registration_number, cross, ar, sim = sm, gv, br, hfd))

#' 
#' ### Misc. downloaded by me
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
ran_breed %<>% 
  bind_rows(read_csv(here::here("data/raw_data/breed_key/201005.RAN.AnimalList.csv")) %>% 
              select(registration_number = `RAAA#`, brd = BrdCds) %>% 
              mutate(ar = case_when(brd == "100% AR" ~
                                      "100",
                                    str_detect(brd, "(?<=[[:digit:]]% )AR") ~
                                      str_extract(brd, "[[:graph:]]+(?=% AR)")),
                     sim = case_when(str_detect(brd, "(?<=[[:digit:]]% )SM") ~
                                       str_extract(brd, "[[:graph:]]+(?=% SM)")),
                     hfd = case_when(str_detect(brd, "(?<=[[:digit:]]% )HP") ~
                                       str_extract(brd, "[[:graph:]]+(?=% HP)"))) %>% 
              mutate_at(vars("ar", "sim", "hfd"), ~ as.numeric(.)) %>% 
              mutate_at(vars("ar", "sim", "hfd"), ~ ./100) %>% 
              mutate_at(vars("ar", "sim", "hfd"), ~ tidyr::replace_na(., 0)) %>% 
              mutate(cross = case_when(ar == 1 ~ "AR",
                                       sim == 1 ~ "SIM",
                                       hfd == 1 ~ "HFD",
                                       TRUE ~ "CROS")) %>% 
              select(-brd))

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
ran_breed %<>% 
  mutate_at(vars(ar:br), ~ tidyr::replace_na(., 0)) %>% 
  distinct() %>% 
  group_by(registration_number) %>% 
  arrange(cross) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(brd_source = glue("RAAA{row_number()}")) 

#' 
#' ## Simmental key
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
sim_breed <-
  read_csv(here::here("data/raw_data/breed_key/201005.SIM.breeds.csv")) %>% 
  select(registration_number = asa_nbr, brd = breed_code, pct) %>% 
  mutate(cross = if_else(pct == 1, brd, "CROS")) %>% 
  tidyr::pivot_wider(id_cols = c("registration_number", cross),
                     names_from = brd,
                     values_from = pct) %>% 
  mutate_at(vars(SM:SH), ~ replace_na(., 0)) %>% 
  janitor::clean_names() %>% 
  # Don't differentiate between horned and polled Hereford
  mutate(hfd = hh + hp,
         # Don't differentiate between purebred and commercial 
         sim = sm + cs) %>% 
  select(registration_number, cross, an, ar, br, chi = ca, gv, hfd, sim) %>% 
  # Don't differentiate between Red Angus and Angus since RAAA doesn't
  mutate(ar = case_when(ar != 0 & an != 0 & br == 0 & chi == 0 & gv == 0 & hfd == 0 & sim == 0 ~ ar + an,
                        TRUE ~ ar),
         ar = if_else(ar > 1, 1, ar),
         cross = case_when(ar == 1 ~ "AR",
                           (ar + an) == 1 ~ "AR",
                           sim == 1 ~ "SIM",
                           an == 1 ~ "AN",
                           TRUE ~ cross),
         brd_source = glue("ASA{row_number()}")) 
#' 
#' # Breed key
#' 
#' ## RHF data, misc. data
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
breed_key <-
  cleaned %>% 
  distinct(farm_id, temp_id, animal_id, registration_number, breed_code, Lab_ID) %>% 
  coalesce_join(rhf_breed, 
                by = c("farm_id", "animal_id", "Lab_ID", "temp_id"),
                join = dplyr::left_join)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
breed_key %<>%
  coalesce_join(read_excel(here::here("data/raw_data/breed_key/201005.misc_breed.xlsx")),
                by = c("farm_id", "animal_id", "registration_number"),
                join = dplyr::left_join)

#' 
#' ## Impute `full_reg` and join to `full_ped`
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
breed_key %<>%
  left_join(full_ped %>% 
              distinct(farm_id, temp_id, full_reg, source) %>% 
              filter(!is.na(farm_id))) %>% 
  mutate(breed_code = case_when(breed_code == "AN" ~ "AAA",
                                breed_code == "ANR" ~ "RAN",
                                breed_code == "BG" ~ "BGR",
                                breed_code == "BRN" ~ "BSW",
                                breed_code == "MAAN" ~ "RDP",
                                TRUE ~ breed_code),
         full_reg = case_when(is.na(full_reg) &
                                !is.na(registration_number) ~ glue("{breed_code}{registration_number}"),
                              is.na(full_reg) &
                                is.na(registration_number) ~ glue("{farm_id}{animal_id}{temp_id}"),
                              TRUE ~ full_reg),
         hfd = case_when(source == "American Hereford Association" ~ 1,
                         farm_id %in% c("RCR") ~ 1,
                         TRUE ~ hfd),
         an = case_when(source == "American Angus Association" ~ 1, 
                        farm_id %in% c("UMCT", "WAA", "HAF", "KAF", "DRR", "RAAF") ~ 1,
                        TRUE ~ an),
         ar = case_when(farm_id %in% c("SRA") ~ 1,
                        TRUE ~ ar),
         brd_source = case_when(!is.na(brd_source) ~ brd_source,
                                is.na(brd_source) & !source %in% c("American Simmental Association",
                                                                   "Red Angus Association of America") ~ source,
                                # Known HFD and AN and RAN
                                hfd == 1 | an == 1 | ar == 1 ~ farm_id,
                                TRUE ~ brd_source))

#' 
#' ## Search for breed comp in `sim_breed`, `ran_breed`
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
breed_key %<>% 
  id_search(source_col = full_reg,
            search_df = sim_breed %>% 
              mutate(dummy_reg = glue("SIM{registration_number}")),
            search_col = dummy_reg,
            key_col = brd_source) %>% 
  id_search(source_col = full_reg,
            search_df = ran_breed %>% 
              mutate(dummy_reg = glue("RAN{registration_number}")),
            search_col = dummy_reg,
            key_col = brd_source) %>% 
  id_search(source_col = registration_number,
            search_df = sim_breed,
            search_col = registration_number,
            key_col = brd_source) %>% 
  id_search(source_col = registration_number,
            search_df = ran_breed %>% 
              mutate(registration_number = as.character(registration_number)),
            search_col = registration_number,
            key_col = brd_source) %>% 
  coalesce_join(ran_breed %>% 
                  select(-registration_number),
                by = c("brd_source"),
                join = dplyr::left_join) %>% 
  coalesce_join(sim_breed %>% 
                  select(-registration_number),
                by = c("brd_source"),
                join = dplyr::left_join)

#' 
#'
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
breed_key %<>%
  mutate(cross = case_when(!is.na(cross) ~ cross,
                           farm_id %in% c("BAT", "SAV") ~ "CROS",
                           breed_code %in% c("SH",
                                             "RDP",
                                             "HFD",
                                             "BSW",
                                             "SIMB",
                                             "BGR",
                                             "CHA",
                                             "GEL",
                                             "CROS",
                                             "CHIA") ~ breed_code,
                           str_detect(full_reg, "AAA|BIR") ~ "AN",
                           str_detect(registration_number, "^6") ~ "AN",
                           an == 1 ~ "AN",
                           hfd == 1 ~ "HFD",
                           ar == 1 ~ "RAN",
                           TRUE ~ cross)) 


#' 
## ---------------------------------------------------------------------------------------------------------------------------------
breed_key %<>% 
  mutate(cross = case_when(cross %in% c("CROS", "cross", "MX") ~ "CROS",
                           cross %in% c("SIM", "SM") ~ "SIM",
                           cross %in% c("RAN", "AR") ~ "RAN",
                           is.na(cross) ~ "CROS",
                           TRUE ~ cross),
         brd_source = as.character(brd_source),
         brd_source = case_when(str_detect(brd_source, "^ASA") ~ "American Simmental Association",
                                str_detect(brd_source, "simmisc") ~ "American Simmental Association",
                                str_detect(brd_source, "^RAAA") ~ "Red Angus Association of America",
                                str_detect(brd_source, "ranmisc") ~ "Red Angus Association of America",
                                TRUE ~ brd_source)) %>% 
  select(Lab_ID, farm_id, animal_id, temp_id, full_reg, breed_code, cross, source, brd_source, an, ar, br, chi, gv, hfd, sim) %>% 
  mutate_at(vars(an:sim), ~ replace_na(., 0))

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
breed_key %>% 
  write_rds(here::here("data/derived_data/breed_key/breed_key.rds"))

#' 
