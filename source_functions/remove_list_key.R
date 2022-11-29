library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(glue)

# Used by blupf90_geno_format.snakefile to create decoder key of animals to be removed/IDs updated

# Setup
full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

cleaned <- read_rds(here::here("data/derived_data/import_join_clean/cleaned.rds"))

sample_table <-
  read_csv(here::here("data/raw_data/import_join_clean/200820_sample_sheet.csv"),
           trim_ws = TRUE,
           guess_max = 100000)

remove_list <- 
  read_table2(here::here("data/derived_data/seekparentf90/parentage_conflicts.removelist"),
              col_names = "full_reg")

geno_prefix <- as.character(commandArgs(trailingOnly = TRUE)[1])

# Import fam file
fam <-
  read_table2(here::here(glue("{geno_prefix}.fam")),
              col_names = FALSE) %>%
  select(international_id = 1)

key <-
  fam %>%
  left_join(sample_table %>%
              distinct(lab_id, international_id)) %>%
  rename(Lab_ID = lab_id) %>%
  left_join(full_ped %>%
              select(full_reg, Lab_ID) %>%
              distinct()) %>%
  distinct(international_id, full_reg) %>%
  group_by(international_id) %>%
  fill(full_reg, .direction = "downup") %>%
  ungroup() %>%
  distinct() %>%
  mutate(dummy_reg = str_remove(international_id, "(?<=M|F)0+(?=[1-9])"),
         full_reg = case_when(is.na(full_reg) ~
                                glue::glue("{str_extract(dummy_reg, '^[[:upper:]]{3}')}{str_extract(dummy_reg, '(?<=M|F)[[:digit:]]+')}"),
                              TRUE ~
                                full_reg),
         full_reg = str_replace(full_reg, "AAN", "AAA"))

# Write out PLINK --update-id file
fam %>% 
  left_join(key) %>%
  assertr::verify(length(full_reg) == length(fam$international_id)) %>%
  assertr::verify(!is.na(full_reg)) %>%
  mutate(fid = international_id, new_fid = full_reg) %>% 
  select(international_id, fid, full_reg, new_fid) %>%
  write_tsv(here::here(glue::glue("{geno_prefix}.update_id.txt")),
            col_names = FALSE)

remove_list %>% 
  left_join(key) %>%
  assertr::verify(length(international_id) == length(remove_list$full_reg)) %>%
  assertr::verify(!is.na(international_id)) %>% 
  mutate(fid = full_reg) %>% 
  select(full_reg, fid) %>% 
  write_tsv(here::here(glue::glue("{geno_prefix}.remove.txt")),
            col_names = FALSE) 