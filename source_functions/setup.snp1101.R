## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
library(glue)
library(purrr)
library(DRP)
library(magrittr)
library(stringr)
library(tidylog)

source(here::here("source_functions/calculate_acc.R"))

## -------------------------------------------------------------------------------------------------------------------------------
dir <- as.character(commandArgs(trailingOnly = TRUE)[1])

animal_effect <- as.numeric(commandArgs(trailingOnly = TRUE)[2])

gen_var <- as.numeric(commandArgs(trailingOnly = TRUE)[3])

h2 <- as.numeric(commandArgs(trailingOnly = TRUE)[4])

model <- str_extract(dir, "(?<=/)[[:alnum:]]+$")


## -------------------------------------------------------------------------------------------------------------------------------
# My full pedigree with all hair shedding animals
full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

# Pedigree created by BLUPF90 for the current model/dataset
renaddped <-
  read_table2(here::here(glue("{dir}/renadd0{animal_effect}.ped")),
            col_names = FALSE) %>%
  select(id_new = X1, sire_id = X2, dam_id = X3, full_reg = X10)

# Converting BLUPF90 renamed IDs back to registration numbers
renaddped %<>%
  left_join(renaddped %>%
              select(sire_id = id_new,
                     sire_reg = full_reg)) %>%
  left_join(renaddped %>%
              select(dam_id = id_new,
                     dam_reg = full_reg)) %>%
  select(id_new, full_reg, sire_reg, dam_reg) %>%
  filter(!is.na(full_reg)) %>%
  mutate_at(vars(contains("reg")), ~ replace_na(., "0")) %>% 
  filter(full_reg != "0")

# Appending animal sexes
renaddped %<>%
  left_join(full_ped %>%
              select(full_reg, sex)) %>%
  mutate(sex = case_when(full_reg %in% renaddped$sire_reg ~ "M",
                         full_reg %in% renaddped$dam_reg ~ "F",
                         TRUE ~ sex),
         sex = replace_na(sex, "F"))

# Calculate pedigree based inbreeding
pedinb <-
  renaddped %>%
  select(full_reg, sire_reg, dam_reg) %>%
  optiSel::prePed() %>%
  optiSel::pedInbreeding() %>%
  tibble::remove_rownames() %>%
  rename(full_reg = Indiv,
         f = Inbr)

## -------------------------------------------------------------------------------------------------------------------------------
trait <-
  read_table2(here::here(glue("{dir}/solutions")),
              col_names = c("trait", "effect", "id_new", "solution", "se"),
              skip = 1) %>%
  # limit to animal effect
  filter(effect == animal_effect) %>%
  select(id_new, solution, se) %>%
  # Re-attach original IDs
  left_join(renaddped) %>%
  left_join(pedinb) %>%
  # If missing inbreeding, replace with 0
  mutate(f = tidyr::replace_na(f, 0),
         acc = purrr::map2_dbl(.x = se,
                               .y = f,
                               # Calculate breeding value reliability given
                               # genetic variance, se column, f (inbreeding) column
                              ~ calculate_acc(u = gen_var,
                                              se = .x,
                                              f = .y,
                                              option = "reliability")),
         # If reliability less than 0, call it 0
         acc = if_else(0 > acc, 0, acc)) %>%
  filter(!is.na(full_reg)) %>%
  # Make sure no missing sires or dams
  assertr::verify(!is.na(sire_reg)) %>%
  assertr::verify(!is.na(dam_reg)) %>%
  assertr::verify(full_reg != "0")


## -------------------------------------------------------------------------------------------------------------------------------
# Append sire and dam accuracies
trait %<>%
  left_join(trait %>%
              select(sire_reg = full_reg, sire_acc = acc, sire_sol = solution)) %>%
  left_join(trait %>%
              select(dam_reg = full_reg, dam_acc = acc, dam_sol = solution)) %>%
  select(contains("reg"), contains("sol"), contains("acc"))


## -------------------------------------------------------------------------------------------------------------------------------
# De-regress breeding values using DRP package
drp <-
  wideDRP(Data = trait,
          animalId = "full_reg",
          sireId = "sire_reg",
          damId = "dam_reg",
          animalEBV = "solution",
          sireEBV = "sire_sol",
          damEBV = "dam_sol",
          animalr2 = "acc",
          sirer2 = "sire_acc",
          damr2 = "dam_acc",
          c = 0.1,
          h2 = h2) %>%
  # 1/6/21: Stop removing parent averages
  mutate(Anim_DRP_Trait_r2 = if_else(0 > Anim_DRP_Trait_r2, 0, Anim_DRP_Trait_r2),
         Group = 1,
         Rel = Anim_DRP_Trait_r2*100) 


drp %>%
  filter(!is.na(Anim_DRP_Trait)) %>% 
  # Remove infinite values, otherwise will fail with "Matrix is singular"
  filter(!is.infinite(Anim_DRP_Trait)) %>% 
  assertr::verify(Rel >= 0) %>% 
  # limit to only animals with genotypes
  # left_join(read_table2(here::here("data/derived_data/grm_inbreeding/mizzou_hairshed.grm.id"),
  #                       col_names = c("full_reg", "iid"))) %>%
  # filter(!is.na(iid)) %>% 
  # 1/6/21: Stop removing parent averages
  select(ID = full_reg, Group, Obs = Anim_DRP_Trait, Rel) %>%
  write_tsv(here::here(glue("data/derived_data/snp1101/{model}/trait.txt")))

renaddped %>%
  assertr::verify(!is.na(sire_reg)) %>%
  assertr::verify(!is.na(dam_reg)) %>%
  assertr::verify(full_reg != "0") %>%
  select(full_reg, sire_reg, dam_reg, sex) %>%
  write_tsv(here::here(glue("data/derived_data/snp1101/{model}/ped.{model}.txt")),
            col_names = FALSE)

