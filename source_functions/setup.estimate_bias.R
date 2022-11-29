library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(glue)
library(magrittr)
library(tidylog)

source(here::here("source_functions/three_gen.R"))

# Setup

## Arguments
model <- as.character(commandArgs(trailingOnly = TRUE)[1])

iter <- as.numeric(commandArgs(trailingOnly = TRUE)[2])

pct_excl <- as.numeric(commandArgs(trailingOnly = TRUE)[3])

hair_col <- as.numeric(commandArgs(trailingOnly = TRUE)[4])

in_path <- glue("data/derived_data/aireml_varcomp/{model}/data.txt")

out_path <- glue("data/derived_data/estimate_bias/{model}/{iter}")

print(glue("Model {model} iteration {iter}, excluding phenotypes from random {pct_excl} of animals"))

## List of animals with genotypes
genotyped <- 
  read_csv(here::here("data/derived_data/grm_inbreeding/mizzou_hairshed.diagonal.full_reg.csv"))

## Full pedigree
full_ped <- read_rds(here::here("data/derived_data/3gen/full_ped.rds"))

## Data from full AIREML run
full_dat <-
  read_table2(here::here(in_path),
              col_names = FALSE) %>% 
  rename(full_reg = 1,
         hair_score = hair_col)

## Function to set a fraction of phenotypes to missing
choose_validation_set <-
  function(df, frac) {
    
    # Registration numbers whose phenotypes will be dropped
    drop <-
      df %>%
      distinct(full_reg) %>%
      sample_frac(frac) %>%
      pull(full_reg)
    
    # Set all phenotypes of validation animals to missing
    val_set <-
      df %>%
      filter(full_reg %in% drop) %>%
      mutate(hair_score = -999)
    
    # Animals with retained phenotypes
    train_set <-
      df %>%
      filter(!full_reg %in% drop)
    
    bind_rows(train_set, val_set)
    
  }

# Choose validation animals

subset_dat <- 
  choose_validation_set(full_dat, pct_excl)

subset_dat %>% 
  write_delim(here::here(glue("{out_path}/data.txt")),
              delim = " ",
              col_names = FALSE)

# Pedigree

ped <-
  subset_dat %>% 
  distinct(full_reg) %>% 
  left_join(full_ped %>% 
              select(full_reg, sire_reg, dam_reg)) %>% 
  three_gen(full_ped = full_ped) 

ped %>% 
  write_delim(here::here(glue("{out_path}/ped.txt")),
              delim = " ",
              col_names = FALSE)

# Genotype pull list

ped %>% 
  select(full_reg) %>% 
  bind_rows(ped %>% 
              select(full_reg = sire_reg)) %>% 
  bind_rows(ped %>% 
              select(full_reg = dam_reg)) %>% 
  distinct() %>% 
  filter(full_reg %in% genotyped$full_reg) %>% 
  select(full_reg) %>% 
  write_delim(here::here(glue("{out_path}/pull_list.txt")),
              delim = " ",
              col_names = FALSE)



