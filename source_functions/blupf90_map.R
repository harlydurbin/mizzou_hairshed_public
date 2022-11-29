library(readr)
library(tidyr)
library(glue)
library(dplyr)
library(stringr)
library(tidylog)

# Used by blupf90_geno_format.snakefile to format a map file the way BLUPF90 wants it because I'm lazy

bim_path <- as.character(commandArgs(trailingOnly = TRUE)[1])

geno_prefix <- as.character(commandArgs(trailingOnly = TRUE)[2])

bim <-
  read_table2(here::here(bim_path),
              col_names = FALSE)

bim %>%
  select(X1, X4) %>%
  mutate(SNP_ID = row_number()) %>%
  select(SNP_ID, CHR = X1, POS = X4) %>%
  write_delim(here::here(glue::glue("{geno_prefix}.chrinfo.txt")),
              delim = " ",
              col_names = FALSE)

bim %>%
  mutate(chrpos = glue::glue("{X1}:{X4}")) %>%
  select(chrpos) %>%
  write_delim(here::here(glue::glue("{geno_prefix}.chrpos.txt")),
              col_names = FALSE)
