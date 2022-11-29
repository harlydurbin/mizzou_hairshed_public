library(dplyr)
library(readr)

fam <- as.character(commandArgs(trailingOnly = TRUE)[1])

pedind <- as.character(commandArgs(trailingOnly = TRUE)[2])

read_table2(here::here(fam), col_names = FALSE) %>%
  select(-X6) %>%
  mutate(pop = "cow") %>%
  write_tsv(here::here(pedind), col_names = FALSE)
