library(rlang)
library(dplyr)

find_dup <- function(df, var) {
  
  group <- enquo(var)
  
  df %>%
    group_by(!!group) %>%
    filter(n() > 1 & !is.na(!!group)) %>% 
    ungroup() %>% 
    arrange(!!group)
  
}


# To run in a %>% chain you'd do
# 
# ... %>% 
#   find_dup(., group)

find_dup2 <- function(df, vars, dups) {
  
  dups <- enquo(dups)
  
  df %>%
    group_by(!!!syms(vars)) %>%
    filter(n() > 1 & !is.na(!!dups)) %>% 
    ungroup()
  
}