impute_age <- function(df, id_vars) {

  
  lost <-
    # data frame of animals missing ages and which years they're missing them
    df %>%
    dplyr::filter(is.na(age)) %>%
    #distinct(farm_id, temp_id) %>%
    distinct(!!!rlang::syms(id_vars)) %>% 
    left_join(df) %>%
    #distinct(farm_id, temp_id, year, age) %>%
    distinct(!!!rlang::syms(id_vars), year, age) %>%
    arrange(year) %>%
    group_by(farm_id, temp_id, year) %>% 
    tidyr::fill(age, .direction = "downup") %>% 
    ungroup() %>% 
    distinct() %>% 
    group_by(farm_id, temp_id, year) %>% 
    filter(n() == 1) %>% 
    ungroup() %>% 
    tidyr::pivot_wider(
      #id_cols = c("farm_id", "temp_id"),
      id_cols = id_vars,
      #id_cols = c("registration_number", "animal_id"),
      names_from = year,
      values_from = age
    ) %>% 
    mutate_at(vars(-c("farm_id", "temp_id")), ~as.numeric(.))
    
  
  print("Generated 'lost' dataframe")
  
  years <-
    # All years with any missing data
    colnames(lost) %>%
    stringr::str_subset(., "[[:digit:]]") %>%
    as.integer(.)
  
  print("Generated 'years' dataframe")
  
  yearsdf <-
    # Expanded data frame with all years and how far apart they are
    tidyr::expand_grid(year1 = years, year2 = years) %>%
    dplyr::filter(year1 != year2) %>%
    dplyr::mutate(dist = year1 - year2) %>%
    dplyr::mutate_at(vars(contains("year")), ~ as.character(.))
  
  print("Generated 'yearsdf' dataframe")
  
  key <-
    purrr::pmap(
      list(
        x = yearsdf$year1,
        y = yearsdf$year2,
        z = yearsdf$dist
      ),
      .f = function(x, y, z) {
        # Add/impute ages
        lost %>%
          mutate(!!rlang::sym(x) := !!rlang::sym(x) + z)
      }
    ) 
  
  
  print("Generated 'key' dataframe")
  
  key %<>%
    # Join, replacing NAs
    reduce(coalesce_join, 
           #by = c("farm_id", "temp_id")) 
           by = id_vars) 
  
  print("Coalesce joined 'key'")
  
  key %<>%
           #by = c("registration_number", "animal_id")) %>% 
    tidyr::pivot_longer(
      #cols = -one_of("farm_id", "temp_id"),
      cols = -one_of(id_vars),
     # cols = -one_of("registration_number", "animal_id"),
      names_to = "year",
      values_to = "age2"
    ) %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(age2 > 0)
  
  print("Pivoted 'key'")
  
  # Bind original non-missing data to newly imputed data
  df %>%
    #dplyr::left_join(key, by = c("registration_number", "animal_id", "year")) %>%
    dplyr::left_join(key, by = c(id_vars, "year")) %>%
    dplyr::mutate(age = if_else(!is.na(age2), as.double(age2), as.double(age))) %>%
    dplyr::select(-age2)
}