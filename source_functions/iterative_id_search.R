
# source_df contains the source_col you're trying to use for matching
# search_df contains the search_col that could contain matches to the source_col
# and the key_col of what you're trying to assign to source_col

id_search <- 
  
  function(source_df, source_col, search_df, search_col, key_col, match_source = FALSE) {

    search_col <- rlang::enquo(search_col)
    
    source_col <- rlang::enquo(source_col)
    
    key_col <- rlang::enquo(key_col)
    
    fncols <- function(data, cname) {
      add <-cname[!cname%in%names(data)]
      
      if(length(add)!=0) data[add] <- NA
      data
    }
  
    # Add key col if doesn't exist in source df
    source_df <-
      fncols(data = source_df, cname = c(rlang::quo_name(key_col), rlang::quo_name(source_col)))
      
    choices <- 
      search_df %>%
      # Select the column to search thru + key_col
      dplyr::select(!!search_col, !!key_col) %>% 
      # Set column names
      dplyr::rename(right_col := !!search_col) %>% 
      # Remove rows where the column of interest is NA
      dplyr::filter(!is.na(right_col))
    
    add <-  
      if(match_source == FALSE){
      source_df %>%
      dplyr::filter(is.na(!!key_col)) %>%
      dplyr::select(-!!key_col) %>% 
      dplyr::rename(left_col := !!source_col) %>% 
      dplyr::left_join(
        choices,
        by = c("left_col" = "right_col")
      ) %>% 
      dplyr::rename(!!source_col := left_col)
      } else {
        source_df %>%
          dplyr::filter(is.na(!!key_col)) %>%
          dplyr::select(-!!key_col) %>% 
          dplyr::rename(left_col := !!source_col) %>%
          dplyr::left_join(
            choices,
            by = c("left_col" = "right_col")
          ) %>% 
          dplyr::rename(!!source_col := left_col) %>% 
          dplyr::mutate(
            match_source = dplyr::case_when(
              !is.na(!!key_col) ~ glue::glue("{rlang::quo_name(source_col)}-{rlang::quo_name(search_col)}"),
              TRUE ~ NA_character_
              )
            )
      }
    
    found <- 
      add %>% 
      dplyr::filter(!is.na(!!key_col)) %>% 
      #dplyr::pull(!!source_col) %>% 
      dplyr::summarise(found = n()) %>% 
      dplyr::pull(found)
    
    updated <-
      source_df %>% 
      dplyr::filter(!is.na(!!key_col)) %>% 
      dplyr::bind_rows(add)
    
    missing <-
      updated %>% 
      dplyr::filter(is.na(!!key_col)) %>% 
      dplyr::summarise(missing = n()) %>% 
      dplyr::pull(missing)
    
    print(glue::glue("Found {found} new matches, {missing} still unmatched"))
    
    updated

    
  }