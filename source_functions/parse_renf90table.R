
parse_renf90table <-
  function(path, effect_num, effect_key = FALSE) {
    raw <-
      readr::read_lines(path)
    
    rownum <-
      which(stringr::str_detect(raw, glue::glue("Effect group {effect_num}")))
    
    levels <-
      raw[rownum] %>%
      stringr::str_extract("(?<=with )[[:digit:]]+(?= levels)") %>%
      as.numeric()
    
    effect_table <-
      readr::read_table2(path,
                         skip = rownum + 1,
                         n_max = levels,
                         col_names = c("id_original", "n", "id_renamed")) %>% 
      mutate(id_original = as.character(id_original),
             id_renamed = as.character(id_renamed))
    
    effect_table <- 
      if (effect_key == TRUE) {
        effect_table %>% 
          dplyr::mutate(effect = effect_num)
    } else effect_table
    
    return(effect_table)
    
  }