biv_heritability <-
  
  function(df,
           abbrvs,
           descs,
           mat = NULL,
           mpe = NULL,
           pe = NULL) {
    
    r1_abbrv <- abbrvs[[1]]
    r2_abbrv <- abbrvs[[2]]
    
    r1_desc <- descs[[1]]
    r2_desc <- descs[[2]]
    
    spread1 <-
      # Takes the "long" output of melt_aireml
      # One row for each variance or covariance
      df %>%
      mutate(
        key =
          case_when(
            str_detect(val1, r1_abbrv) ~ r1_desc,
            str_detect(val1, r2_abbrv) ~ r2_desc,
          ),
        # Direct/maternal covariance
        val1 =
          case_when(
            val1 == glue::glue("{r1_abbrv}_dir") &
              val2 == glue::glue("{r1_abbrv}_mat") ~ glue::glue("{r1_abbrv}_dir_mat"),
            val1 == glue::glue("{r2_abbrv}_dir") &
              val2 == glue::glue("{r2_abbrv}_mat") ~ glue::glue("{r2_abbrv}_dir_mat"),
            TRUE ~ val1
          ),
        val2 = if_else(str_detect(val1, "dir_mat"), val1, val2)
      ) %>%
      filter(val1 == val2) %>%
      select(-val2) %>%
      # Pivot to wide format
      mutate(val1 = str_remove(val1, glue::glue("{r1_abbrv}_|{r2_abbrv}_"))) %>%
      tidyr::pivot_wider(id_cols = key,
                         names_from = val1,
                         values_from = var_cov)
    
    # Determine effects
    effects <- colnames(spread1)
    
    # Create a sum/denominator column
    spread1$denominator <-
      rowSums(spread1[sapply(spread1, is.numeric)], na.rm = TRUE)
    
    # If maternal effect fit, put in 2*cov(dir, mat)
    # 12/15/19: Stop using 2*dir_mat
    spread2 <- spread1
      # if ("dir_mat" %in% effects)
      #   spread1 %>% mutate(denominator = denominator - dir_mat,
      #                      denominator = denominator + (2 * dir_mat))
    # else
    #   spread1
    
    mat_h2 <-
      if (!is.null(mat)) {
        spread2 %>%
          group_by(key) %>%
          summarise(!!glue::glue("Maternal h2") := mat / denominator) %>%
          ungroup() %>%
          arrange(key) %>%
          select(-key)
      }
    
    mpe_c2 <-
      if (!is.null(mpe)) {
        spread2 %>%
          group_by(key) %>%
          summarise(!!glue::glue("MPE c2") := mpe / denominator) %>%
          ungroup() %>%
          arrange(key) %>%
          select(-key)
      }
    
    repeatability <-
      if (!is.null(pe)) {
        spread2 %>%
          group_by(key) %>%
          summarise(!!glue::glue("Repeatability") := sum(dir, pe) / denominator) %>%
          ungroup() %>%
          arrange(key) %>%
          select(-key)
      }
    
    spread2 %>%
      group_by(key) %>%
      summarise(!!glue::glue("Direct h2") := dir / denominator,
                `Total variance` = denominator) %>%
      ungroup() %>%
      arrange(key) %>%
      bind_cols(mat_h2,
                mpe_c2,
                repeatability) %>%
      select(key, `Total variance`, `Direct h2`, everything())
  }

univ_heritability <-
  
  function(df, abbrv, desc, mat = NULL, mpe = NULL, pe = NULL) {
    
    spread1 <-
      df %>%
      mutate(key = desc,
             # Direct/maternal covariance
             val1 = case_when(str_detect(val1, "dir") & str_detect(val2, "mat") ~ glue::glue("{abbrv}_dir_mat"),
                              TRUE ~ val1),
             val2 = if_else(str_detect(val1, "dir_mat"), val1, val2)) %>%
      filter(val1 == val2) %>%
      select(-val2) %>% 
      mutate(val1 = str_remove(val1, glue::glue("{abbrv}_"))) %>%
      tidyr::pivot_wider(names_from = val1,
                         values_from = var_cov)
    
    # Determine effects
    effects <- colnames(spread1)
    
    # Create a sum/denominator column
    spread1$denominator <-
      rowSums(spread1[sapply(spread1, is.numeric)], na.rm = TRUE)
    
    # If maternal effect fit, put in 2*cov(dir, mat)
    # 12/15/19: Stop using 2*dir_mat
    spread2 <- spread1
    # spread2 <-
    #   if ("dir_mat" %in% effects)
    #     spread1 %>% mutate(denominator = denominator - dir_mat,
    #                        denominator = denominator + (2 * dir_mat))
    # else
    #   spread1
    
    mat_h2 <-
      if (!is.null(mat)) {
        spread2 %>%
          group_by(key) %>%
          summarise(!!glue::glue("Milk h2") := mat / denominator) %>%
          ungroup() %>%
          arrange(key) %>%
          select(-key)
      }  
    
    mpe_c2 <-
      if (!is.null(mpe)) {
        spread2 %>%
          group_by(key) %>%
          summarise(!!glue::glue("MPE c2") := mpe / denominator) %>%
          ungroup() %>%
          arrange(key) %>%
          select(-key)
      }
    
    repeatability <-
      if (!is.null(pe)) {
        spread2 %>%
          group_by(key) %>%
          summarise(!!glue::glue("Repeatability") := sum(dir, pe) / denominator) %>%
          ungroup() %>%
          arrange(key) %>%
          select(-key)
      }
    
    spread2 %>%
      group_by(key) %>%
      summarise(!!glue::glue("Direct h2") := dir / denominator,
                `Total variance` = denominator) %>%
      ungroup() %>%
      arrange(key) %>%
      bind_cols(mat_h2,
                mpe_c2,
                repeatability) %>%
      select(key, `Total variance`, `Direct h2`, everything())
    
  }