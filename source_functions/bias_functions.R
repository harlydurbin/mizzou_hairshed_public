

read_bias <-
  function(model, iter, hair_col, effect_num){
    
    # Path to full AIREML run
    full_path <- glue::glue("data/derived_data/aireml_varcomp/{model}")
    
    # Path to bias estimation iteration for given model
    subset_path <- glue::glue("data/derived_data/estimate_bias/{model}/{iter}")
    
    # Animals with phenotypes excluded in the given iteration
    dropped <-
      readr::read_table2(here::here(glue("{subset_path}/data.txt")),
                  col_names = FALSE) %>%
      select(full_reg = 1,
             hair_score = hair_col) %>% 
      dplyr::filter(hair_score == -999) %>%
      dplyr::pull(full_reg) %>%
      unique()
    
    bias_dat <-
      c(subset_path, full_path) %>%
      purrr::set_names("reduced", "full") %>%
      purrr::imap(~ readr::read_table2(here::here(glue("{.x}/solutions")),
                                       skip = 1,
                                       col_names = c("trait", "effect", "id_renamed", "solution")) %>%
                    dplyr::left_join(readr::read_table2(here::here(glue::glue("{.x}/renadd0{effect_num}.ped")),
                                                        col_names = FALSE) %>%
                                       dplyr::select(id_renamed = 1, full_reg = 10)) %>%
                    dplyr::mutate(analysis = .y)) %>%
      purrr::reduce(bind_rows) %>%
      dplyr::filter(effect == effect_num) %>%
      dplyr::select(-trait, -effect, -id_renamed) %>%
      tidyr::pivot_wider(values_from = solution,
                         names_from = analysis,
                         names_prefix = "solution_") %>%
      dplyr::mutate(group = dplyr::if_else(full_reg %in% dropped, "dropped", "kept"),
                    iter = iter)
    
    return(bias_dat)
    
  }

lr_prediction_acc <- 
  function(bias_df, sigma2_u, f_val) {
    
    bias_df <-
      bias_df %>% 
      filter_at(vars("solution_full", "solution_reduced"), all_vars(!is.na(.)))
    
    # Covariance between full and reduced EBVs for all animals
    cov <- cov(bias_df$solution_full, bias_df$solution_reduced)
    
    # f_val is average inbreeding for validation animals (those with phenotype excluded)
    # sigma2_u is estimated additive genetic variance
    sqrt((cov)/((1-f_val)*sigma2_u))
    
  } 