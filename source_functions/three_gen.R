
# Pull out 3 generation pedigree

three_gen <-
  function(df, full_ped) {
    
    sires_list <-
      df %>%
      dplyr::select(full_reg = sire_reg) %>%
      dplyr::distinct() %>%
      dplyr::pull(full_reg)
    
    # Pull out paternal grandparents
    sires <- 
      full_ped[full_ped$full_reg %in% sires_list, ]
    
    dams_list <-
      df %>%
      dplyr::select(full_reg = dam_reg) %>%
      dplyr::distinct() %>%
      dplyr::pull(full_reg)
    
    # Pull out maternal grandparents
    dams <- 
      full_ped[full_ped$full_reg %in% dams_list, ]    
    
    
    dplyr::bind_rows(df, sires, dams) %>%
      dplyr::select(full_reg, sire_reg, dam_reg) %>% 
      dplyr::distinct() 
  } 