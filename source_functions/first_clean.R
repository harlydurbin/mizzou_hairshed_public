
first_clean <- function(hs_id, ignore = NULL) {
  
  
  print(glue::glue("{hs_id}: start"))
  
  angus_reg <-
    read_excel(here::here("data/derived_data/import_join_clean/angus_reg.xlsx")) %>% 
    dplyr::select(farm_id, animal_id, an_reg)
  
  approved_cols <-
    c(
      "Lab_ID",
      "temp_id",
      "farm_id",
      "animal_id",
      "registration_number",
      "breed_code",
      "sex",
      "color",
      "date_score_recorded",
      "hair_score",
      "age",
      "calving_season",
      "toxic_fescue",
      "comment",
      "barcode",
      "year",
      "sold"
    )
  
  #### STEP ONE: RAW FILES ####
  # Creating a list of files
  # Requirements for this to work: semi-standardized column names
  raw <-
    combine(
      list.files(path = "~/Box Sync/HairShedding/ReportedData/2016",
                 full.names = TRUE,
                 pattern = "xlsx|xls"),
      list.files(path = "~/Box Sync/HairShedding/ReportedData/2017",
                 full.names = TRUE,
                 pattern = "xlsx|xls"),
      list.files(path = "~/Box Sync/HairShedding/ReportedData/2018",
                 full.names = TRUE,
                 pattern = "xlsx|xls"),
      list.files(path = "~/Box Sync/HairShedding/ReportedData/2019",
                 full.names = TRUE,
                 pattern = "xlsx|xls"),
      list.files(path = "~/Box Sync/HairShedding/ReportedData/2020",
                 full.names = TRUE,
                 pattern = "xlsx|xls")
    ) %>%
    # Only data for the specifed farm
    .[str_detect(., glue::glue("_{hs_id}_"))] %>%
    purrr::set_names(nm = (basename(.) %>%
                             tools::file_path_sans_ext()) %>% 
                       str_remove_all(., "DataRecording_")) %>%
    # Toss data if it's in the black listed set
    purrr::imap(~discard(.x, .y %in% ignore)) %>%
    purrr::compact() %>% 
    purrr::map(function(.x) {
      # Vector of column names
      (nms <- names(read_excel(.x)))
      # Assign column types based on column name
      (ct <-
          if_else(str_detect(nms, "date|Date"), "date", "text"))
      
      read_excel(.x, col_types = ct, trim_ws = TRUE)
    }) %>%
    # Remove blank rows and columns
    purrr::map(janitor::remove_empty, which = c("rows", "cols")) %>%
    # Standardize column names
    purrr::map(janitor::clean_names)
  
  print(glue::glue("{hs_id}: finished importing raw files"))
  
  
  
  #### STEP 2: FIRST CLEAN
  
  first_clean <-
    raw %>%
    # Extract year and farm ID from file name
    purrr::imap(~ dplyr::mutate(
      .x,
      year = str_extract(.y, "(?<=_)[[:digit:]]{4}"),
      year = as.numeric(year),
      farm_id = hs_id
    ))  %>%
    # Add columns if they don't exist
    purrr::map( ~ fncols(
      data = .x,
      cname = c("registration_number", "animal_id", "barcode")
    ) %>% 
      mutate_at(vars(c("registration_number", "animal_id", "barcode")), ~as.character(.))) %>% 
    purrr::map(~ dplyr::select(.x, -one_of("farm"))) %>%
    # Angus registration number fixes
    purrr::map(~ dplyr::left_join(.x, angus_reg) %>% 
                 dplyr::mutate(registration_number = if_else(!is.na(an_reg), an_reg, registration_number)) %>% 
                 dplyr::select(-an_reg)) %>% 
    # Rename sold_2017 and sold2018 etc to sold
    purrr::map(~ dplyr::rename_at(.x,
                                  vars(starts_with("sold")),
                                  #...by removing all numbers and punctuation from
                                  # column names where the column name contains "sold"
                                  funs(
                                    str_remove_all(., "[[:punct:]]|[[:digit:]]")
                                  ))) %>%
    # Remove sire registration, shearing, ge_epd if they exist
    purrr::map(~ dplyr::select(.x, -one_of(c("sire_registration", "shearing", "ge_epd")))) %>% 
    # At all columns that are characters, mutate to uppercase 
    # (i.e. to standardize Spring/spring/SPRING/sPrInG)
    purrr::map(~ dplyr::mutate_if(.x,
                                  is.character,
                                  .funs = ~ stringr::str_to_upper(.))) %>% 
    # Remove extraneous blank spaces
    purrr::map(~ dplyr::mutate_if(.x, is.character,
                                  .funs = ~ str_squish(.)))
  
  print(glue::glue("{hs_id}: finished first clean"))
  
  #### STEP 3: ID KEY
  
  key <- if (hs_id == "UMCT") {
    first_clean %>%
      # Use any_of in case one or more columns don't exist
      purrr::map(~ dplyr::select(.x, any_of(
        c("registration_number", "animal_id", "barcode")
      ))) %>%
      # Add columns if they don't exist
      purrr::map( ~ fncols(
        data = .x,
        cname = c("registration_number", "animal_id", "barcode")
      )) %>%
      purrr::map(~ mutate_all(.x, ~ as.character(.))) %>%
      # Remove all spaces and punctuation
      purrr::map(~ mutate_all(.x, ~ str_remove_all(., "[[:space:]]"))) %>%
      purrr::reduce(coalesce_join, by = c("animal_id")) %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        readr::read_csv(here::here("data/derived_data/import_join_clean/umct_id_key.csv")) %>%
          dplyr::mutate_at(vars(-one_of("Lab_ID")),  ~ as.character(.))
      ) %>% 
      dplyr::mutate(registration_number2 = if_else(str_detect(registration_number, "^6"),
                                                   glue::glue("BIR{registration_number}"),
                                                   registration_number)) %>% 
      # registration_number --> Reg
      id_search(
        source_col = registration_number,
        search_df = animal_table,
        search_col = Reg,
        key_col = Lab_ID
      ) %>%
      # registration_number2 --> Reg
      id_search(
        source_col = registration_number2,
        search_df = animal_table,
        search_col = Reg,
        key_col = Lab_ID
      ) %>%
      # registration_number2 --> breed_assoc_reg
      id_search(
        source_col = registration_number2,
        search_df = animal_table,
        search_col = breed_assoc_reg,
        key_col = Lab_ID
      ) %>%
      dplyr::select(-registration_number2) %>% 
      dplyr::mutate(temp_id = row_number())
    
    
  } else {
    first_clean %>%
      # Use any_of in case one or more columns don't exist
      purrr::map( ~ dplyr::select(.x, any_of(
        c("registration_number", "animal_id", "barcode", "farm_id", "breed_code")
      ))) %>%
      # Add columns if they don't exist
      purrr::map(~ fncols(
        data = .x,
        cname = c("registration_number", "animal_id", "barcode")
      )) %>%
      purrr::map( ~ mutate_all(.x, ~ as.character(.))) %>%
      # Remove all spaces and punctuation
      purrr::map( ~ mutate_all(.x, ~ str_remove_all(., "[[:space:]]|[[:punct:]]"))) %>%
      purrr::reduce(coalesce_join, by = c("animal_id")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        # Add Gelbvieh prefixes for Gelbvieh producers
        registration_number = case_when(
          farm_id %in% c("J5C", "NOF", "TPF", "YDG") &
            breed_code == "GEL" &
            !stringr::str_detect(registration_number, "^AMGV") ~ as.character(glue::glue("AMGV{registration_number}")),
          TRUE ~ registration_number
        ),
        registration_number2 = if_else(
          str_detect(registration_number, "^6"),
          glue::glue("BIR{registration_number}"),
          registration_number
        )
      ) %>% 
      # registration_number --> Reg
      id_search(
        source_col = registration_number,
        search_df = animal_table,
        search_col = Reg,
        key_col = Lab_ID
      ) %>%
      # barcode --> Reg
      id_search(
        source_col = barcode,
        search_df = animal_table,
        search_col = Reg,
        key_col = Lab_ID
      ) %>%
      # barcode --> Ref_ID2
      id_search(
        source_col = barcode,
        search_df = animal_table,
        search_col = Ref_ID2,
        key_col = Lab_ID
      ) %>%
      # registration_number --> Ref_ID2
      id_search(
        source_col = registration_number,
        search_df = animal_table,
        search_col = Ref_ID2,
        key_col = Lab_ID
      ) %>%
      # barcode --> Ref_ID3
      id_search(
        source_col = barcode,
        search_df = animal_table,
        search_col = Ref_ID3,
        key_col = Lab_ID
      ) %>%
      # registration_number --> Ref_ID3
      id_search(
        source_col = registration_number,
        search_df = animal_table,
        search_col = Ref_ID3,
        key_col = Lab_ID
      ) %>%
      # registration_number --> breed_assoc_reg
      id_search(
        source_col = registration_number,
        search_df = animal_table,
        search_col = breed_assoc_reg,
        key_col = Lab_ID
      ) %>%
      # registration_number2 --> Reg
      id_search(
        source_col = registration_number2,
        search_df = animal_table,
        search_col = Reg,
        key_col = Lab_ID
      ) %>%
      # registration_number2 --> breed_assoc_reg
      id_search(
        source_col = registration_number2,
        search_df = animal_table,
        search_col = breed_assoc_reg,
        key_col = Lab_ID
      ) %>%
      dplyr::left_join(animal_table %>% 
                         select(Lab_ID, Comment)) %>% 
      # If more than 1 Lab_ID match and one of them is from the RAAA/ASA dump 
      # or Ireland, use the older Lab ID
      dplyr::group_by(animal_id, registration_number, barcode) %>%
      dplyr::mutate(helper_col = 
               dplyr::case_when(
                 dplyr::n_distinct(Lab_ID) > 1 &
                   stringr::str_detect(Comment, "RAAA|ASA|Teagasc") ~ "DROP"
               )) %>% 
      dplyr::filter(is.na(helper_col)) %>% 
      # For remaining animals with more than one Lab ID match,
      # use the more recent Lab ID
      dplyr::filter(Lab_ID == max(Lab_ID) | is.na(Lab_ID)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-registration_number2, -helper_col, -Comment) %>% 
      dplyr::mutate(temp_id = row_number())
  }
  
  key %<>% 
    select(-one_of("farm_id", "breed_code"))
    
  print(glue::glue("{hs_id}: finished creating key")) 
  
  ## Key QC reporting
  
  e1 <-
    key %>% 
    dplyr::filter(is.na(Lab_ID)) %>% 
    dplyr::tally() %>% 
    dplyr::mutate(error = "Missing Lab ID")
  
  
  e2 <-
    key %>% 
    find_dup(registration_number) %>% 
    dplyr::tally() %>% 
    dplyr::mutate(error = "Duplicate registration number")
  
  
  e3 <-
    key %>% 
    find_dup(barcode) %>% 
    dplyr::tally() %>% 
    dplyr::mutate(error = "Duplicate barcode")
  
  e4 <-
    key %>% 
    find_dup(animal_id) %>% 
    dplyr::tally() %>% 
    dplyr::mutate(error = "Duplicate animal ID")
  
  e5 <-
    key %>% 
    find_dup(Lab_ID) %>% 
    dplyr::tally() %>% 
    dplyr::mutate(error = "Duplicate Lab ID")
  
  e6 <-
    dplyr::bind_rows(e1, e2, e3, e4, e5) %>% 
    dplyr::mutate(farm = hs_id) %>% 
    dplyr::select(farm, error, n)
  
  print(e6)
  
  #### STEP 4: REDUCE ####
  
  reduced <-
    first_clean %>%
    # Join up temp ID
    purrr::map(
      ~ id_search(
        source_df = .x,
        source_col = registration_number,
        search_df = key,
        search_col = registration_number,
        key_col = temp_id
      )
    ) %>%
    purrr::map(
      ~ id_search(
        source_df = .x,
        source_col = barcode,
        search_df = key,
        search_col = barcode,
        key_col = temp_id
      )
    ) %>%
    purrr::map(
      ~ id_search(
        source_df = .x,
        source_col = animal_id,
        search_df = key,
        search_col = animal_id,
        key_col = temp_id
      )
    ) %>%
    purrr::map(~ dplyr::select(.x, -any_of(
      c("registration_number", "animal_id", "barcode")
    ))) %>%
    purrr::reduce(bind_rows) %>% 
    dplyr::left_join(key) %>% 
    dplyr::select(any_of(approved_cols)) %>% 
    # Need everything to have a temp ID
    assertr::verify(!is.na(temp_id))
  
  print(glue::glue("{hs_id}: finished reducing rows"))
  
  return(reduced)
  
}