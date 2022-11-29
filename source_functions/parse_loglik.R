parse_loglik <- function(path, option = c("AIC", "logL")) {
  raw <-
    read_file(path) %>%
    str_split("\n") %>%
    map(~ str_squish(.x)) %>%
    tibble::tibble() %>%
    rename(c1 = 1) %>%
    unnest(c1) %>%
    filter(str_detect(c1, "AIC")) %>%
    pull(c1)
  
  
  AIC <-
    raw %>%
    str_extract("(?<=AIC = )[[:graph:]]+") %>%
    as.numeric(.)
  
  
  logL <-
    raw %>%
    str_extract("(?<=-2logL = )[[:graph:]]+(?= :)") %>%
    as.numeric(.)
  
  if (option == "AIC")
    return(AIC)
  
  if (option == "logL")
    return(logL)
  
}