
# u = genetic variance 
# se is approximated standard error (last column of solution file)
# f is inbreeding coefficient 
calculate_acc <- function(u, se, f, option = "reliability") {
  PEV <- (se ^ 2)
  
  acc <- if (option == "reliability") {
    1 - (PEV / ((1+f)*u))
  } else if(option == "bif"){
    
    1 - sqrt((PEV / ((1+f)*u)))
    
  }
  
  return(acc)
}

# Could apply the function to make a new df column called "reliabilty" using the 
# code below where "se_column" is your column containing standard errors and 
# "f_column" is your column containing inbreeding coefficients
#
# df %>% 
#   dplyr::mutate(reliability = purrr::map2_dbl(.x = se_column,
#                                               .y = f_column,
#                                               ~ calculate_acc(u = gen_var,
#                                                               se = .x,
#                                                               f = .y,
#                                                               option = "reliability")))