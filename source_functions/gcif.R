

gcif <- 
  function(df, p_col = p_wald, adjust_p = FALSE){
    
    p_col <- rlang::enquo(p_col)
    
    df <- 
      df %>% 
      rename(p = !!p_col)
    
    # Median of genome-wide Chi square tests
    # p = vector of probabilities
    gci <- median(qchisq(p = df$p, 
                         # degrees of freedom
                         df = 1,
                         # Probabilities are P[X > x]
                         lower.tail = FALSE))/qchisq(0.5, 1)
    
    if(adjust_p == TRUE){
      new_p <- 
        # q = vector of quantiles
        pchisq(q = qchisq(p = df$p,
                          # degrees of freedom
                          df = 1,
                          # Probabilities are P[X > x]
                          # Divide by median of genome wide Chi square tests calculated above
                          lower.tail = FALSE)/gci,
               df = 1,
               lower.tail = FALSE)
      
      return(new_p)
    } else {
      return(gci)
    }
  }
