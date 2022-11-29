lrt_calc <- 
  
  function(ll_null, ll_test) {
    
    pval <- pchisq(ll_null - ll_test, 1, lower.tail = FALSE, log.p = TRUE)/2
    
    -log10(exp(pval))
    
  }