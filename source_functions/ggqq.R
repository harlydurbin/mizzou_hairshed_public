# Stolen from Troy

ggqq <- function(pvector){
  pvector = pvector[!is.na(pvector) & !is.nan(pvector) & !is.null(pvector) & is.finite(pvector) & pvector<1 & pvector>0]
  pdf = data.frame(observed = -log10(sort(pvector, decreasing = FALSE)),
                   expected = -log10(ppoints(length(pvector))))
  
  upper_limit = max(max(pdf$observed),
                    max(pdf$expected))
  
  qqplotted <-
    ggplot2::ggplot(pdf,
                    ggplot2::aes(expected, observed)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0,
                         slope = 1,
                         colour = "red") +
    ggplot2::scale_x_continuous(limits = c(0, upper_limit)) +
    ggplot2::scale_y_continuous(limits = c(0, upper_limit)) +
    ggplot2::labs(x = expression(paste("Observed ", -log10, "(", italic('p'), ")")),
                  y = expression(paste("Expected ", -log10, "(", italic('p'), ")")))
  
  return(qqplotted)
}