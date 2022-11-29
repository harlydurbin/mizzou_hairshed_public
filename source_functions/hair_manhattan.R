# Prep gwas output for plotting
# Stolen from Troy

chr_len <- function(df) {
  df %>%
    # Summarise each chromosome length
    group_by(chr) %>%
    summarise(chr_len = max(pos)) %>%
    # Total relative to entire genome
    mutate(tot = cumsum(chr_len) - chr_len) %>%
    select(-chr_len) %>%
    left_join(df, by = c("chr" = "chr")) %>%
    arrange(chr, pos) %>%
    mutate(BPcum = pos + tot) %>%
    ungroup()
}


hair_manhattan <-
  function(df,
           y_var,
           y_lab = NULL,
           plot_title = NULL,
           facet = FALSE,
           nfacets = 2,
           desc = NULL,
           sigline = NULL,
           highlight = FALSE,
           highlight_col = NULL,
           color1 = NULL,
           color2 = NULL) {
    y_var <- rlang::enquo(y_var)
    
    desc <- rlang::enquo(desc)
    
    highlight_col <- rlang::enquo(highlight_col)
    
    axisdf <-
      df %>%
      # Add chromosome length for plotting
      chr_len() %>%
      group_by(chr) %>%
      summarize(center = (max(BPcum) + min(BPcum)) / 2)
    
    
    if(!is.null(color1)) {
    df <-
      df %>%
      chr_len() %>%
      # Alternating chromosome color
      mutate(chrcolor = case_when(chr %in% c(seq(from = 1,
                                                 to = 29,
                                                 by = 2)) ~ color1,
                                  chr %in% c(seq(from = 2,
                                                 to = 29,
                                                 by = 2)) ~ color2))
    } else if(is.null(color1)){
      df %>%
        chr_len() %>%
        # Alternating chromosome color
        mutate(chrcolor = case_when(chr %in% c(seq(from = 1,
                                                   to = 29,
                                                   by = 2)) ~ "#C2D9CD",
                                    chr %in% c(seq(from = 2,
                                                   to = 29,
                                                   by = 2)) ~ "#538797"))
    }
    
    if(facet == TRUE){
      
      df <-
        df %>% 
        rename(facetvar := !!desc)
      
    }
    
    
    gg <- 
      df %>%
      ggplot(aes(x = BPcum,
                 y = !!y_var)) +
      geom_point(aes(color = chrcolor),
                 alpha = 0.75) +
      scale_color_identity()
    
    gg <- 
      if(highlight == TRUE) {
        gg +
          geom_point(data = df %>% 
                       filter(!!highlight_col == TRUE),
                       aes(x = BPcum,
                           y = !!y_var),
                     color = "black",
                     fill = "black")
        } else {
          gg
          }
    
    gg <-
      gg +
      # scale_color_manual(values = rep(colors, 29), guide = "none") +
      # Every 3 chromosomes gets a label
      scale_x_continuous(label = axisdf$chr[c(TRUE, FALSE)],
                         breaks = axisdf$center[c(TRUE, FALSE)]) +
      theme_classic() +
      theme(plot.title = element_text(size = 24,
                                      face = "italic",
                                      margin = margin(t = 0,
                                                      r = 0,
                                                      b = 13,
                                                      l = 0)),
            plot.subtitle = element_text(size = 20,
                                         face = "italic",
                                         margin = margin(t = 0,
                                                         r = 0,
                                                         b = 13,
                                                         l = 0)),
            axis.title = element_text(size = 18),
            axis.title.y = element_text(margin = margin(t = 0,
                                                        r = 13,
                                                        b = 0,
                                                        l = 0)),
            axis.title.x = element_text(margin = margin(t = 13,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0)),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 18)) +
      labs(x = NULL,
           y = y_lab,
           title = plot_title)
      
    gg <-
      if (facet == TRUE) {
        gg +
          facet_wrap(~ facetvar,
                     nrow = nfacets,
                     scales = "free_x")
      } else {
        gg
      }
    
    gg <-
      if (!is.null(sigline)) {
        gg +
          geom_hline(yintercept = sigline,
                     color = "red",
                     size = 0.25)
        } else {
          gg
          }
    
    return(gg)
  }
