

theme_ki <- function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      axis.text.x=element_text(size=7),
      axis.text.y=element_text(size=7),
      legend.text=element_text(size=7),
      axis.title = element_text(size = 10),
      strip.text.x = element_text(size=9, face = "bold"),
      strip.text.y = element_text(size=9, angle = 270, face = "bold"),          
      plot.title = element_text(hjust = 0.5, face = "plain", size=9)
    )
}

#hbgdki pallets
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

theme_set(theme_ki())



scaleFUN <- function(x) sprintf("%.2f", x)




heatmap_plot <- function(d, colours, textcol= "grey20"){
  p <- ggplot(d, aes(x = Y, y = X, fill = pval_cat )) + 
    geom_tile(colour = "grey80", size = 0.25) + scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0), 
                     breaks=levels(d$X),
                     labels=lab_f) + 
    #scale_y_discrete(expand = c(0, 0)) + 
    theme_minimal(base_size = 8) +
    facet_grid(target~., scales = "free", space="free") +
    scale_fill_manual(values = colours, drop = FALSE) +
    scale_color_manual(values = c("grey20", "black"), drop = FALSE) +
    geom_text(aes(label = est, color=factor(pooled)), size=2.25) +
    theme(legend.title = element_text(color = textcol,
                                      size = 8), legend.margin = margin(grid::unit(0.1, "cm")), legend.text = element_text(colour = textcol,
                                                                                                                           size = 7, face = "bold"), legend.key.height = grid::unit(0.2,   "cm"), legend.key.width = grid::unit(1, "cm"),
          legend.position = "bottom", axis.text.x = element_text(size = 8,  colour = textcol), 
          axis.text.y = element_text(size = 8, vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4),
          plot.title = element_text(colour = textcol, hjust = 0, size = 12, face = "bold"), strip.text.x = element_text(size = 10),
          strip.text.y = element_text(angle = 0, size = 10),
          plot.background = element_blank(), panel.border = element_blank(),
          strip.background = element_blank(), panel.background = element_rect(fill = "grey80", colour = "grey80"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.spacing = unit(panel_spacing, "lines")) +
    guides(fill = guide_legend("P-value strength", nrow = 2)) + labs(x = "", y = "", title = "")
  
  return(p)
}


