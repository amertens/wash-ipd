
rm(list=ls())
source(here::here("0-config.R"))

unadj_diff <- readRDS(file=here("results/unadjusted_aim1_diff.Rds")) %>% mutate(sparse="no") %>% filter(sample!="FP")
adj_diff <- readRDS(file=here("results/adjusted_aim1_diff.Rds"))   %>% mutate(sparse="no")  %>% filter(sample!="FP")

#---------------------------------------------------------------
#function to clean results/order factors
#---------------------------------------------------------------

target_lev=target_levels
sample_cats = levels(unadj_diff$sample_cat)[levels(unadj_diff$sample_cat)!="Any sample"]


#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

#unadj_RR <- clean_res(unadj_RR)
unadj_diff <- clean_res(unadj_diff)
adj_diff <- clean_res(adj_diff)


#see if any levels are missing
sample_cats = levels(unadj_diff$sample_cat)[levels(unadj_diff$sample_cat)!="Any sample"]


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------

base_plot_diff <- function(mydf, legend_labels=sample_cats){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hands"  = my_colors[7],
               "Mother's hands" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies in kitchen" = my_colors[9],
               "Flies in latrine" = my_colors[10],
               "Sparse data" = "grey50")
  
  mydf <- mydf %>% droplevels(.)
  
  ggplot(data = mydf, (aes(x=study, y=coef, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
    geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE) +
    scale_shape_manual(values=c(16, 13,9), guide=FALSE) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
    coord_flip()+
    labs(color="Sample type") + xlab("") + ylab("Mean difference (Log10 transformed)") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=11, face = "bold"),
          strip.text.y = element_text(size=11, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}

base_plot <- function(mydf, legend_labels=sample_cats){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hands"  = my_colors[7],
               "Mother's hands" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies in kitchen" = my_colors[9],
               "Flies in latrine" = my_colors[10],
               "Sparse data" = "grey50")
  
  mydf <- mydf %>% droplevels(.)
  
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
    geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE) +
    scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
    scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN)+ coord_flip()+
    labs(color="Sample type") + xlab("") + ylab("Count difference") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=11, face = "bold"),
          strip.text.y = element_text(size=11, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}



# -	Fig S5. Abundance of specific pathogens
p_s5 <- adj_diff %>% 
  filter(target %in% any_pathogens, model=="linear") %>%
  droplevels(.) %>%
  #mutate(study=paste0(study,"-",round)) %>%
  base_plot_diff
p_s5


p_s5_sth <- adj_diff %>% 
  filter(target %in% any_pathogens, model=="neg. binomial") %>%
  droplevels(.) %>%
  #mutate(study=paste0(study,"-",round)) %>%
  base_plot
p_s5_sth

# -	Fig S6. Abundance of specific MST markers 

p_s6 <- adj_diff %>% 
  filter(target %in% any_MST, model=="linear") %>%
  droplevels(.) %>%
  #mutate(study=paste0(study,"-",round)) %>%
  base_plot_diff
p_s6





#save figures
save(list=ls(pattern="p_"), file=here("figures/aim1_abundance_figures.Rdata"))

