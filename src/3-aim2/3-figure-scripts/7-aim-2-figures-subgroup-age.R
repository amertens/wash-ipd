


rm(list=ls())
source(here::here("0-config.R"))
library(scales)
 
adj_RR <- readRDS(file=here("results/adjusted_aim2_res_subgroup_age.Rds")) 


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

adj_RR$agecat[is.na(adj_RR$agecat)] <- adj_RR$agecat_anthro [is.na(adj_RR$agecat)]
adj_RR <- adj_RR %>% filter(!is.na(agecat)) %>%
  rename(Vlevel = agecat)

#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------
#adj_RR <- adj_RR %>% filter(!is.na(coef))
# adj_RR$sparse <- ifelse(is.na(adj_RR$coef),"yes",adj_RR$coef)
# adj_RR$RR[adj_RR$sparse=="yes"] <- 1
adj_RR <- clean_res_subgroup(adj_RR)
# adj_RR <- adj_RR %>% mutate(
#   int.p =case_when(
#     Vlevel==0 ~ NA_character_,
#     int.p<0.001~"***",
#     int.p<0.01~"**",
#     int.p<0.05~"*",
#     int.p>=0.05~""
#   ),
#   Vlevel = factor(case_when(
#     sparse=="yes" ~ "Sparse data",
#     V=="wet" & Vlevel==0 ~ "Dry season",
#     V=="wet" & Vlevel==1 ~ "Wet season",
#     V=="wet_CH" & Vlevel==0 ~ "Dry season",
#     V=="wet_CH" & Vlevel==1 ~ "Wet season",
#     V=="animals" & Vlevel==0 ~ "No animals",
#     V=="animals" & Vlevel==1 ~ "Animals in\ncompound"
#   ), levels = c("Dry season", "Wet season","No animals", "Animals in\ncompound","Sparse data"))
# )



#see if any levels are missing
adj_RR$target[is.na(adj_RR$target_f)]
sample_cats = levels(adj_RR$sample_cat)[levels(adj_RR$sample_cat)!="Any sample"]



#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------

mydf <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d")
drop_full_sparse=T
ylimits=c(0.125,8)

base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, ylimits=c(0.25,5)){
  
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
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(sample_cat) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% droplevels(.)
  
  Y_breaks=c(.25, .5,1, 2, 4, 8)
  Y_breaks2=c("1/4", "1/2","1", "2", "4", "8")
  
  ggplot(data = mydf, (aes(x=study, y=RR, group=Vlevel, color=Vlevel, shape=Vlevel))) + 
    geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    #Mark significant interactions
    #geom_text(aes(y=ci.ub, label=int.p), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
    scale_color_manual(#breaks = legend_labels,
      values = c(cbbPalette[2:5],"grey50"), drop = FALSE) +
    scale_shape_manual(values=c(16, 16, 16,18), guide=FALSE)+  
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
      breaks=Y_breaks, 
      trans='log10', 
      labels = Y_breaks2
    ) + 
    coord_flip(ylim=ylimits)+
    labs(color="Subgroup") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
  
}



base_plot_diff <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, ylimits=c(-1,1)){
  
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
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(sample_cat) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% droplevels(.)
  
  
  ggplot(data = mydf, (aes(x=study, y=coef, group=Vlevel, color=Vlevel, shape=Vlevel))) + 
    geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    #Mark significant interactions
    #geom_text(aes(y=ci.ub, label=int.p), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
    scale_color_manual(#breaks = legend_labels,
      values = c(cbbPalette[2:5],"grey50"), drop = FALSE) +
    scale_shape_manual(values=c(16, 16, 16,18), guide=FALSE)+  
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    # scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
    #   breaks=Y_breaks, 
    #   #trans='log10', 
    #   labels = Y_breaks2
    # ) + 
    coord_flip(ylim=ylimits)+
    labs(color="Subgroup") + xlab("") + ylab("Mean Z-score difference") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
  
}

table(adj_RR$Y)

#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
p_age_diar_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T, ylimits=c(0.125,8))
ggsave(p_age_diar_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_age_diar.png"), width = 10, height = 6)

p_age_haz_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T, ylimits=c(-4,4))
ggsave(p_age_haz_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_age_haz.png"), width = 10, height = 6)

#save figures
save(list=ls(pattern="p_"), file=here("figures/aim2_subgroup_figures_age.Rdata"))



