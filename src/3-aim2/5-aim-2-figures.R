
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 
d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data.rds"))

temp<-adj_RR%>%filter(Y=="diar7d", target=="Any MST")
table(temp$sample_cat)

#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

#unadj_RR<-unadj_RR %>% filter(!is.na(se))
sample_cats = levels(unadj_RR$sample_cat)[levels(unadj_RR$sample_cat)!="Any sample"]


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------

# legend_labels=sample_cats
# mydf <- unadj_RR %>% 
#   filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", coef!=1)
# 
# unique(unadj_RR$study)
# mydf <- unadj_RR %>% 
#   filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", study=="Fuhrmeister 2020")
# d <- d %>% 
#   filter(target %in% c("Any pathogen","Any MST"),  study=="Fuhrmeister 2020")
# table(d$pos, d$diar7d, d$sample, d$target)
# 
# mydf <- unadj_RR %>% 
#   filter(Y=="diar7d", coef!=1)
# 
mydf <- unadj_RR %>% 
  filter(target %in% c("Any MST"), Y=="diar7d") %>% 
  filter(study=="Odagiri 2016", target=="Any MST")


base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hands"  = my_colors[7],
               "Mother's hands" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               # "Flies in kitchen" = my_colors[9],
               # "Flies in latrine" = my_colors[10],
               "Flies" = my_colors[9],
               "Sparse data" = "grey50")
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
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
    labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=11, face = "bold"),
          strip.text.y = element_text(size=11, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}


base_plot_diff <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hands"  = my_colors[7],
               "Mother's hands" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               # "Flies in kitchen" = my_colors[9],
               # "Flies in latrine" = my_colors[10],
               "Flies" = my_colors[9],
               "Sparse data" = "grey50")
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  mydf <- mydf %>% droplevels(.)
  
  ggplot(data = mydf, (aes(x=study, y=coef, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
    geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE) +
    scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
    coord_flip()+
    labs(color="Sample type") + xlab("") + ylab("Mean difference") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=11, face = "bold"),
          strip.text.y = element_text(size=11, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}



#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
  
# d <- unadj_RR %>% 
#   filter(target %in% c("Any pathogen","Any MST")) %>% filter(study=="Holcomb 2020")

#Primary figure
p_diar_1_unadj <- unadj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)


#Sprcific pathogens
p_diar_adj_path <- adj_RR %>% 
  filter( Y=="diar7d", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  base_plot(drop_full_sparse=T)
p_diar_adj_path

p_haz_adj_path <- adj_RR %>% 
  filter( Y=="haz", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  base_plot_diff(drop_full_sparse=T)
p_haz_adj_path



p_stunt_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="stunt") %>%
  base_plot(drop_full_sparse=T)


p_wast_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="wast") %>%
  base_plot(drop_full_sparse=T)


p_underwt_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="underwt") %>%
  base_plot(drop_full_sparse=T)




p_diar_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)


p_diar_2_adj <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)


p_diar_s1_adj <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)


p_haz_1 <- unadj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)


p_haz_2 <- unadj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)


p_haz_s1 <- unadj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)



p_haz_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)


p_haz_2_adj <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)


p_haz_s1_adj <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)




p_waz_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="waz") %>%
  base_plot_diff(drop_full_sparse=T)


p_waz_2_adj <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="waz") %>%
  base_plot_diff(drop_full_sparse=T)


p_waz_s1_adj <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="waz") %>%
  base_plot_diff(drop_full_sparse=T)



p_whz_1 <- unadj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="whz") %>%
  base_plot_diff(drop_full_sparse=T)


p_whz_2 <- unadj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="whz") %>%
  base_plot_diff(drop_full_sparse=T)


p_whz_s1 <- unadj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="whz") %>%
  base_plot_diff(drop_full_sparse=T)


p_whz_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="whz") %>%
  base_plot_diff(drop_full_sparse=T)


p_whz_2_adj <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="whz") %>%
  base_plot_diff(drop_full_sparse=T)


p_whz_s1_adj <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="whz") %>%
  base_plot_diff(drop_full_sparse=T)



#save figures
save(list=ls(pattern="p_"), file=here("figures/aim2_figures.Rdata"))
ls(pattern="p_")



