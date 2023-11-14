
rm(list=ls())
source(here::here("0-config.R"))

adj_RR_tmle <- readRDS(file=here("results/tmle_aim2_res.Rds")) %>% mutate(analysis="TMLE")
adj_RR_tmle$study <- factor(adj_RR_tmle$study)
levels(adj_RR_tmle$study)[levels(adj_RR_tmle$study) == "Capone 2022 in prep"] <- "Capone 2022"

adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) %>% mutate(analysis="GLM") 
adj_RR$sample_cat_f[is.na(adj_RR$sample_cat_f)] <- "Any sample"

adj_RR <- bind_rows(adj_RR, adj_RR_tmle)
sample_cats = levels(adj_RR$sample_type)[levels(adj_RR$sample_type)!="Any sample"]
adj_RR <- adj_RR %>%
  mutate(analysis = factor(analysis, levels=c("TMLE","GLM"))) %>% 
  filter(!is.na(coef), !is.infinite(coef)) %>%
  group_by(Y, target, sample, study) %>%
  mutate(N=n()) %>% filter(N==2)

adj_RR %>% filter(sample=="any sample type", study=="Odagiri 2016", target=="Any MST")


unique(adj_RR_tmle$study)
unique(adj_RR$study)


class(adj_RR$sample_cat)
adj_RR <- clean_res_subgroup(adj_RR)

adj_RR$study <- factor(adj_RR$study, levels = rev(c(
  "Fuhrmeister 2020", "Boehm 2016","Kwong 2021" ,       
  "Steinbaum 2019","Holcomb 2021","Capone 2021",
  "Capone 2022", "Reese 2017","Odagiri 2016",
  "Pooled")))


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------

mydf <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") 
legend_labels=sample_cats
drop_full_sparse=F

base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother's hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies" = my_colors[9],
               "Sparse data" = "grey50")
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% droplevels(.)
  #ggplot(data = mydf, (aes(x=study, y=RR, group=intersect(sample_cat, analysis), color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  ggplot(data = mydf, (aes(x=study, y=RR, group=analysis, color=sample_cat_f, shape=analysis))) + 
    geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE, guide=FALSE) +
    scale_shape_manual(values=c(16, 17)) + 
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    scale_y_continuous(
      # breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
      # trans='log10', 
      # labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
      breaks=c(0.125,.25, .5,1, 2, 4, 8), 
      trans='log10', 
      labels = c("1/8","1/4", "1/2","1", "2", "4", "8")
    ) + coord_flip() +
    labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=10, face = "bold"),
          strip.text.y = element_text(size=10, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}


base_plot_diff <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother's hand rinse" = my_colors[8],
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
  
  ggplot(data = mydf, (aes(x=study, y=coef, group=analysis, color=sample_cat_f, shape=analysis))) + 
    geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE, guide=FALSE) +
    scale_shape_manual(values=c(16, 17)) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    coord_flip()+
    labs(color="Sample type") + xlab("") + ylab("Mean difference") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=10, face = "bold"),
          strip.text.y = element_text(size=10, angle = 270, face = "bold"),          
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}



#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------

#Primary figure
p_diar_1_adj_tmle <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)
p_diar_1_adj_tmle

p_haz_1_adj_tmle <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
#p_haz_1_adj_tmle




#save figures
save(list=ls(pattern="p_"), file=here("figures/aim2_tmle_figures.Rdata"))
ls(pattern="p_")