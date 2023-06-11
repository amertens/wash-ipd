


rm(list=ls())
source(here::here("0-config.R"))
library(scales)

#adj_zoo <- readRDS(file=here("results/adjusted_aim2_RR_pooled.Rds")) %>% filter(target %in% c("Any zoonotic","Any non-zoonotic"))
#adj_RR <- readRDS(file=here("results/adjusted_aim2_emm.Rds")) 
adj_RR <- readRDS(file=here("results/subgroup_aim2_pooled.Rds")) 
adj_PD <- readRDS(file=here("results/subgroup_PD_aim2_pooled.Rds"))  #Need to add individual estimates



Ns <- adj_RR %>% group_by(study, sample, target, V) %>% summarise(N=n())
table(Ns$N)
Ns[Ns$N>2,]
Ns[Ns$N==1,]
adj_RR


adj_RR$RR[is.na(adj_RR$RR)] <- NA
target_lev=target_levels


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------
#adj_RR <- adj_RR %>% filter(!is.na(coef))
# adj_RR$sparse <- ifelse(is.na(adj_RR$coef),"yes",adj_RR$coef)
# adj_RR$RR[adj_RR$sparse=="yes"] <- 1
adj_RR <- clean_res_subgroup(adj_RR)
adj_PD <- clean_res_subgroup(adj_PD)

adj_RR <- adj_RR %>% mutate(
  int.p =case_when(
    int.p<0.001~"***",
    int.p<0.01~"**",
    int.p<0.05~"*",
    int.p>=0.05~""
  ),
  Vlevel = factor(case_when(
    sparse=="yes" ~ "Sparse data",
    V=="sex" & Vlevel==0 ~ "Female",
    V=="sex" & Vlevel==1 ~ "Male",
    V=="wet" & Vlevel==0 ~ "Dry season",
    V=="wet" & Vlevel==1 ~ "Wet season",
    V=="wet_CH" & Vlevel==0 ~ "Dry season",
    V=="wet_CH" & Vlevel==1 ~ "Wet season",
    V=="animals" & Vlevel==0 ~ "No animals",
    V=="animals" & Vlevel==1 ~ "Animals in\ncompound"
  ), levels = c("Male","Female","Dry season", "Wet season","No animals", "Animals in\ncompound","Sparse data"))
)

table(adj_RR$int.p)
table(adj_RR$Vlevel, adj_RR$int.p)
table(adj_RR$Vlevel)

#see if any levels are missing
adj_RR$target[is.na(adj_RR$target_f)]
sample_cats = levels(adj_RR$sample_cat)[levels(adj_RR$sample_cat)!="Any sample"]




adj_PD <- adj_PD %>% mutate(
  int.p =case_when(
    int.p<0.001~"***",
    int.p<0.01~"**",
    int.p<0.05~"*",
    int.p>=0.05~""
  ),
  Vlevel = factor(case_when(
    sparse=="yes" ~ "Sparse data",
    V=="sex" & Vlevel==0 ~ "Female",
    V=="sex" & Vlevel==1 ~ "Male",
    V=="wet" & Vlevel==0 ~ "Dry season",
    V=="wet" & Vlevel==1 ~ "Wet season",
    V=="wet_CH" & Vlevel==0 ~ "Dry season",
    V=="wet_CH" & Vlevel==1 ~ "Wet season",
    V=="animals" & Vlevel==0 ~ "No animals",
    V=="animals" & Vlevel==1 ~ "Animals in\ncompound"
  ), levels = c("Male","Female","Dry season", "Wet season","No animals", "Animals in\ncompound","Sparse data"))
)


adj_RR <- adj_RR %>% group_by(study, target, sample, Y, V) %>% arrange(Vlevel) %>% mutate(int.p=ifelse(Vlevel==last(Vlevel),int.p,NA))
adj_PD <- adj_PD %>% group_by(study, target, sample, Y, V) %>% arrange(Vlevel) %>% mutate(int.p=ifelse(Vlevel==last(Vlevel),int.p,NA))

#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------

mydf <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), V=="wet")
drop_full_sparse=T
legend_labels=sample_cats


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
    geom_point(size=2, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    #Mark significant interactions
    geom_text(aes(y=ci.ub, label=int.p), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
    scale_color_manual(#breaks = legend_labels,
      values = c(cbbPalette[2:3],"grey50"), drop = FALSE) +
    scale_shape_manual(values=c(16, 16,18), guide=FALSE)+  
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


legend_labels=sample_cats
drop_full_sparse=F
ylimits=c(-1,1)
p_hjust=-0.5
ylab="Mean Z-score difference"

base_plot_diff <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, ylimits=c(-1,1), p_hjust=-0.5, ylab="Mean Z-score difference"){
  
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
      #filter(n()!=sum(sparse=="yes")) %>% ungroup()
      filter(n()!=sum(is.na(se))) %>% ungroup()
    }
  
  mydf <- mydf %>% droplevels(.)
  
  # Y_breaks=c(.25, .5,1, 2, 4, 8)
  # Y_breaks2=c("1/4", "1/2","1", "2", "4", "8")
  
  ggplot(data = mydf, (aes(x=study, y=coef, group=Vlevel, color=Vlevel, shape=Vlevel))) + 
    geom_point(size=2, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    #Mark significant interactions
    geom_text(aes(y=coef, label=int.p), color="black", position = position_dodge(0.5), hjust = p_hjust, size=4) +
    scale_color_manual(#breaks = legend_labels,
      values = c(cbbPalette[2:3],"grey50"), drop = FALSE) +
    scale_shape_manual(values=c(16, 16,18), guide=FALSE)+  
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    # scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
    #   breaks=Y_breaks, 
    #   labels = Y_breaks2
    # ) + 
    coord_flip(ylim=ylimits)+
    labs(color="Subgroup") + xlab("") + ylab(ylab) + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
  
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
# p_wet_diar_1 <- adj_RR %>% 
#   filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", V=="wet") %>%
#   base_plot(drop_full_sparse=T, ylimits=c(0.125,8))
# ggsave(p_wet_diar_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_wet_diar.png"), width = 10, height = 6)


p_wet_diar_2 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", V=="wet_CH") %>%
  base_plot(drop_full_sparse=T, ylimits=c(0.125,8))
ggsave(p_wet_diar_2, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_wet_CH_diar.png"), width = 10, height = 6)

#Prevalence difference:
p_wet_diar_2_PD <- adj_PD %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", V=="wet_CH") %>%
  base_plot_diff(drop_full_sparse=T, ylimits=c(-.55,.3), ylab="Prevalence difference", p_hjust=-.75)

p_animals_diar_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", V=="animals") %>%
  base_plot(drop_full_sparse=T, ylimits=c(0.125,8))
ggsave(p_animals_diar_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_animal_diar.png"), width = 10, height = 6)

p_animals_diar_1_PD <- adj_PD %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", V=="animals") %>%
  base_plot_diff(drop_full_sparse=T, ylimits=c(-.5,.5), ylab="Prevalence difference", p_hjust=-.75)

p_wet_haz_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz", V=="wet") %>%
  base_plot_diff(drop_full_sparse=T, ylimits=c(-0.89,1.2))
ggsave(p_wet_haz_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_wet_haz.png"), width = 10, height = 6)


p_animals_haz_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz", V=="animals") %>%
  base_plot_diff(drop_full_sparse=T, ylimits=c(-1.1,0.8))
ggsave(p_animals_haz_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_animals_haz.png"), width = 10, height = 6)


p_sex_diar_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", V=="sex") %>%
  base_plot(drop_full_sparse=T, ylimits=c(0.125,8))
ggsave(p_sex_diar_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_sex_diar.png"), width = 10, height = 6)

p_sex_diar_1_PD <- adj_PD %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", V=="sex") %>%
  base_plot_diff(drop_full_sparse=T, ylimits=c(-.4,.55), ylab="Prevalence difference", p_hjust=-.75)

p_sex_haz_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz", V=="sex") %>%
  base_plot_diff(drop_full_sparse=T, ylimits=c(-1.5,1.5))
ggsave(p_sex_haz_1, file = paste0(here::here(),"/figures/pngs/subgroup_aim2_p_sex_haz.png"), width = 10, height = 6)




#save figures
save(list=ls(pattern="p_"), file=here("figures/aim2_subgroup_figures.Rdata"))


adj_RR %>% filter(target %in% c("Any pathogen"), sample=="any sample type", Y=="haz", V=="sex", study=="Pooled")
adj_PD %>% filter(target %in% c("Any pathogen"), Y=="diar7d", V=="wet_CH", study=="Pooled")
