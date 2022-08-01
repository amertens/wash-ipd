
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 

temp<-adj_RR%>%filter(Y=="diar7d", target=="Any MST")
table(temp$sample_cat)

unique(adj_RR$target)

#count number of covariates
unadj_RR$N_W <- ""
adj_RR$N_W <- str_count(adj_RR$W,",") + 1
adj_RR$N_W[adj_RR$W=="unadjusted"] <- 0

temp <- adj_RR %>% filter(study=="Holcomb 2021", target=="Any MST", !is.na(coef), Y %in% c("diar7d","haz"))


table(adj_RR$minN)
table(adj_RR$a)
table(adj_RR$c)
table(adj_RR$a+adj_RR$c)

summary(adj_RR$RR)
adj_RR$ci.lb[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- NA
adj_RR$ci.ub[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- NA
adj_RR$sparse[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- "yes"
adj_RR$sample_cat[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- "Sparse data"
adj_RR$RR[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- 1

adj_RR <- adj_RR %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)


unadj_RR <- unadj_RR %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)

res <- adj_RR %>% filter(target %in% c("Any MST"), 
                         Y=="diar7d", study=="Holcomb 2020",
                         sample=="any sample type")
res$N_W
res$W


#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

sample_cats = levels(unadj_RR$sample_cat)[levels(unadj_RR$sample_cat)!="Any sample"]


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------



base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, facet_lab_size=10){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
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
  
  minCI <- min(mydf$ci.lb, na.rm=T)-0.001
  maxCI <- max(mydf$ci.ub, na.rm=T) +0.001
  axislims = c(minCI, maxCI)
  axislims[1]=ifelse(axislims[1] < 1/20, (1/20)-0.001, axislims[1])
  axislims[2]=ifelse(axislims[2] > 20, 20.001, axislims[2])
  axisindex= 1:9
  axisbreaks = c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16)
  axisindex = axisindex[axisbreaks > axislims[1] & axisbreaks < axislims[2]]
  axisbreaks = axisbreaks[axisindex]
  axislabels =c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")[axisindex]
  
  mydf <- mydf %>% droplevels(.)
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
    #geom_text(aes(label=N_W), color="black", position = position_dodge(0.5)) +
    #geom_text(aes(label=minN), color="black", position = position_dodge(0.5)) +
    geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE) +
    scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
    scale_y_continuous(
      breaks=axisbreaks, 
      trans='log10', 
      labels = axislabels
    ) + coord_flip(ylim=axislims)+
    labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=10, face = "bold"),
          strip.text.y = element_text(size=facet_lab_size, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}


base_plot_diff <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, facet_lab_size = 10){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
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
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
    #geom_text(aes(label=N_W), color="black", position = position_dodge(0.5)) +
    geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
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
          strip.text.x = element_text(size=10, face = "bold"),
          strip.text.y = element_text(size=facet_lab_size, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------

#Primary figure
p_diar_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)
p_diar_1_adj
ggsave(p_diar_1_adj, file = paste0(here::here(),"/figures/pngs/pres_aim2_diar_1_adj.png"), width = 10, height = 4)


p_haz_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
ggsave(p_haz_1_adj, file = paste0(here::here(),"/figures/pngs/pres_aim2_haz_1_adj.png"), width = 10, height = 4)





