
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 
d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data.rds"))


#examine N's by study and outcome
tab <- adj_RR %>% group_by(study, Y) %>% 
  filter(study!="Pooled") %>%
  summarise(
    medianN=median(N, na.rm=T), 
    maxN=max(N, na.rm=T),
    median_case=median(a+c, na.rm=T), 
    max_case=max(a+c, na.rm=T)) %>% 
  filter(maxN!=0) %>%
  as.data.frame()
tab


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




res2 <- adj_RR %>% filter(target %in% c("Avian (GFD)"), )
res2

#---------------------------------------------------------------
# Print pooled estimates
#---------------------------------------------------------------

adj_RR$est <- paste0(sprintf("%.2f",adj_RR$RR)," (",
                     sprintf("%.2f",adj_RR$ci.lb),", ",
                     sprintf("%.2f",adj_RR$ci.ub),")")
adj_RR$est[adj_RR$study!="Pooled"] <- ""

adj_RR$est_diff <- paste0(sprintf("%.2f",adj_RR$coef)," (",
                     sprintf("%.2f",adj_RR$ci.lb),", ",
                     sprintf("%.2f",adj_RR$ci.ub),")")
adj_RR$est_diff[adj_RR$study!="Pooled"] <- ""

unadj_RR$est <- paste0(sprintf("%.2f",unadj_RR$RR)," (",
                       sprintf("%.2f",unadj_RR$ci.lb),", ",
                       sprintf("%.2f",unadj_RR$ci.ub),")")
unadj_RR$est[unadj_RR$study!="Pooled"] <- ""

unadj_RR$est_diff <- paste0(sprintf("%.2f",unadj_RR$coef)," (",
                          sprintf("%.2f",unadj_RR$ci.lb),", ",
                          sprintf("%.2f",unadj_RR$ci.ub),")")
unadj_RR$est_diff[unadj_RR$study!="Pooled"] <- ""

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


base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, facet_lab_size=10){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
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
    #geom_text(aes(y=RR, label=est), color="black", vjust = -0.8, hjust = -0.1, size=1.5) +
    geom_text(aes(y=RR, label=est), color="black", vjust = -0.8, hjust = -0.1, size=2.25) +
    geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE) +
    scale_shape_manual(values=c(16, 13,18), guide = "none") + 
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
    geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), vjust =0.75, hjust = -0.5, size=4) +
    geom_text(aes(y=coef, label=est_diff), color="black", vjust = -1.1, hjust = -0.1, size=2.25) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE) +
    scale_shape_manual(values=c(16, 13,18), guide = "none") + 
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
p_diar_1_unadj <- unadj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)
ggsave(p_diar_1_unadj, file = paste0(here::here(),"/figures/pngs/aim2_p_diar_1_unadj.png"), width = 10, height = 6)

p_diar_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)
p_diar_1_adj
ggsave(p_diar_1_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_diar_1_adj.png"), width = 10, height = 6)

ggsave(filename=here("C:/Users/andre/Dropbox/IPD WASH/Aim 2/figures/aim2-fig-3.pdf"), plot = p_diar_1_adj, device='pdf',width=10,height=6)


#get I2 for figure captions
adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d", study=="Pooled", !is.na(Q)) %>% 
  select(Y, sample, target, I2, Q)


p_haz_1 <- unadj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
ggsave(p_haz_1, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_1_unadj.png"), width = 10, height = 6)

p_haz_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
ggsave(p_haz_1_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_1_adj.png"), width = 10, height = 6)
ggsave(filename=here("C:/Users/andre/Dropbox/IPD WASH/Aim 2/figures/aim2-fig-4.pdf"), plot = p_haz_1_adj, device='pdf',width=10,height=6)

#get I2 for figure captions
adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="haz", study=="Pooled", !is.na(I2)) %>% 
  select(Y, sample, target, I2, Q)




p_stunt_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="stunt") %>%
  base_plot(drop_full_sparse=T)
ggsave(p_stunt_1_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_stunt_1_adj.png"), width = 10, height = 6)


p_wast_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="wast") %>%
  base_plot(drop_full_sparse=T)
ggsave(p_wast_1_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_wast_1_adj.png"), width = 10, height = 6)


p_underwt_1_adj <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), Y=="underwt") %>%
  base_plot(drop_full_sparse=T)
ggsave(p_underwt_1_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_underwt_1_adj.png"), width = 10, height = 6)





p_diar_2_unadj <- unadj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)
ggsave(p_diar_2_unadj, file = paste0(here::here(),"/figures/pngs/aim2_p_diar_2_unadj.png"), width = 10, height = 6)

p_diar_2_adj <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)
ggsave(p_diar_2_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_diar_2_adj.png"), width = 10, height = 6)


p_diar_s1_adj <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="diar7d") %>%
  base_plot(drop_full_sparse=T)
ggsave(p_diar_s1_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_diar_s1_adj.png"), width = 10, height = 6)


p_haz_2 <- unadj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
ggsave(p_haz_2, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_2.png"), width = 10, height = 6)


p_haz_s1 <- unadj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
ggsave(p_haz_s1, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_s1.png"), width = 10, height = 6)




p_haz_2_adj <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
ggsave(p_haz_2_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_2_adj.png"), width = 10, height = 6)


p_haz_s1_adj <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), Y=="haz") %>%
  base_plot_diff(drop_full_sparse=T)
ggsave(p_haz_s1_adj, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_s1_adj.png"), width = 10, height = 6)




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

p_haz_1_adj

