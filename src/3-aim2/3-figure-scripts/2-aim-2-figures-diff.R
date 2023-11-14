

rm(list=ls())
source(here::here("0-config.R"))


adj_diff <- readRDS(here("results/adjusted_aim2_abund_res.Rds"))
adj_diff_roq <- readRDS(here("results/adjusted_aim2_abund_ROQ_res.Rds"))

sample_cats = levels(adj_diff$sample_cat)[levels(adj_diff$sample_cat)!="Any sample"]

adj_diff <- clean_res(adj_diff) 
adj_diff_roq <- clean_res(adj_diff_roq) 

head(adj_diff)

#examine covariates
adj_diff$W

#count number of covariates
adj_diff$N_W <- str_count(adj_diff$W,",") + 1
adj_diff$N_W[adj_diff$W=="unadjusted"] <- 0

adj_diff <- adj_diff %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)

adj_diff_roq <- adj_diff_roq %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)



#------------------------------------
#Diarrhea plot
#------------------------------------


d <- adj_diff %>% filter(sparse=="no", Y=="diar7d")
table(d$target)
table(d$sample)
table(d$target, d$sample)
head(d)


#Check for any significant
d %>% filter((ci.lb < 1 & ci.ub < 1) | (ci.lb > 1 & ci.ub > 1))

legend_labels=sample_cats
drop_full_sparse=F
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

# p_abund_diar <- ggplot(data = d, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
#   geom_point(size=3, position = position_dodge(0.5)) +
#   geom_text(aes(label=sig_cat), color="black", position = position_dodge(0.5), vjust = -0.1) +
#   geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
#                 width = 0.3, size = 1) +
#   scale_color_manual(breaks = legend_labels,
#                      values = colours, drop = FALSE) +
#   scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
#   geom_hline(yintercept = 1, linetype="dashed") +
#   facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
#   scale_y_continuous(breaks=c(0.0625, .125, 0.25, 0.5,1, 2, 4, 8, 16, 32), trans='log10', labels=scaleFUN) + 
#   coord_flip(ylim=c(0.0625, 32)) +
#   labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
#   theme_ki() + 
#   theme(axis.ticks.x=element_blank(),
#         legend.position = "bottom",
#         strip.placement = "outside",
#         strip.text.x = element_text(size=11, face = "bold"),
#         strip.text.y = element_text(size=11, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
#         panel.spacing = unit(0, "lines")) 

#need to get pretty + automatic breaks
p_abund_diar <- ggplot(data = d, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),  position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=3) +
  #facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
  scale_y_continuous(
    breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
    trans='log10', 
    labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
  ) +
  coord_flip() +
  labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
   theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(size=11, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines"))
p_abund_diar




#------------------------------------
#HAZ plot
#------------------------------------

dhaz <- adj_diff %>% filter(sparse=="no", Y=="haz")
head(dhaz)

#Check for any significant
dhaz %>% filter((ci.lb < 1 & ci.ub < 1) | (ci.lb > 1 & ci.ub > 1))


p_abund_haz <- ggplot(data = dhaz, (aes(x=study, y=coef, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=4) +
  #facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
  scale_y_continuous(breaks=scales::pretty_breaks(n=6)) + 
  coord_flip() +
  labs(color="Sample type") + xlab("") + ylab("Mean HAZ difference") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(size=11, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines"))
p_abund_haz






#------------------------------------
# ROQ plots plot
#------------------------------------



d <- adj_diff_roq %>% filter(sparse=="no", Y=="diar7d")
table(d$target)
table(d$sample)
table(d$target, d$sample)
head(d)


#Check for any significant
d %>% filter((ci.lb < 1 & ci.ub < 1) | (ci.lb > 1 & ci.ub > 1))

legend_labels=sample_cats
drop_full_sparse=F
my_colors = c("grey20",carto_pal(12, "Prism"))

colours <- c("Any sample" = my_colors[1],
             "Source water" = my_colors[3],
             "Stored water"  = my_colors[4],
             "Child hands"  = my_colors[7],
             "Mother's hands" = my_colors[8],
             "Latrine soil" = my_colors[5],
             "House soil" = my_colors[6],
             "Flies" = my_colors[9],
             "Sparse data" = "grey50")

#need to get pretty + automatic breaks
p_abund_diar_ROQ <- ggplot(data = d, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),  position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=3) +
  #facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
  scale_y_continuous(
    breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
    trans='log10', 
    labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
  ) +
  coord_flip() +
  labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(size=11, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines"))
p_abund_diar_ROQ


dhaz <- adj_diff_roq %>% filter(sparse=="no", Y=="haz")
head(dhaz)

#Check for any significant
dhaz %>% filter((ci.lb < 1 & ci.ub < 1) | (ci.lb > 1 & ci.ub > 1))


p_abund_haz_ROQ <- ggplot(data = dhaz, (aes(x=study, y=coef, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=4) +
  #facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
  scale_y_continuous(breaks=scales::pretty_breaks(n=6)) + 
  coord_flip() +
  labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(size=11, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines"))
p_abund_haz_ROQ






#------------------------------------
# Save plots
#------------------------------------
save(list=ls(pattern="p_"), file=here("figures/aim2_abund_figures.Rdata"))
ls(pattern="p_")



# #Check some of the more unexpected findings
# 
# d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
# unique(d$study)
# unique(d$target)
# df <- d %>% filter(study=="Capone 2022 in prep") %>% filter(!is.na(abund)) %>% droplevels()
# head(df)
# 
# p_spline <- ggplot(df, aes(x=abund, y=haz)) +
#   facet_wrap(~target) +
#   geom_smooth(method="lm") +
#   scale_x_continuous(trans = "log10")
# p_spline
# 
# 
# df <- d %>% filter(study=="Holcomb 2020") %>% filter(!is.na(abund)) %>% droplevels()
# head(df)
# 
# p_spline <- ggplot(df, aes(x=abund, y=haz)) +
#   facet_wrap(sample~target) +
#   geom_smooth(method="lm") +
#   scale_x_continuous(trans = "log10")
# p_spline
# 
# df2 <- df %>% filter(target=="Avian (GFD)", sample=="LS") 
# 
# table(df2$abund)
# table(round(df2$haz,1), round(df2$abund,0))
# 
# hist(df2$abund)
# hist(log10(df2$abund))
# 
# p_spline2 <- ggplot(df2, aes(x=log10(abund), y=haz)) +
#   geom_jitter() +
#   geom_smooth(method="lm") 
# p_spline2
# 
# df3 <- df %>% filter(target=="Avian (GFD)", sample=="LS", qual=="ROQ") 
# 
# table(round(df3$haz,1), round(df3$abund,0))
# 
# p_spline2 <- ggplot(df2, aes(x=log10(abund), y=haz)) +
#   geom_jitter() +
#   geom_smooth(method="lm") 
# p_spline2


