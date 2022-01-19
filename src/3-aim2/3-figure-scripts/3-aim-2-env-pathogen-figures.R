
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 
d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data.rds"))

temp<-adj_RR%>%filter(Y=="diar7d", target=="Any MST")
table(temp$sample_cat)

unique(adj_RR$target)

#count number of covariates
unadj_RR$N_W <- ""
adj_RR$N_W <- str_count(adj_RR$W,",") + 1
adj_RR$N_W[adj_RR$W=="unadjusted"] <- 0


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
# adj_RR$sparse[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- "yes"
# adj_RR$RR[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- NA
# adj_RR$ci.lb[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- NA
# adj_RR$ci.ub[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- NA

res <- adj_RR %>% filter(target %in% c("Any MST"), 
                         Y=="diar7d", study=="Holcomb 2020",
                         sample=="any sample type")
res$N_W
res$W
#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

#unadj_RR<-unadj_RR %>% filter(!is.na(se))
sample_cats = levels(adj_RR$sample_cat)#[levels(adj_RR$sample_cat)!="Any sample"]


#----------------------------------------------------
# Specific pathogens
#----------------------------------------------------
# diar_adj_path_df <- adj_RR %>% 
#   filter( Y=="diar7d", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) #%>%
#   #base_plot(drop_full_sparse=T, facet_lab_size = 8)

#Drop sparse and any repeat of the any sample type when only one sample type
# diar_adj_path_df <- diar_adj_path_df %>% group_by(target) %>%
#   filter(n()!=sum(sparse=="yes")) %>% ungroup()

#Drop carraige returns
adj_RR$target_f <- gsub("\n"," ",adj_RR$target_f)

diar_adj_path_df <- adj_RR %>% 
  filter(sparse=="no", Y=="diar7d", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  group_by(target, study) %>%
  mutate(N=n()) %>%
  filter(!(N==2 & sample=="any sample type")) %>% ungroup()
table(diar_adj_path_df$N)
table(diar_adj_path_df$N, diar_adj_path_df$sample)

table(d$study)
table(d$target)
temp <- diar_adj_path_df %>% filter(study=="Capone 2021 in prep", target=="Pathogenic E. coli")
head(temp)

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

p_diar_adj_path <- ggplot(data = diar_adj_path_df, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),  position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=3) +
  scale_y_continuous(
    breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
    trans='log10', 
    labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
  ) +
  coord_flip() +
  labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = c(0.7,0.075),
        strip.text.x = element_text(size=10, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines")) + 
  guides(colour = guide_legend(nrow = 3))


ggsave(p_diar_adj_path, file = paste0(here::here(),"/figures/pngs/p_diar_adj_path.png"), width = 10, height = 6)

# p_haz_adj_path <- adj_RR %>% 
#   filter( Y=="haz", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
#   base_plot_diff(drop_full_sparse=T, facet_lab_size = 8)

haz_adj_path_df <- adj_RR %>% 
  filter(sparse=="no", Y=="haz", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  group_by(target, study) %>%
  mutate(N=n()) %>%
  filter(!(N==2 & sample=="any sample type")) %>% ungroup()


p_haz_adj_path <- ggplot(data = haz_adj_path_df, (aes(x=study, y=coef, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=4) +
  scale_y_continuous(breaks=scales::pretty_breaks(n=6)) + 
  coord_flip() +
  labs(color="Sample type") + xlab("") + ylab("Mean Z-score difference") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = c(0.7,0.075),
        strip.text.x = element_text(size=10, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines")) + 
  guides(colour = guide_legend(nrow = 3))
ggsave(p_haz_adj_path, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_adj_path.png"), width = 10, height = 6)









diar_adj_MST_df <- adj_RR %>% 
  filter(sparse=="no", Y=="diar7d", target %in% any_MST, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  group_by(target, study) %>%
  mutate(N=n()) %>%
  filter(!(N==2 & sample=="any sample type")) %>% ungroup()
table(diar_adj_MST_df$N)
table(diar_adj_MST_df$N, diar_adj_MST_df$sample)

table(d$study)
table(d$target)
temp <- diar_adj_MST_df %>% filter(study=="Capone 2021 in prep", target=="Pathogenic E. coli")
head(temp)

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

p_diar_adj_MST <- ggplot(data = diar_adj_MST_df, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),  position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=3) +
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
        strip.text.x = element_text(size=10, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines")) + 
  guides(colour = guide_legend(nrow = 3))
ggsave(p_diar_adj_MST, file = paste0(here::here(),"/figures/pngs/p_diar_adj_MST.png"), width = 10, height = 6)

# p_haz_adj_MST <- adj_RR %>% 
#   filter( Y=="haz", target %in% any_MSTogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
#   base_plot_diff(drop_full_sparse=T, facet_lab_size = 8)

haz_adj_MST_df <- adj_RR %>% 
  filter(sparse=="no", Y=="haz", target %in% any_MST, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  group_by(target, study) %>%
  mutate(N=n()) %>%
  filter(!(N==2 & sample=="any sample type")) %>% ungroup()


p_haz_adj_MST <- ggplot(data = haz_adj_MST_df, (aes(x=study, y=coef, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  facet_wrap(~target_f,  scales="free", ncol=4) +
  scale_y_continuous(breaks=scales::pretty_breaks(n=6)) + 
  coord_flip() +
  labs(color="Sample type") + xlab("") + ylab("Mean Z-score difference") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = c(0.7,0.075),
        strip.text.x = element_text(size=10, face = "bold"),
        strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
        panel.spacing = unit(0, "lines")) + 
  guides(colour = guide_legend(nrow = 3))
ggsave(p_haz_adj_MST, file = paste0(here::here(),"/figures/pngs/aim2_p_haz_adj_MST.png"), width = 10, height = 6)




#save figures
save(list=ls(pattern="p_"), file=here("figures/aim2_pathogen_specific_figures.Rdata"))
ls(pattern="p_")



