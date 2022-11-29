

rm(list=ls())
source(here::here("0-config.R"))

fullres_adj <- readRDS(here("results/aim2_sens_diar_time_res.Rds"))

plotdf <- fullres_adj %>% group_by(study, target, sample) %>% filter(n()>1)
plotdf$study <- factor(plotdf$study, levels = unique(plotdf$study))
levels(plotdf$study)

plotdf <- plotdf %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)



mydf <- plotdf %>%
  filter(target %in% c("Any pathogen","Any MST")) 


temp<-mydf %>% filter(study=="Pooled")

legend_labels = levels(fullres_adj$sample_cat)[levels(fullres_adj$sample_cat)!="Any sample"]

facet_lab_size=10

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
mydf$time <- factor(mydf$time, levels = c("All", "4 months", "1 month"))

p_diar_time <- ggplot(data = mydf, (aes(x=study, y=RR, group=time, color=sample_cat, shape=time))) + 
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_text(aes(label=sig_cat), color="black", position = position_dodge(0.5), vjust = -0.01) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE, guide="none") +
  scale_shape_manual(values=c(12,19, 9)) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_grid(target_f~sample_cat ,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
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

ggsave(p_diar_time, file = paste0(here::here(),"/figures/pngs/aim2_p_diar_time_comp.png"), width = 10, height = 6)

save(list=ls(pattern="p_"), file=here("figures/aim2_diar_time_figures.Rdata"))
