
rm(list=ls())
source(here::here("0-config.R"))

adj_RR <- readRDS(file=here("results/adjusted_aim2_tr_res.Rds")) 
unadj_RR <- readRDS(file=here("results/unadjusted_aim2_tr_res.Rds")) 

temp<-adj_RR%>%filter(Y=="diar7d", target=="Any MST")
table(temp$sample_cat)

unique(adj_RR$target)

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



#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
  

p_diar_tr <- ggplot(data = adj_RR %>% filter(Y=="diar7d"), (aes(x=trial, y=RR, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                width = 0.3, size = 1, color="black") +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75, color="black") +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_y_continuous(
    trans='log10', 
  ) + coord_flip(ylim=NULL)+
   xlab("") + ylab("Adjusted prevalence ratio:\nTreatment effects on diarrhea") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) 



p_haz_tr <- ggplot(data = adj_RR %>% filter(Y=="haz"), (aes(x=trial, y=coef, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                width = 0.3, size = 1, color="black") +
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75, color="black") +
  geom_text(aes(y=ci.ub, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, size=4) +
  scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip()+
  xlab("") + ylab("Adjusted mean difference:\nTreatment effects on HAZ") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) 
p_haz_tr


#save figures
save(list=ls(pattern="p_"), file=here("figures/aim2_tr_figures.Rdata"))
ls(pattern="p_")



