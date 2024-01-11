

rm(list=ls())
source(here::here("0-config.R"))
library(gt)
library(patchwork)


adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 

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



adj_RR$est <- paste0(sprintf("%.2f",adj_RR$RR)," (",
                     sprintf("%.2f",adj_RR$ci.lb),", ",
                     sprintf("%.2f",adj_RR$ci.ub),")")
adj_RR$est[adj_RR$study!="Pooled"] <- ""

adj_RR$est_diff <- paste0(sprintf("%.2f",adj_RR$coef)," (",
                          sprintf("%.2f",adj_RR$ci.lb),", ",
                          sprintf("%.2f",adj_RR$ci.ub),")")
adj_RR$est_diff[adj_RR$study!="Pooled"] <- ""

adj_RR <- adj_RR %>% filter(target %in% c("Any pathogen","Any MST"), Y=="diar7d") 
sample_cats = levels(adj_RR$sample_cat)[levels(adj_RR$sample_cat)!="Any sample"]


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------



legend_labels=sample_cats
drop_full_sparse=F
facet_lab_size=10
facet=T
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies" = my_colors[9])
  
  if(drop_full_sparse){
    adj_RR <- adj_RR %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  minCI <- min(adj_RR$ci.lb, na.rm=T)-0.001
  maxCI <- max(adj_RR$ci.ub, na.rm=T) +0.001
  axislims = c(minCI, maxCI)
  axislims[1]=ifelse(axislims[1] < 1/20, (1/20)-0.001, axislims[1])
  axislims[2]=ifelse(axislims[2] > 20, 20.001, axislims[2])
  axisindex= 1:9
  axisbreaks = c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16)
  axisindex = axisindex[axisbreaks > axislims[1] & axisbreaks < axislims[2]]
  axisbreaks = axisbreaks[axisindex]
  axislabels =c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")[axisindex]
  
  adj_RR <- adj_RR %>% droplevels(.)
  p <- ggplot(data = adj_RR, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
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
  
    p <- p + facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") 





#adj_RR <- adj_RR %>% mutate(target_f=factor(target_f, levels=c("Ascaris","Trichuris","Giardia","C. difficile","Campylo-\nbacter","Pathogenic\nE. coli","Shigella","Norovirus","Salmonella","Rotavirus")))
adj_RR <- adj_RR %>% mutate(Y_cat= paste0(study,"-",sample,"-",target),
                            N_lab=paste0(nY,"/",N),
                            prev_ratio=paste0(RR," (",ci.lb,", ", ci.ub,")"))
#Drop N's for pooled
adj_RR$N_lab[adj_RR$study=="Pooled"] <- ""

adj_RR$Y_cat <- as.character(adj_RR$Y_cat)
unique(adj_RR$Y_cat)

adj_RR <- adj_RR %>% arrange(target, sample, study)
adj_RR$Y_cat=factor(adj_RR$Y_cat, levels=unique(adj_RR$Y_cat))

#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------


legend_labels=sample_cats
drop_full_sparse=F


my_colors = c("grey20",carto_pal(12, "Prism"))

colours <- c("Any sample" = my_colors[1],
             "Source water" = my_colors[3],
             "Stored water"  = my_colors[4],
             "Child hand rinse"  = my_colors[7],
             "Mother hand rinse" = my_colors[8],
             "Latrine soil" = my_colors[5],
             "House soil" = my_colors[6],
             "Flies" = my_colors[9])

adj_RR <- adj_RR %>% filter(sample_cat!="Sparse data") %>% droplevels(.) %>%
  mutate(point_shape=factor(ifelse(study=="Pooled","1","0")))

p_mid <- ggplot(data = adj_RR, (aes(x=Y_cat, y=RR, group=sample_cat, color=sample_cat, shape=factor(point_shape, levels=c("0","1"))))) + 
  geom_point(size=3, position = position_dodge(1), alpha=0.75) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(1),
                width = 0.3, size = 1) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  geom_text(aes(y=RR, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, vjust=-0.05, size=4) +
  scale_shape_manual(values=c(15, 5), guide="none") + 
  geom_hline(yintercept = 1, linetype="dashed") +
  geom_vline(xintercept = c(1.5, 3.5, 5.5,7.5,9.5,10.5,13.5,14.5,15.5), linetype="solid") +
  #facet_wrap(~target_f, ncol=1,  scales="free_y", space = "free_x") +
  #scale_x_discrete(labels=(adj_RR$study)) +
  scale_y_continuous(
    breaks=c(0.25, .5,1, 2, 4),
    trans='log10',
    labels = c( "1/4","1/2","1", "2", "4")
  ) + coord_flip(ylim=c(0.35,4)) +
  labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none",
        strip.placement = "outside",
        strip.text.x = element_text(size=10, face = "bold"),
        strip.text.y = element_text(size=10, angle = 270, face = "bold"),          
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines"),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"))  + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE),
         color=guide_legend(nrow=2,byrow=TRUE))
p_mid

#to do: make no space between facets like in KI


#---------------------------------------------------------------
# Plot tables
#---------------------------------------------------------------



#pre-process
unique(adj_RR$target)
unique(adj_RR$sample)
adj_RR <- adj_RR %>% 
            mutate(sample=case_when(sample=="LS" ~ "Latrine Soil",
                                    sample=="S" ~ "Soil",
                                    sample=="Fly" ~ "Fly",
                                    sample=="CH" ~ "Child hands",
                                    sample=="MH" ~ "Caregiver hands",
                                    sample=="W" ~ "Stored water",
                                    sample=="SW" ~ "Source water",
                                    sample=="any sample type" ~ "Any sample type"
                                    ),
                   target=case_when(
                     target=="Pathogenic E. coli" ~ "Path. E. coli",
                     target==target ~ target))


# wrangle results into pre-plotting table form
res_plot <- adj_RR |>
  mutate(across(c(RR, ci.lb, ci.ub), ~str_pad(round(.x, 2), width=4, pad="0", side="right")),
         estimate_lab = paste0(RR , " (", ci.lb, ", ", ci.ub,")")#,
         #color = rep(c("gray","white"),5)
  ) |>
  mutate(pval  = case_when(pval  < .0001 ~ "<0.0001", TRUE ~ str_pad(as.character(round(pval , 3)),width=4,pad="0",side="right"))) |>
  bind_rows(data.frame(study = "Study",sample = "Sample", target="Target",  N_lab="n/N", 
                       estimate_lab = "PR (95% CI)", conf.low = "", conf.high="",pval ="p-value")) |>
  mutate(study = fct_rev(fct_relevel(study, "Study")),
         sample = fct_rev(fct_relevel(sample, "Sample")),
         N_lab = fct_rev(fct_relevel(N_lab, "n/N")),
         target = fct_rev(fct_relevel(target, "Target")))



#pad white space
n_blank_rows=1
res_plot <-bind_rows(data.frame(Y_cat=paste("blank",1:n_blank_rows),
                                study = rep("",n_blank_rows),
                                sample = rep("",n_blank_rows), 
                                target=rep("",n_blank_rows),  
                                N_lab=rep("",n_blank_rows), 
                                estimate_lab = rep("",n_blank_rows), 
                                conf.low = rep("",n_blank_rows), 
                                conf.high=rep("",n_blank_rows),
                                pval =rep("",n_blank_rows)),
                     res_plot)
res_plot$Y_cat = factor(res_plot$Y_cat, levels=unique(res_plot$Y_cat))

# left side of plot - hazard ratios
text_size=2
p_left <-
  res_plot  |>
  ggplot(aes(y = Y_cat)) + 
  geom_text(aes(x=0, label=study), hjust=0, size=text_size, fontface = "bold") +
  geom_text(aes(x=.95, label=target), hjust=0, size=text_size, fontface = ifelse(res_plot$study == "Study", "bold", "plain")) +
  geom_text(aes(x=1.7, label=sample), hjust=0, size=text_size, fontface = ifelse(res_plot$study == "Study", "bold", "plain")) +
  geom_text(aes(x=2.65, label=N_lab), hjust=0, size=text_size, fontface = ifelse(res_plot$study == "Study", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,4))

# right side of plot - pvalues
p_right <-
  res_plot  |>
  ggplot() +
  geom_text(aes(x=0.15, y=Y_cat, label=estimate_lab), hjust=0, size=text_size, fontface = ifelse(res_plot$estimate_lab == "PR (95% CI)", "bold", "plain")) +
  geom_text(aes(x=0.75, y=Y_cat, label=pval), hjust=0, size=text_size, fontface = ifelse(res_plot$pval == "p-value", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,1))

# layout design (top, left, bottom, right)
layout <- c(
  area(t = 0, l = 0, b = 20, r = 5.5),
  area(t = 2.99, l = 5.55, b = 20, r = 9),
  area(t = 0, l = 10, b = 20, r = 11)
)

# final plot arrangement                                       
p_tab <- p_left + plot_spacer() + p_right + 
  plot_layout(design = layout)
p<-p_tab +  inset_element(p_mid, left = -2.688, bottom = 0, right = 0.1, top = .959) 
p

## save final figure
ggsave(filename=here("C:/Users/andre/Dropbox/IPD WASH/Aim 2/figures/aim2-fig-3-reformatted.pdf"), plot = p, device='pdf',width=7.2,height=7.2)

