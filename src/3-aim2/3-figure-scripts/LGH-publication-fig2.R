


rm(list=ls())
source(here::here("0-config.R"))
library(gt)
library(patchwork)

unadj_RR <- readRDS(file=here("results/pathogen_specific_aim2_res_unadj.Rds")) 
adj_RR <- readRDS(file=here("results/pathogen_specific_aim2_res.Rds")) %>% arrange(minN)


adj_RR <- adj_RR %>% filter(minN>=5)
unadj_RR <- unadj_RR %>% filter(minN>=5)

adj_RR$N_W <- str_count(adj_RR$W,",") + 1
adj_RR$N_W[adj_RR$W=="unadjusted"] <- 0

adj_RR %>% filter(target=="Ascaris", sample=="S")
adj_RR %>% filter(target=="Trichuris", sample=="S")
temp<-adj_RR %>% filter(target=="Trichuris")
unique(adj_RR$Y)

#Drop non-QPCR sth outcomes from plot to avoid overplotting
adj_RR <- adj_RR %>% filter(!(study=="Kwong 2021" & (Y=="ch_pos_ascaris" | Y=="ch_pos_trichuris" | Y=="ch_pos_trichuris_EE" | Y=="ch_pos_ascaris_EE")))
adj_RR <- adj_RR %>% filter(sample!="any sample type")


unique(adj_RR$study)
adj_RR <- adj_RR %>% mutate(study=factor(study, levels = rev(c("Steinbaum 2019","Fuhrmeister 2020","Kwong 2021","Capone 2021","Capone 2022 in prep","Boehm 2016"))))

#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

adj_RR$sparse <- "no"
adj_RR <- clean_res(adj_RR)

adj_RR <- adj_RR %>% mutate(
  sig_cat = case_when(
    pval>=0.05 ~"",
    pval<0.05 ~"*",
    pval<0.01 ~"**",
    pval<0.001 ~"***"
  )
)

sample_cats = levels(adj_RR$sample_cat)[levels(adj_RR$sample_cat)!="Any sample"]

adj_RR <- adj_RR %>% droplevels()
unique(adj_RR$target_f)
adj_RR <- adj_RR %>% mutate(target_f=factor(target_f, levels=c("Ascaris","Trichuris","Giardia","C. difficile","Campylo-\nbacter","Pathogenic\nE. coli","Shigella","Norovirus","Salmonella","Rotavirus")))
adj_RR <- adj_RR %>% mutate(Y_cat= paste0(study,"-",sample,"-",target),
                            N_lab=paste0(n,"/",N),
                            prev_ratio=paste0(RR," (",ci.lb,", ", ci.ub,")"))

adj_RR <- adj_RR %>% arrange(target, sample, study)
adj_RR$Y_cat=factor(adj_RR$Y_cat, levels=rev(unique(adj_RR$Y_cat)))

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
             "Flies" = my_colors[9],
             "Sparse data" = "grey50")

adj_RR <- adj_RR %>% droplevels(.)

p_mid <- ggplot(data = adj_RR, (aes(x=Y_cat, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_point(shape=15, size=3, position = position_dodge(1), alpha=0.75) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(1),
                width = 0.3, size = 1) +
  scale_color_manual(breaks = legend_labels,
                     values = colours, drop = FALSE) +
  geom_text(aes(y=RR, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, vjust=-0.05, size=4) +
  scale_shape_manual(values=c(16, 13,18), guide="none") + 
  geom_hline(yintercept = 1, linetype="dashed") +
  #scale_x_discrete(labels=(adj_RR$study)) +
  scale_y_continuous(
    breaks=c(0.25, .5,1, 2, 4, 8),
    trans='log10',
    labels = c( "1/4","1/2","1", "2", "4", "8")
  ) + coord_flip(ylim=c(0.35,8)) +
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
        plot.margin= grid::unit(c(0, 0, 0, 0), "in")) 


#---------------------------------------------------------------
# Plot tables
#---------------------------------------------------------------

#pre-process
unique(adj_RR$target)
adj_RR <- adj_RR %>% 
            mutate(sample=case_when(sample=="LS" ~ "Latrine Soil",
                                    sample=="S" ~ "Soil",
                                    sample=="Fly" ~ "Fly"),
                   target=case_when(
                     target=="Pathogenic E. coli" ~ "Path. E. coli",
                     target==target ~ target))


# wrangle results into pre-plotting table form
res_plot <- adj_RR |>
  mutate(across(c(RR, ci.lb, ci.ub), ~str_pad(round(.x, 2), width=4, pad="0", side="right")),
         estimate_lab = paste0(RR , " (", ci.lb, "-", ci.ub,")")#,
         #color = rep(c("gray","white"),5)
  ) |>
  mutate(pval  = case_when(pval  < .0001 ~ "<0.0001", TRUE ~ str_pad(as.character(round(pval , 3)),width=4,pad="0",side="right"))) |>
  bind_rows(data.frame(study = "Study",target = "Target",sample = "Sample", target="Pathogen",  N_lab="n/N", 
                       estimate_lab = "PR (95% CI)", conf.low = "", conf.high="",pval ="p-value")) |>
  mutate(study = fct_rev(fct_relevel(study, "Study")),
         sample = fct_rev(fct_relevel(sample, "Sample")),
         N_lab = fct_rev(fct_relevel(N_lab, "n/N")),
         target = fct_rev(fct_relevel(target, "Pathogen")))

#pad white space
n_blank_rows=1
res_plot <-bind_rows(res_plot,data.frame(Y_cat=paste("blank",1:n_blank_rows),
                                study = rep("",n_blank_rows),
                                sample = rep("",n_blank_rows), 
                                target=rep("",n_blank_rows),  
                                N_lab=rep("",n_blank_rows), 
                                estimate_lab = rep("",n_blank_rows), 
                                conf.low = rep("",n_blank_rows), 
                                conf.high=rep("",n_blank_rows),
                                pval =rep("",n_blank_rows)))
res_plot$Y_cat = factor(res_plot$Y_cat, levels=rev(unique(res_plot$Y_cat)))

# left side of plot - hazard ratios
text_size=2.25
p_left <-
  res_plot  |>
  ggplot(aes(y = Y_cat)) + 
  geom_text(aes(x=0, label=study), hjust=0, size=text_size, fontface = "bold") +
  geom_text(aes(x=1.2, label=target), hjust=0, size=text_size, fontface = ifelse(res_plot$study == "Study", "bold", "plain")) +
  geom_text(aes(x=2.1, label=sample), hjust=0, size=text_size, fontface = ifelse(res_plot$study == "Study", "bold", "plain")) +
  geom_text(aes(x=2.9, label=N_lab), hjust=0, size=text_size, fontface = ifelse(res_plot$study == "Study", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,4))

# right side of plot - pvalues
p_right <-
  res_plot  |>
  ggplot() +
  geom_text(aes(x=0, y=Y_cat, label=estimate_lab), hjust=0, size=text_size, fontface = ifelse(res_plot$estimate_lab == "PR (95% CI)", "bold", "plain")) +
  geom_text(aes(x=0.69, y=Y_cat, label=pval), hjust=0, size=text_size, fontface = ifelse(res_plot$pval == "p-value", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,1))

# layout design (top, left, bottom, right)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 4),
  area(t = 4.99, l = 4.25, b = 30, r = 9),
  area(t = 0, l = 10, b = 30, r = 11)
)

# final plot arrangement
#p <- p_left + p_mid + p_right + plot_layout(design = layout)

p_tab <- p_left + plot_spacer() + p_right + 
  plot_layout(design = layout)
p <- p_tab +  inset_element(p_mid, left = -2.875, bottom = 0.045, right = 0, top = .9) 

## save final figure
ggsave(filename=here("C:/Users/andre/Dropbox/IPD WASH/Aim 2/figures/aim2-fig-2-reformatted.pdf"), plot = p, device='pdf',width=7.2,height=5.6)
