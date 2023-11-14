
rm(list=ls())
source(here::here("0-config.R"))

#-----------------------------------
#heatmap per study and sample by target combination and covariate p-value 
#-----------------------------------

res <- readRDS(here("results/covariate_associations.Rds"))
levels(res$study)[levels(res$study) == "Capone 2022 in prep"] <- "Capone 2022"

require(RColorBrewer)
res$pval[res$pval==1] <- NA
res <- res %>% filter(var!="failed", !is.na(pval))
textcol=tableau11[1]

cols = (brewer.pal(n = 9, name = "Spectral"))
colours <- c(`<0.01 increase risk` = cols[1], `<0.05 increase risk` = cols[2], 
             `0.05-0.2 increase risk` = cols[3],
             `0.2-1` = cols[5],  
             `0.05-0.2 decrease risk` = cols[7], `<0.05 decrease risk` = cols[8], 
             `<0.01 decrease risk` = cols[9], `Not estimated` = "gray80")


res$pval_cat <- cut(res$pval, breaks = c(-1, 0.01, 0.05, 0.2, 
                                         0.5, 2), labels = c("<0.01", "<0.05", "0.05-0.2", 
                                                              "0.2-0.5", "0.5-1"))

table(res$pval_cat)
res$pval_cat <- factor(res$pval_cat, levels = c("<0.01", "<0.05", "0.05-0.2", 
                                                "0.2-0.5", "0.5-1"))

#Keep colors consistent with other heatmaps
cols = (brewer.pal(n = 9, name = "Spectral"))
colours <- c(`<0.01` = cols[1], `<0.05` = cols[2], 
             `0.05-0.2` = cols[3],
             `0.2-0.5` = cols[4],
             `0.5-1` = cols[5])




res <- droplevels(res)
res <- res %>% group_by(var) %>%
  mutate(aveP=mean(pval)) %>% ungroup() %>%
  arrange(aveP) %>%
  mutate(var=factor(var, levels=unique(var)),
         target=factor(target, levels=rev(unique(target))))

#-----------------------------------
#heatmap with all targets
#-----------------------------------


plot_heatmap_all <- function(res, study){
  plot_df <- res %>% filter(study=={{study}})
  
  p <-  ggplot(plot_df, aes(x=var, y=target, fill=pval_cat)) +
      geom_tile(colour = "grey80", size = 0.25) + 
      facet_wrap(~sample, scales = "free") +
      scale_x_discrete(expand = c(0, 0)) + 
      scale_y_discrete(expand = c(0, 0)) + 
      theme_minimal(base_size = 10) + 
      scale_fill_manual(values = colours, drop = FALSE) + 
      #geom_text(aes(label = est)) + 
      theme(aspect.ratio = 1, legend.title = element_text(color = textcol, 
             size = 8), legend.margin = margin(grid::unit(0.1,"cm")), 
            legend.text = element_text(colour = textcol, size = 7, face = "bold"), legend.key.height = grid::unit(0.2, 
           "cm"), legend.key.width = grid::unit(1, "cm"), 
            legend.position = "right", axis.text.y = element_text(size = 8, 
            vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4), 
            plot.title = element_text(colour = textcol, hjust = 0, 
            size = 12, face = "bold"), strip.text.x = element_text(size = 10), 
            axis.text.x = element_text(size = 8, angle = -45, hjust = 0, colour = textcol),
            strip.text.y = element_text(angle = 0, size = 10), 
            plot.background = element_blank(), panel.border = element_blank(), 
            strip.background = element_blank(), panel.background = element_rect(fill = "grey80", 
            colour = "grey80"), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + guides(fill = guide_legend("P-value strength", 
            ncol = 1)) + labs(x = "Covariate", y = "Target", title = study)
  
  return(p)
}

unique(res$study)
p_fuhr <- plot_heatmap_all(res, study="Fuhrmeister 2020")
p_boehm <- plot_heatmap_all(res, study="Boehm 2016")
p_kwong <- plot_heatmap_all(res, study="Kwong 2021")
p_stein <- plot_heatmap_all(res, study="Steinbaum 2019")
p_holc <- plot_heatmap_all(res, study="Holcomb 2020")
p_cap1 <- plot_heatmap_all(res, study="Capone 2021")
p_cap2 <- plot_heatmap_all(res, study="Capone 2022")
p_reese <- plot_heatmap_all(res, study="Reese 2017")
p_odag <- plot_heatmap_all(res, study="Odagiri 2016")



#-----------------------------------
#heatmap with all studies
#-----------------------------------
plot_df <- res %>% filter(sample=="any sample type")

p_cov_all <-  ggplot(plot_df, aes(x=var, y=target, fill=pval_cat)) +
  geom_tile(colour = "grey80", size = 0.25) + 
  facet_wrap(~study, scales = "free") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme_minimal(base_size = 10) + 
  scale_fill_manual(values = colours, drop = FALSE) + 
  #geom_text(aes(label = est)) + 
  theme(#aspect.ratio = 1, 
        legend.title = element_text(color = textcol, size = 8), legend.margin = margin(grid::unit(0.1,"cm")), 
        legend.text = element_text(colour = textcol, size = 7, face = "bold"),
        legend.key.height = grid::unit(0.2, "cm"), legend.key.width = grid::unit(1, "cm"), 
        legend.position = "top", legend.direction="horizontal", 
        axis.text.y = element_text(size = 7,  vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4), 
        plot.title = element_text(colour = textcol, hjust = 0,  size = 12, face = "bold"), 
        strip.text.x = element_text(size = 8), 
        axis.text.x = element_text(size = 7, angle = -30, hjust = 0, colour = textcol),
        strip.text.y = element_text(angle = 0, size = 8), 
        plot.background = element_blank(), panel.border = element_blank(), 
        strip.background = element_blank(), panel.background = element_rect(fill = "grey80", 
                                                                            colour = "grey80"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + guides(fill = guide_legend("P-value strength", 
                                                                         ncol = 7)) + labs(x = "Covariate", y = "Target", title = "")

ggsave(p_cov_all, file = paste0(here::here(),"/figures/pngs/aim2_p_covariate.png"), width = 10, height = 7)

save(list=ls(pattern="p_"), file=here("figures/aim2_covar_sig_figures.Rdata"))

