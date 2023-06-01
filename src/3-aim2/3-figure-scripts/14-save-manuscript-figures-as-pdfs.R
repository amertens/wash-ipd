
library(tidyverse)
library(RColorBrewer)
library(officedown)
library(janitor)
library(knitr)
library(here)
library(officer)
library(flextable)
library(bibtex)
library(citr)

textcol = "grey20"
cols = (brewer.pal(n = 9, name = "Spectral"))
colours <- c(`<0.01 (increased risk)` = cols[1], `<0.05 (increased risk)` = cols[2], 
             `0.05-0.2 (increased risk)` = cols[3],
             `0.2-1` = cols[5],  
             `0.05-0.2 (decreased risk)` = cols[7], `<0.05 (decreased risk)` = cols[8], 
             `<0.01 (decreased risk)` = cols[9], `Not estimated` = "gray80")

panel_spacing = 0.65

source(here("0-config.R"))


op = function(x, d=2) sprintf(paste0("%1.",d,"f"), x) 

Print <- function(x){
  prettyNum(round(x,2), big.mark = ",", scientific = FALSE)
}

library(pander)
panderOptions('digits', 2)
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)
panderOptions('graph.fontsize', 8)

panderOptions('table.alignment.default', function(df)
  ifelse(sapply(df, is.numeric), 'right', 'left'))
panderOptions('table.split.table', Inf)
panderOptions('big.mark', ",")

set_flextable_defaults(big.mark = ",", 
                       font.size = 8, theme_fun = theme_vanilla,
                       padding.bottom = 5, 
                       padding.top = 5,
                       padding.left = 5,
                       padding.right = 5)


#Clean results function
clean_est <- function(res, outcome="binomial"){
  if(outcome=="binomial"){
    
    est = format(round(res$RR, 2), nsmall = 2)
    ci.lb = format(round(res$ci.lb, 2), nsmall = 2) 
    ci.ub = format(round(res$ci.ub, 2), nsmall = 2)
    est.ci = paste0(est," (95% CI: ",ci.lb,", ",ci.ub,")")
  }else{
    est = format(round(res$coef, 2), nsmall = 2)
    ci.lb = format(round(res$ci.lb, 2), nsmall = 2) 
    ci.ub = format(round(res$ci.ub, 2), nsmall = 2)
    est.ci = paste0(est," (95% CI: ",ci.lb,", ",ci.ub,")")
  }
  return(est.ci)
}

clean_res  <- function(res, outcome="binomial"){
  if(outcome=="binomial"){
    res$est.ci = paste0("RR=",round(as.numeric(res$RR),1)," (95% CI: ",round(res$ci.lb,1),", ",round(res$ci.ub,1),")")
    res <- res %>% ungroup() %>%
      select(study, sample_type, target_f, est.ci, pval, a, b, c, d, N) 
    res$est.ci <- gsub("RR\\=1 \\(95\\% CI\\: NA, NA\\)","Not estimated",res$est.ci)
    
    res$pval <- as.character(round(res$pval,3))
    res$pval[is.na(res$pval)] <- ""
    res <- res %>% 
      rename(Study=study,
             Sample=sample_type,
             Target=target_f,
             Estimate=est.ci,
             `p-value`=pval, 
             `Positive,\nIntervention`=a,
             `Negative,\nIntervention`=b,
             `Positive,\nControl`=c,
             `Negative,\nControl`=d,
             `Total\nobservations`=N)
  }else{
    res$est.ci = paste0(round(as.numeric(res$coef),1),"Difference= (95% CI: ",round(res$ci.lb,1),", ",round(res$ci.ub,1),")")
    res <- res %>% ungroup() %>%
      select(sample_type, target_f, est.ci, pval, a, b, c, d, N) 
    res$est.ci <- gsub("Difference\\=0 \\(95\\% CI\\: NA, NA\\)","Not estimated",res$est.ci)
    res$pval <- as.character(round(res$pval,3))
    res$pval[is.na(res$pval)] <- ""
    res <- res %>% 
      rename(Study=study,
             Sample=sample_type,
             Target=target_f,
             Estimate=est.ci,
             `p-value`=pval, 
             `Positive,\nIntervention`=a,
             `Negative,\nIntervention`=b,
             `Positive,\nControl`=c,
             `Negative,\nControl`=d,
             `Total\nobservations`=N)
  }
  
  return(res)
}

clean_res_tab <- function(unadj_RR, adj_RR){
  unadj<-clean_res(unadj_RR) %>% select(Study,Sample,Target,Estimate, `p-value`) %>% 
    rename(`Unadjusted\nEstimate`=Estimate, `Unadjusted\np-value`=`p-value`) 
  adj<-clean_res(adj_RR)%>% rename(`Adjusted\nEstimate`=Estimate, `Adjusted\np-value`=`p-value`)
  
  res <- left_join(unadj, adj, by=c("Study", "Sample", "Target"))
  res <- res %>% arrange(Study,  Sample,  Target) %>% filter(Study!="Pooled")
  res$`Adjusted\nEstimate`[res$Study=="Odagiri 2016"] <- ""
  res$`Adjusted\np-value`[res$Study=="Odagiri 2016"] <- ""
  return(res)
}




heatmap_plot2 <- function(d, colours, textcol= "grey20"){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  # levels(dfull$X)
  # d<-dfull
  d$sample_cat[is.na(d$sample_cat)] <- "Any sample"
  
  
  d <- d %>% #filter(!is.na(X)) %>%
    arrange(desc(X)) %>%
    mutate(
      X = case_when(
        X=="Any sample: POOLED" ~ paste0("<span style='color:",my_colors[1],"'>**", X, "**</span>"),
        sample_cat=="Any sample" ~ paste0("<span style='color:",my_colors[1],"'>", X, "</span>"),
        sample_cat=="Source water" ~ paste0("<span style='color:",my_colors[3],"'>", X, "</span>"),
        sample_cat=="Stored water" ~ paste0("<span style='color:",my_colors[4],"'>", X, "</span>"),
        sample_cat=="Child hand rinse" ~ paste0("<span style='color:",my_colors[7],"'>", X, "</span>"),
        sample_cat=="Mother hand rinse" ~ paste0("<span style='color:",my_colors[8],"'>", X, "</span>"),
        sample_cat=="Latrine soil" ~ paste0("<span style='color:",my_colors[5],"'>", X, "</span>"),
        sample_cat=="House soil" ~ paste0("<span style='color:",my_colors[6],"'>", X, "</span>"),
        sample_cat=="Flies" ~ paste0("<span style='color:",my_colors[9],"'>", X, "</span>")
      )
    )
  
  d$X=factor(d$X, levels = rev(unique(d$X)))
  
  
  
  
  p <- ggplot(d, aes(x = Y, y = X, fill = pval_cat )) + 
    geom_tile(colour = "grey80", size = 0.25) + scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0)) +
    # scale_y_discrete(expand = c(0, 0),
    #                  breaks=levels(d$X2),
    #                  labels=lab_f) +
    theme_minimal(base_size = 8) +
    facet_grid(target~., scales = "free", space="free") +
    scale_fill_manual(values = colours, drop = FALSE, na.value = 'grey80') +
    scale_color_manual(values = c("grey30", "black"), drop = FALSE, guide = FALSE) +
    geom_text(aes(label = est, color=factor(pooled)), size=2.25) +
    theme(legend.title = element_text(color = textcol,
                                      size = 8), legend.margin = margin(grid::unit(0.1, "cm")), legend.text = element_text(colour = textcol,
                                                                                                                           size = 7, face = "bold"), legend.key.height = grid::unit(0.2,   "cm"), legend.key.width = grid::unit(1, "cm"),
          legend.position = "bottom", axis.text.x = element_text(size = 8,  colour = textcol), 
          #axis.text.y = element_text(size = 8, vjust = 0.2, colour = rev(df$sample_cols)), 
          #axis.ticks = element_line(size = 0.4),
          #axis.text.y = element_markdown(size = 8, vjust = 0.2),  
          axis.text.y = element_markdown(size = 8),  
          plot.title = element_text(colour = textcol, hjust = 0, size = 12, face = "bold"), strip.text.x = element_text(size = 10),
          strip.text.y = element_text(angle = 0, size = 10),
          plot.background = element_blank(), panel.border = element_blank(),
          strip.background = element_blank(), panel.background = element_rect(fill = "grey80", colour = "grey80"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.spacing = unit(panel_spacing, "lines")) +
    guides(fill = guide_legend("P-value strength\n(estimate direction)", nrow = 2)) + labs(x = "", y = "", title = "")
  
  
  
  return(p)
}




load(here("figures/aim2_figures.Rdata"))
load(here("figures/aim2_pathogen_specific_figures.Rdata"))
load(here("figures/aim2_abund_figures.Rdata"))
load(here("figures/aim2_covar_sig_figures.Rdata"))
load(here("figures/aim2_tmle_figures.Rdata"))
load(here("figures/aim2_pathogen_figures.Rdata"))
load(here("figures/aim2_diar_time_figures.Rdata"))
load(here("figures/aim2_adjusted_unadjusted_comp_figures.Rdata"))
load(here("figures/aim2_subgroup_figures.Rdata"))
load(here("figures/aim2_subgroup_figures_age.Rdata"))
load(here("figures/aim2_path_heatmap.Rdata"))
load(here("figures/aim2_abund_heatmap.Rdata"))
load(here("figures/aim2_all_tables.Rdata"))

load(here("figures/aim2_tr_figures.Rdata"))









# Primary figures

p_pathogen
getwd()

ggsave(filename=paste0(here::here(), "/figures/aim2_submission/WASH_IPD2_Fig_2.pdf"),
       plot = p_pathogen, device='pdf',width=10,height=6)


ggsave(filename=paste0(here::here(), "/figures/aim2_submission/WASH_IPD2_Fig_3.pdf"),
       plot = p_diar_1_adj, device='pdf',width=10,height=5)


ggsave(filename=paste0(here::here(), "/figures/aim2_submission/WASH_IPD2_Fig_4.pdf"),
       plot = p_haz_1_adj, device='pdf',width=10,height=5)






