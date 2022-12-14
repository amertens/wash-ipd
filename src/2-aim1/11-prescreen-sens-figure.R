


rm(list=ls())
source(here::here("0-config.R"))

main_res <- readRDS(file=here("results/adjusted_aim1_RR_pooled.Rds")) 
glmnet_res <- readRDS(file=here("results/adjusted_aim1_RR_prescreen_sens.Rds"))

unique(main_res$study)
main_res <- main_res %>% filter( target=="Any pathogen"|target=="Any MST") %>% mutate(analysis="Main")
glmnet_res <- glmnet_res %>% filter(!is.na(coef), RR< 129)  %>% mutate(analysis="Sensitivity")

poolRR<-function(d, method="REML"){
  
  d <- d %>% rename(untransformed_estimate=coef, untransformed_se=se)  
  
  fit<-NULL
  try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"))
  if(method=="REML"){
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"))}
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="DL", measure="RR"))}
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="HE", measure="RR"))}
  }
  
  #confint(fit)
  
  est<-data.frame(fit$b, fit$se, fit$I2, fit$QEp)
  colnames(est)<-c("logRR.psi","logSE", "I2","QEp")
  
  est$RR<-exp(est$logRR)
  est$ci.lb<-exp(est$logRR - 1.96 * est$logSE)
  est$ci.ub<-exp(est$logRR + 1.96 * est$logSE)
  est$N <- d$N[1]
  est <- est %>% mutate(study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
  
  return(est)
}

#pool 
glmnet_res_pool <- glmnet_res %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target) %>%
  do(poolRR(.)) %>% mutate(analysis="Sensitivity")

glmnet_res <- bind_rows(glmnet_res, glmnet_res_pool)




glmnet_res <- clean_res(glmnet_res) 
main_res <- clean_res(main_res) 
d <- bind_rows(glmnet_res, main_res)
head(d)

#get differences for primary pooled estimates
d %>% filter(target=="Any pathogen", sample=="any sample type", study=="Pooled") %>% select(analysis, RR, ci.lb, ci.ub)

d %>% group_by(sample,target, study) %>% summarise(n())
d <- d %>% group_by(sample,target, study) %>% filter(n()==2)

d$study <- factor(d$study, levels=rev(unique(d$study)))

 legend_labels = levels(d$sample_cat)[levels(d$sample_cat)!="Any sample"]

                      drop_full_sparse=F
                      ylimits=c(0.125,5)
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies in kitchen" = my_colors[9],
               "Flies in latrine" = my_colors[10],
               "Sparse data" = "grey50")
  
  # if(drop_full_sparse){
  #   mydf <- mydf %>% group_by(target) %>%
  #     filter(n()!=sum(sparse=="yes")) %>% ungroup()
  #   mydf <- mydf %>% group_by(sample) %>%
  #     filter(n()!=sum(sparse=="yes")) %>% ungroup()
  # }
  
  mydf <- d %>% droplevels(.)
  
  Y_breaks=c(.25, .5,1, 2, 4, 8)
  Y_breaks2=c("1/4", "1/2","1", "2", "4", "8")
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  p_prescreen_sens <- ggplot(data = mydf, (aes(x=study, y=RR, group=analysis, color=analysis, shape=analysis))) + 
    geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(#breaks = legend_labels,
      values = c(cbbPalette[2:3],"grey50"), drop = FALSE) +
    scale_shape_manual(values=c(16, 16,16), guide=FALSE)+  
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
      breaks=Y_breaks, 
      trans='log10', 
      labels = Y_breaks2
    ) + 
    coord_flip(ylim=ylimits)+
    labs(color="Analysis") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
  
  p_prescreen_sens

save(list=ls(pattern="p_"), file=here("figures/prescreen_sens_figure.Rdata"))

ggsave(p_prescreen_sens, file = paste0(here::here(),"/figures/pngs/sens_aim1_prescreen.png"), width = 10, height = 6)
