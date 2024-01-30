
rm(list=ls())
source(here::here("0-config.R"))
source(paste0(here::here(),"/src/3-aim2/2-analysis/0-pathogen-function.R"))


#extra analysis for Ollie

d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))


#collapse to avoid seperate env. data
dim(d)
d <- d %>% distinct(study, dataid,  hhid, clusterid, childid, childNo, aged_pathogen,   ch_pos_giardia,     
                    ch_pos_entamoeba,     
                    ch_pos_crypto,        
                    ch_qpcr_pos_ascaris,
                    ch_qpcr_pos_trichuris, 
                    ch_pos_ascaris,   
                    ch_pos_trichuris,  
                    ch_pos_giardia_EE,        
                    ch_pos_entamoeba_EE,    
                    ch_pos_crypto_EE,        
                    ch_pos_ascaris_EE,   
                    ch_pos_trichuris_EE,  
                    ch_pos_adenovirus,    
                    ch_pos_norovirus,      
                    ch_pos_rotavirus,      
                    ch_pos_cdiff,       
                    ch_pos_campylobacter,  
                    ch_pos_salmonella,     
                    ch_pos_shigella,    
                    ch_pos_cholera,     
                    ch_pos_yersinia,        
                    ch_pos_path_ecoli, .keep_all = T)
dim(d)

#create aggregate values:
d <- d %>% 
  rowwise() %>% 
  mutate(any_pathogen = 1*(sum(
  ch_pos_giardia,     
  ch_pos_entamoeba,     
  ch_pos_crypto,        
  ch_qpcr_pos_ascaris,
  ch_qpcr_pos_trichuris, 
  ch_pos_ascaris,   
  ch_pos_trichuris,  
  ch_pos_giardia_EE,        
  ch_pos_entamoeba_EE,    
  ch_pos_crypto_EE,        
  ch_pos_ascaris_EE,   
  ch_pos_trichuris_EE,  
   ch_pos_adenovirus,    
  ch_pos_norovirus,      
  ch_pos_rotavirus,      
  ch_pos_cdiff,       
   ch_pos_campylobacter,  
   ch_pos_salmonella,     
   ch_pos_shigella,    
   ch_pos_cholera,     
  ch_pos_yersinia,        
  ch_pos_path_ecoli, na.rm=TRUE)>1))
table(d$any_pathogen)
table(d$study, d$any_pathogen)

d$sample<-'NA'
d$target<-'NA'


#Covariate list
d$age[!is.na(d$aged_pathogen)] <- d$aged_pathogen[!is.na(d$aged_pathogen)]
Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#Run analysis
res <- d %>% group_by(study) %>%
  do(aim1_glm(., outcome="any_pathogen", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res$sparse <- ifelse(is.na(res$RR), "yes", "no")
res$RR[is.na(res$RR)] <- 1


res_adj <- d %>% 
  group_by(study) %>%
  do(aim1_glm(., outcome="any_pathogen", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial"))
res_adj$sparse <- ifelse(is.na(res_adj$RR), "yes", "no")
res_adj$RR[is.na(res_adj$RR)] <- 1


table(res$target)
table(res$sample)

#pool estimates
poolRR<-function(d, method="REML"){
  
  d <- d %>% rename(untransformed_estimate=coef, untransformed_se=se)  
  
  fit<-NULL
  try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"))
  if(method=="REML"){
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"))}
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="DL", measure="RR"))}
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="HE", measure="RR"))}
  }
  
  est<-data.frame(fit$b, fit$se, fit$I2, fit$QEp)
  colnames(est)<-c("logRR.psi","logSE", "I2","QEp")
  
  est$RR<-exp(est$logRR)
  est$ci.lb<-exp(est$logRR - 1.96 * est$logSE)
  est$ci.ub<-exp(est$logRR + 1.96 * est$logSE)
  est$N <- d$N[1]
  est <- est %>% mutate(study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
  
  return(est)
}

#pool primary estimates by study
res <- bind_rows(res, poolRR(res))
res_adj <- bind_rows(res_adj, poolRR(res_adj))

saveRDS(res, file=here("results/unadjusted_aim1_child_pathogens.Rds"))
saveRDS(res_adj, file=here("results/adjusted_aim1_child_pathogens.Rds"))



#plots
base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F,
                      lab_width=10, title='',
                      # Y_breaks=c(.25, .5,1, 2, 4, 8),
                      # Y_labs=c("1/4", "1/2","1", "2", "4", "8")){
                      Y_range=c(.25, 4)){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies" = my_colors[9],
               # "Flies in kitchen" = my_colors[9],
               # "Flies in latrine" = my_colors[10],
               "Sparse data" = "grey50")
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% mutate(sparse=factor(sparse, levels=c("no","yes","pooled"))) %>%
    droplevels(.)
  
  
  shapes <- c("no" = 16,
              "yes" = 13,
              "pooled"  = 18)
  
  axis_range <- range(Y_range)
  axis_range[1] <- axis_range[1]/1.1
  axis_range[2] <- axis_range[2]*1.1
  
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=sparse))) + 
    geom_point(size=2, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.2, size = 0.75) +
    scale_color_manual(breaks = legend_labels, guide = guide_legend(),
                       values = colours, drop = FALSE) +
    scale_shape_manual(values=shapes, 
                       #guide = guide_legend(),
                       guide="none", 
                       drop = FALSE) + 
    #geom_text(aes(y=RR, label=est), color="black", vjust = -0.8, hjust = -0.1, size=1.5) +
    geom_hline(yintercept = 1, linetype="dashed") +
    #facet_grid(target_f~sample_type,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = lab_width, multi_line = TRUE)) +
    scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
      breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
      trans='log10', 
      labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
    ) + coord_flip(ylim=axis_range)+
    guides(color=guide_legend(title="Sample type", nrow=2,byrow=TRUE)#, 
           #shape=guide_legend(title="Sample type", nrow=2,byrow=TRUE)
    ) + 
    ggtitle(title) +
    xlab("") + ylab("Prevalence ratio (Intervention vs. control)") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
}



#---------------------------------------------------------------
#  figures
#---------------------------------------------------------------

res <- clean_res(res)
res_adj <- clean_res(res_adj)
sample_cats = levels(res_adj$sample_cat)
res_adj$study =relevel(factor(res_adj$study), ref="Pooled")

p_unadj_1 <- res %>% filter(!is.na(coef)|sparse=='pooled') %>%
  base_plot(drop_full_sparse=T, Y_range=c(0.25,4))
p_unadj_1

p_adj_1 <- res_adj %>% filter(!is.na(coef)|sparse=='pooled') %>%
  base_plot(drop_full_sparse=T, Y_range=c(0.25,4))
p_adj_1<-p_adj_1 + geom_vline(xintercept = 1.5, linetype='dashed')


ggsave(filename=here("figures/wash-ipd-intervention-child-pathogens.pdf"),
       plot = p_adj_1,device='pdf',width=7.2,height=5.6)




