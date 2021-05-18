
rm(list=ls())
source(here::here("0-config.R"))
unadj_RR <- readRDS(here("results/unadjusted_aim1_RR.Rds"))

unadj_RR <- clean_res(unadj_RR)
head(unadj_RR)
table(unadj_RR$sample_cat)

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
 
      est<-data.frame(fit$b, fit$se, fit$I2)
      colnames(est)<-c("logRR.psi","logSE", "I2")
      
      est$RR<-exp(est$logRR)
      est$ci.lb<-exp(est$logRR - 1.96 * est$logSE)
      est$ci.ub<-exp(est$logRR + 1.96 * est$logSE)
      est$N <- d$N[1]
      est <- est %>% mutate(study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
      
  return(est)
}

#pool primary estimates by study


res <- unadj_RR %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target) %>%
  do(poolRR(.)) 

unadj_pool <- bind_rows(unadj_RR, res)
unadj_pool$study <- factor(unadj_pool$study, levels = rev(c(levels(unadj_RR$study),"Pooled")))

saveRDS(unadj_pool, file=here("results/unadjusted_aim1_RR_pooled.Rds"))

