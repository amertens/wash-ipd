
rm(list=ls())
source(here::here("0-config.R"))
unadj_RR <- readRDS(here("results/unadjusted_aim2_res.Rds"))
adj_RR <- readRDS(here("results/adjusted_aim2_res.Rds"))

unadj_RR <- clean_res(unadj_RR) #%>% distinct()
adj_RR <- clean_res(adj_RR) #%>% distinct()
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
  est <- est %>% mutate(Y=d$Y[1],study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
  
  return(est)
}


pool.cont<-function(d, method="REML"){
    
    fit<-NULL
    try(fit<-rma(yi=coef, sei=se, data=d, method=method, measure="GEN"))
    if(method=="REML"){
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="ML", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="DL", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="HE", measure="GEN"))}
    }
    est<-data.frame(fit$b, fit$ci.lb, fit$ci.ub, fit$I2)
    colnames(est)<-c("coef","ci.lb","ci.ub", "I2")
 
    est$N <- d$N[1]
    est <- est %>% mutate(Y=d$Y[1],study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
    
  rownames(est) <- NULL
  return(est)
}


binary_Y =c("diar7d","stunt","wast","underwt")
cont_Y =c("haz","waz","whz","underwt")

#pool primary estimates by study
res_RR_unadj <- res_RR_adj <- NULL
res_RR_unadj <- unadj_RR %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>%  
  do(poolRR(.)) 

res_RR_adj <- adj_RR %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% 
  do(poolRR(.)) 

res_cont_unadj <- unadj_RR %>% filter(Y%in%cont_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(try(pool.cont(.))) 

#adj_RR <- adj_RR %>% filter(Y=="haz")
res_cont_adj <- adj_RR %>% filter(Y%in%cont_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(try(pool.cont(.))) 


unadj_pool <- bind_rows(unadj_RR, res_cont_unadj, res_RR_unadj)
unadj_pool$study <- factor(unadj_pool$study, levels = rev(c(levels(unadj_RR$study),"Pooled")))

adj_pool <- bind_rows(adj_RR, res_cont_adj, res_RR_adj)
adj_pool$study <- factor(adj_pool$study, levels = levels(unadj_pool$study))

#adj_pool<-adj_pool %>% filter(Y=="haz", sample=="any sample type", target=="Any pathogen")

saveRDS(unadj_pool, file=here("results/unadjusted_aim2_pooled.Rds"))
saveRDS(adj_pool, file=here("results/adjusted_aim2_pooled.Rds"))

