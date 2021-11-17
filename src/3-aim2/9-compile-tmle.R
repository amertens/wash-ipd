

rm(list=ls())
source(here::here("0-config.R"))


res_diar <- readRDS(here("results/tmle_aim2_res_diarlist.Rds"))

res_haz <- readRDS(here("results/tmle_aim2_res_hazlist.Rds"))

res_diar_full <- NULL
for(i in 1:nrow(res_diar)){
  res_diar_full <- try(bind_rows(res_diar_full, res_diar$res[[i]]))
}

res_haz_full <- NULL
for(i in 1:nrow(res_haz)){
  res_haz_full <- try(bind_rows(res_haz_full, res_haz$res[[i]]))
}



saveRDS(res_diar_full, file=here("results/tmle_aim2_res_diar.Rds"))
saveRDS(res_haz_full, file=here("results/tmle_aim2_res_haz.Rds"))



poolRR_tmle<-function(d, method="REML"){
  
  if(nrow(d)>0){
    d$se <- (log(d$ci.ub) - d$coef)/1.96
    
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
    
  }else{
    est <- data.frame(Y=NA,study=NA, sparse=NA, sample_type=NA, sample_cat=NA, target_f=NA)
  }
  
  return(est)
}


pool.cont_tmle<-function(d, method="REML"){
  
  d$se <- (d$ci.ub - d$coef)/1.96
  
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

res_diar <- res_diar_full %>% mutate(sparse="no")
res_haz <- res_haz_full %>% mutate(sparse="no")

res_RR_adj <- res_diar %>% filter(!is.na(coef)) %>%
  group_by(sample, target) %>%
  mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(poolRR_tmle(.)) 

res_RR_adj <- bind_rows(res_RR_adj, res_diar) %>%
  mutate(Y="diar7d")

res_cont_adj <- res_haz %>% filter(!is.na(coef)) %>%
  group_by(sample, target) %>% 
  mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(try(pool.cont_tmle(.))) 

res_cont_adj <- bind_rows(res_cont_adj, res_haz) %>%
  mutate(Y="haz")

adj_RR <- bind_rows(res_RR_adj, res_cont_adj)
adj_RR <- clean_res(adj_RR) 


saveRDS(adj_RR, file=here("results/tmle_aim2_res.Rds"))






