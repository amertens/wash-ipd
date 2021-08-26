#test.meta = rma.mv(d, Var.d., random = list(~1|Study, ~1|Order.class), data=data). 



rm(list=ls())
source(here::here("0-config.R"))
adj_RR <- readRDS(here("results/adjusted_aim1_RR.Rds"))

#clean results
adj_RR <- clean_res(adj_RR)
adj_RR <- adj_RR %>% filter(target =="Any pathogen",sample=="any sample type")
adj_RR <- adj_RR %>% mutate(trial=as.character(study))
adj_RR$trial[adj_RR$study %in% c("Boehm 2016",  "Fuhrmeister 2020",  "Kwong 2021")] <- "WBB"

d <- adj_RR %>% mutate(untransformed_var=se^2/n)
method="REML"

d <- d %>% rename(untransformed_estimate=coef, untransformed_se=se)  
fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR")
test.meta = rma.mv(yi=untransformed_estimate, V=untransformed_se^2, random = list(~1|trial), data=d, method=method)
test.meta2 = rma.mv(yi=untransformed_estimate, V=untransformed_var, random = list(~1|trial), data=d, method=method)


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
res_unadj <- unadj_RR %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target) %>%
  do(poolRR(.)) 

res_adj <- adj_RR %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target) %>%
  do(poolRR(.)) 
