
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)

table(d$study, !is.na(d$animals))
table(d$study, d$animals)
table(d$study, d$wet)

#drop baseline observations and food because only in one study and no estimates, and odigari as it has no variation
table(is.na(d$round))
d <- d %>% filter(round!="bl", sample!="FP", trial!="Odisha") %>% droplevels()
Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof", "elec","dadagri","landown", "momedu", "momage")         

table(d$target)
d_animals <- d %>% filter(!is.na(animals), target %in% c("Any zoonotic","Any non-zoonotic")) %>%
  mutate(tr=animals) %>% droplevels()

table(d_animals$animals, d_animals$pos, d_animals$target)
dzoo <- d_animals %>% filter(target=="Any zoonotic")
dnozoo <- d_animals %>% filter(target=="Any non-zoonotic")
prop.table(table(dzoo$animals, dzoo$pos ),1)
prop.table(table(dnozoo$animals, dnozoo$pos ),1)

prop.table(table(d_animals$target, d_animals$pos ),1)*100

animal_zoonotic_tab <- round(prop.table(table(dzoo$animals, dzoo$pos ),1)*100,1)



#-----------------------------------
# Adjusted RR
#-----------------------------------
res_animals_adj <- d_animals %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(.,  outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1],  Ws=Wvars, family="binomial"))
res_animals_adj %>% filter(!is.na(RR))



#pooling function
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

res_pool <- res_animals_adj %>% filter(!is.na(se)) %>% 
  group_by(sample, study) %>% 
  mutate(N=n()) %>% filter(N==2) %>%
  group_by(sample, target) %>% 
  mutate(N=n()) %>%
  filter(N>=3) %>% group_by(sample, target) %>%
  do(poolRR(.)) 
res_pool

saveRDS(res_pool, file=here("results/adjusted_zoonotic_animals.Rds"))




