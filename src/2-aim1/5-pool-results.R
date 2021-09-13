
rm(list=ls())
source(here::here("0-config.R"))
unadj_RR <- readRDS(here("results/unadjusted_aim1_RR.Rds"))
adj_RR <- readRDS(here("results/adjusted_aim1_RR.Rds"))
emm_RR <- readRDS(file=here("results/adjusted_aim1_emm.Rds")) 

#clean results
unadj_RR <- clean_res(unadj_RR)
adj_RR <- clean_res(adj_RR)
#emm_RR <- clean_res(emm_RR)
head(unadj_RR)
table(unadj_RR$sample_cat)

#make sure zoonotic origin targets have both zoo and not zoo in a study
#-note changed based on what Ayse said
zoo_RR <- adj_RR %>%
   filter(target %in% c("Any zoonotic","Any non-zoonotic")) #%>%
  # group_by(sample, study) %>% mutate(N=n()) %>%
  # filter(N==2)
unadj_RR <- unadj_RR %>% filter(!(target %in% c("Any zoonotic","Any non-zoonotic")))
adj_RR <- adj_RR %>% filter(!(target %in% c("Any zoonotic","Any non-zoonotic")))
adj_RR <- bind_rows(adj_RR, zoo_RR)

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

res_emm <- emm_RR %>% group_by(sample, target, V, Vlevel) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target, V, Vlevel) %>%
  do(poolRR(.)) %>% mutate(coef=log(RR)) 

#add blank rows for subgroups without paired pooled estimates
res_emm_blank <- res_emm %>% group_by(sample,target,V) %>% mutate(N=n()) %>% filter(N==1) %>%
  mutate(Vlevel = -1*(Vlevel-1), coef=NA, logRR.psi=NA,  logSE=NA,I2=NA,QEp=NA,RR=NA, ci.lb=NA, ci.ub=NA, sparse="pooled") %>% subset(., select=-c(N))
res_emm <- bind_rows(res_emm, res_emm_blank)

unadj_pool <- bind_rows(unadj_RR, res_unadj)
unadj_pool$study <- factor(unadj_pool$study, levels = rev(c(levels(unadj_RR$study),"Pooled")))

adj_pool <- bind_rows(adj_RR, res_adj)
adj_pool$study <- factor(adj_pool$study, levels = levels(unadj_pool$study))

emm_pool <- bind_rows(emm_RR, res_emm)
emm_pool$study <- factor(emm_pool$study, levels = levels(unadj_pool$study))

saveRDS(unadj_pool, file=here("results/unadjusted_aim1_RR_pooled.Rds"))
saveRDS(adj_pool, file=here("results/adjusted_aim1_RR_pooled.Rds"))
saveRDS(emm_pool, file=here("results/adjusted_aim1_emm_pooled.Rds"))


#pool by urban/rural
adj_RR$urban <- ifelse(adj_RR$study%in% c("Holcomb 2020","Capone et al. 2021", " Capone 2021 in prep." ), "Urban", "Rural")
res_urban <- adj_RR %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target, urban) %>%
  do(poolRR(.)) 
res_urban

#pool by study design
adj_RR$trial <- ifelse(adj_RR$study%in% c("Holcomb 2020","Capone et al. 2021", " Capone 2021 in prep." )| adj_RR$study=="Reese 2017", "Matched Cohort", "Trial")
res_trial <- adj_RR %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target, trial) %>%
  do(poolRR(.)) 
res_trial

#pool by high/low uptake:
# WBB: High uptake: https://pubmed.ncbi.nlm.nih.gov/29976251/ (observed adult use of a hygienic latrine was high (94-97% of events) while child sanitation practices were moderate (37-54%))
# WBK: A high proportion of households (75%) had improved latrine access, which remained stable in year 1 and year 2 in households in the sanitation groups, increasing by more than 50% compared with the active control group.
# MapSan: High uptake according to http://mapsan.gatech.edu/Fidelity.pdf, 
# GV: High (Access to a household improved toilet was almost five times higher in intervention villages than in control villages (85.0% v. 17.7%))
# Odisha: 28% increase in functional latrine compared to control (38% to 10%)
adj_RR$uptake <- ifelse(adj_RR$study%in% c("Odagiri 2016"), "Low", "High")
res_uptake <- adj_RR %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target, uptake) %>%
  do(poolRR(.)) 
res_uptake

#Check if unadjusted are different
unadj_RR$trial <- ifelse(unadj_RR$study%in% c("Holcomb 2020","Capone et al. 2021", " Capone 2021 in prep." )| unadj_RR$study=="Reese 2017", "Matched Cohort", "Trial")
res_trial_unadj <- unadj_RR %>% group_by(sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(sample, target, trial) %>%
  do(poolRR(.)) 
res_trial_unadj

#Compare if estimates are different
rma_format <- function(d, subgroup){
  
  d$subgroup <- factor(d[[subgroup]])
  if(length(levels(d$subgroup))>1){
    fit <- rma(logRR.psi, sei=logSE, mods = ~ subgroup, method="FE", data=d)
    est<-data.frame(fit$b, fit$se, fit$I2)
    colnames(est)<-c("logRR.psi","logSE", "I2")
    
    est$RR<-exp(est$logRR)
    est$ci.lb<-exp(est$logRR - 1.96 * est$logSE)
    est$ci.ub<-exp(est$logRR + 1.96 * est$logSE)
    est$pval <- fit$pval
    est$N <- d$N[1]
    est$subgroup=subgroup
    est$ref=levels(d$subgroup)[1]
    est <- est %>% mutate(study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
    est <- est[2,]
  }else{
    est <- data.frame(
      logRR.psi=NA, logSE=NA, I2=NA, RR=NA,  ci.lb=NA,  ci.ub=NA,    pval=NA, N=NA,  subgroup=NA,  ref=NA, 
      study=NA,  sparse=NA,  sample_type=NA,  sample_cat=NA,  target_f=NA
    )
  }

  return(est)
}

res_urban_comp <- res_urban %>% group_by(sample, target) %>% 
  do(rma_format(., subgroup="urban"))
res_urban_comp

res_trial_comp <- res_trial %>% group_by(sample, target) %>% 
  do(rma_format(., subgroup="trial"))
res_trial_comp

res_uptake_comp <- res_uptake %>% group_by(sample, target) %>% 
  do(rma_format(., subgroup="uptake"))
res_uptake_comp

res_trial_comp_unadj <- res_trial_unadj %>% group_by(sample, target) %>% 
  do(rma_format(., subgroup="trial"))
res_trial_comp_unadj


saveRDS(list(urban=res_urban, urban_comp=res_urban_comp, trial=res_trial, trial_comp=res_trial_comp), file=here("results/subgroup_aim1_RR_pooled.Rds"))


