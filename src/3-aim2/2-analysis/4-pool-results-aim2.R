
rm(list=ls())
source(here::here("0-config.R"))
unadj_RR <- readRDS(here("results/unadjusted_aim2_res.Rds"))
adj_RR <- readRDS(here("results/adjusted_aim2_res.Rds"))
unadj_emm <- readRDS(here("results/unadjusted_aim2_emm.Rds"))
adj_emm <- readRDS(here("results/adjusted_aim2_emm.Rds"))
adj_emm_PD <- readRDS(here("results/adjusted_aim2_emm_PD.Rds"))



unadj_RR <- clean_res(unadj_RR) #%>% distinct()
adj_RR <- clean_res(adj_RR) #%>% distinct()
#adj_emm <- clean_res(adj_emm) #%>% distinct()
adj_emm <- clean_res_subgroup(adj_emm) #%>% distinct()
adj_emm_PD <- clean_res_subgroup(adj_emm_PD) #%>% distinct()
head(unadj_RR)
table(unadj_RR$sample_cat)


df <- adj_RR %>% filter(target=="Any STH", Y=="haz")
df

binary_Y =c("diar7d","stunt","wast","underwt")
cont_Y =c("haz","waz","whz")

#pool primary estimates by study
res_RR_unadj <- res_RR_adj <- NULL
res_RR_unadj <- unadj_RR %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>%  
  do(try(poolRR(.)))

res_RR_adj <- adj_RR %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>% 
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

res_emm_bin_adj <- adj_emm %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target, V, Vlevel) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(Y, sample, target, V, Vlevel) %>%
  do(poolRR(.)) %>% mutate(coef=log(RR)) 

res_emm_cont_adj <- adj_emm %>% filter(Y%in%cont_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target, V, Vlevel) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(Y, sample, target, V, Vlevel) %>%
  do(try(pool.cont(.))) 

res_emm_PD_adj <- adj_emm_PD %>% filter(sample_cat!="Sparse data") %>%
  group_by(Y, sample, target, V, Vlevel) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(Y, sample, target, V, Vlevel) %>%
  do(try(pool.cont(.))) 

#add blank rows for subgroups without paired pooled estimates
res_emm_bin_adj_blank <- res_emm_bin_adj %>% group_by(sample,target,V) %>% mutate(N=n()) %>% filter(N==1) %>%
  mutate(Vlevel = -1*(Vlevel-1), coef=NA, logRR.psi=NA,  logSE=NA,I2=NA,QEp=NA,RR=NA, ci.lb=NA, ci.ub=NA, sparse="pooled") %>% subset(., select=-c(N))
res_bin_emm_adj <- bind_rows(res_emm_bin_adj, res_emm_bin_adj_blank)

res_emm_cont_adj_blank <- res_emm_cont_adj %>% group_by(sample,target,V) %>% mutate(N=n()) %>% filter(N==1) %>%
  mutate(Vlevel = -1*(Vlevel-1), coef=NA, logRR.psi=NA,  logSE=NA,I2=NA,QEp=NA,RR=NA, ci.lb=NA, ci.ub=NA, sparse="pooled") %>% subset(., select=-c(N))
res_cont_emm_adj <- bind_rows(res_emm_cont_adj, res_emm_cont_adj_blank)

res_emm_PD_adj_blank <- res_emm_PD_adj %>% group_by(sample,target,V) %>% mutate(N=n()) %>% filter(N==1) %>%
  mutate(Vlevel = -1*(Vlevel-1), coef=NA, logRR.psi=NA,  logSE=NA,I2=NA,QEp=NA,RR=NA, ci.lb=NA, ci.ub=NA, sparse="pooled") %>% subset(., select=-c(N))
res_PD_emm_adj <- bind_rows(res_emm_PD_adj, res_emm_PD_adj_blank)

res_bin_emm_adj%>%filter(V=="wet_CH", sample=="any sample type", target=="Any pathogen")
res_emm_PD_adj%>%filter(V=="wet_CH", sample=="any sample type", target=="Any pathogen")
temp<-adj_emm_PD%>%filter(V=="wet_CH", sample=="any sample type", target=="Any pathogen")
temp$W

unadj_pool <- bind_rows(unadj_RR, res_cont_unadj, res_RR_unadj)
unadj_pool$study <- factor(unadj_pool$study, levels = rev(c(levels(unadj_RR$study),"Pooled")))

adj_pool <- bind_rows(adj_RR, res_cont_adj, res_RR_adj)
adj_pool$study <- factor(adj_pool$study, levels = levels(unadj_pool$study))

emm_pool <- bind_rows(adj_emm, res_cont_emm_adj, res_bin_emm_adj)
emm_pool$study <- factor(emm_pool$study, levels = levels(unadj_pool$study))

emm_pool_PD <- bind_rows(adj_emm_PD,res_PD_emm_adj)
emm_pool_PD$study <- factor(emm_pool_PD$study, levels = levels(unadj_pool$study))


saveRDS(unadj_pool, file=here("results/unadjusted_aim2_pooled.Rds"))
saveRDS(adj_pool, file=here("results/adjusted_aim2_pooled.Rds"))
saveRDS(emm_pool, file=here("results/subgroup_aim2_pooled.Rds"))
saveRDS(emm_pool_PD, file=here("results/subgroup_PD_aim2_pooled.Rds"))




#pool by urban/rural
unique(adj_RR$study)
adj_RR$urban <- ifelse(adj_RR$study%in% c("Holcomb 2021","Capone et al. 2021", " Capone 2022 in prep." ), "Urban", "Rural")
res_urban <- adj_RR %>% group_by(Y, sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% group_by(Y, sample, target, urban) %>%
  do(poolRR(.)) 
res_urban



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

res_urban_comp <- res_urban %>% group_by(Y, sample, target) %>% 
  do(rma_format(., subgroup="urban")) %>% filter(!is.na(pval))
res_urban_comp


saveRDS(list(urban=res_urban, urban_comp=res_urban_comp), file=here("results/subgroup_aim2_RR_pooled.Rds"))




