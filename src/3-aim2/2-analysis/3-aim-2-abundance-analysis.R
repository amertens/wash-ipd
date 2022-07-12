
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
head(d)
d <- droplevels(d)
table(d$study, d$pos)
table(d$study)
table(d$study, d$sample)
table(d$study, d$sample, d$pos)

d <- d %>% filter(sample!="FP", sample!="any sample type") %>% droplevels()
table(d$pos, !is.na(d$abund), d$study)
table(d$pos, d$abund>0, d$study)

df <- d %>% filter(study=="Steinbaum 2019")
summary(df$abund)
table(df$pos, df$abund)

#check abundance presence
d <- d %>% filter(!is.na(abund)) %>% droplevels(.)
table(d$study, is.na(d$abund))
table(d$sample, is.na(d$abund))
table(d$target, is.na(d$abund))
table(d$target, d$diar7d, d$sample, d$study)


table(d$qual)
summary(d$abund)

df <- d %>% filter(study=="Capone 2022 in prep", sample=="Fly", target=="Adenovirus")
summary(df$abund)
summary(log10(df$abund))
length(unique(df$abund))
length(unique(df$abund[df$diar7d==1]))

#Separate STH from MST abundances and add 0.5 to zero counts to allow for log-transformation
table(d$sample, d$target)
sth <- d %>% filter(target %in% c("Any STH","Ascaris","Trichuris") & sample!="Fly")  %>% droplevels(.)
d <- d %>% filter(!(target %in% c("Any STH","Ascaris","Trichuris"))|sample=="Fly")  %>% droplevels(.)

table(d$abund==0)
table(sth$abund==0)
#look at minimum level above 0
min(sth$abund[sth$abund!=0])
#0.07063845
#impute 0.01 for 0
sth$abund[sth$abund==0] <- 0.01

#combine back together
d <- bind_rows(d, sth)

# 1.	Child birth order/parity 
# 2.	Asset-based wealth index 
# 3.	Number of individuals and children in household
# 4.	Household food security 
# 5.	Household electrification and construction, including wall/roof material 
# 6.	Parental age 
# 7.	Parental education 
# 8.	Parental employment 
# a.	Indicator for works in agriculture 
# 9.	Land ownership 

Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#d <- d %>% mutate(abund=log10(abund))
outcome="diar7d"
exposure="abund"
study="Steinbaum 2019"
sample="S"
target= "Ascaris"
family="binomial"
Ws=Wvars
minN_thres = 5
Ws=NULL
forcedW=NULL


res_diar <- d %>% group_by(study, sample, target) %>%
   mutate(abund=log10(abund)) %>%
   do(aim2_glm(., outcome="diar7d", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar$sparse <- ifelse(is.na(res_diar$RR), "yes", "no")
res_diar$RR[is.na(res_diar$RR)] <- 1

res_haz <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="haz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz$sparse <- ifelse(is.na(res_haz$coef), "yes", "no")
res_haz$coef[is.na(res_haz$coef)] <- 0
res_haz


res_diar_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="diar7d", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial")) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar$RR)] <- 1


res_stunt_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="stunt", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial")) 
res_stunt_adj$sparse <- ifelse(is.na(res_stunt_adj$RR), "yes", "no")
res_stunt_adj$RR[is.na(res_stunt_adj$RR)] <- 1

res_wast_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="wast", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial")) 
res_wast_adj$sparse <- ifelse(is.na(res_wast_adj$RR), "yes", "no")
res_wast_adj$RR[is.na(res_wast_adj$RR)] <- 1

res_underwt_adj <- d %>% group_by(study, sample, target) %>%
  filter(!(study=="Capone 2022 in prep" &  sample=="Fly" &  target=="Animal (BacCow)"),
         !(study=="Capone 2022 in prep" &  sample=="Fly" &  target=="Rotavirus")) %>% #filter due to fit error
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="underwt", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial")) 
res_underwt_adj$sparse <- ifelse(is.na(res_underwt_adj$RR), "yes", "no")
res_underwt_adj$RR[is.na(res_underwt_adj$RR)] <- 1


res_haz_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="haz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0
res_haz_adj

res_waz_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="waz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian")) 
res_waz_adj$sparse <- ifelse(is.na(res_waz_adj$coef), "yes", "no")
res_waz_adj$coef[is.na(res_waz_adj$coef)] <- 0
res_waz_adj

res_whz_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="whz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian")) 
res_whz_adj$sparse <- ifelse(is.na(res_whz_adj$coef), "yes", "no")
res_whz_adj$coef[is.na(res_whz_adj$coef)] <- 0
res_whz_adj


#Estimate difference only among ROQ
table(d$qual)
res_diar_roq_adj <- d %>% group_by(study, sample, target) %>%
  filter(qual=="ROQ") %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="diar7d", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial")) 
res_diar_roq_adj$sparse <- ifelse(is.na(res_diar_roq_adj$RR), "yes", "no")
res_diar_roq_adj$RR[is.na(res_diar_roq_adj$RR)] <- 1


res_haz_roq_adj <- d %>% group_by(study, sample, target) %>%
  filter(qual=="ROQ") %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="haz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian")) 
res_haz_roq_adj$sparse <- ifelse(is.na(res_haz_roq_adj$coef), "yes", "no")
res_haz_roq_adj$coef[is.na(res_haz_roq_adj$coef)] <- 0
res_haz_roq_adj




#Mark extreme RR for diarrhea outcomes
res_diar$sparse[res_diar$RR >32 | 1/res_diar$RR > 32] <- "yes"
res_diar$coef[res_diar$RR >32 | 1/res_diar$RR > 32] <- 0
res_diar$RR[res_diar$RR >32 | 1/res_diar$RR > 32] <- 1

res_diar_adj$sparse[res_diar_adj$RR >32 | 1/res_diar_adj$RR > 32] <- "yes"
res_diar_adj$coef[res_diar_adj$RR >32 | 1/res_diar_adj$RR > 32] <- 0
res_diar_adj$RR[res_diar_adj$RR >32 | 1/res_diar_adj$RR > 32] <- 1=

res_diar_roq_adj$sparse[res_diar_roq_adj$RR >32 | 1/res_diar_roq_adj$RR > 32] <- "yes"
res_diar_roq_adj$coef[res_diar_roq_adj$RR >32 | 1/res_diar_roq_adj$RR > 32] <- 0
res_diar_roq_adj$RR[res_diar_roq_adj$RR >32 | 1/res_diar_roq_adj$RR > 32] <- 1

fullres <- bind_rows( res_haz, res_diar)
fullres_adj <- bind_rows(res_haz_adj,res_waz_adj,res_whz_adj, res_diar_adj, res_stunt_adj, res_wast_adj, res_underwt_adj)
fullres_roq <- bind_rows(res_haz_roq_adj, res_diar_roq_adj)








#need to run STH analysis


saveRDS(fullres, file=here("results/unadjusted_aim2_abund_res.Rds"))
saveRDS(fullres_adj, file=here("results/adjusted_aim2_abund_res.Rds"))
saveRDS(fullres_roq, file=here("results/adjusted_aim2_abund_ROQ_res.Rds"))




