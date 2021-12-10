
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

#check abundance presence
d <- d %>% filter(!is.na(abund)) %>% droplevels(.)
table(d$study, is.na(d$abund))
table(d$sample, is.na(d$abund))
table(d$target, is.na(d$abund))



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
summary(sth$abund)

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


# d <- d %>% mutate(abund=log10(abund)) 
# outcome="diar7d"
# exposure="abund"
# study="Boehm 2016"
# sample="any sample type"
# target= "Avian (GFD)"
# family="binomial"
# Ws=NULL
# 
# 
# any sample type



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

res_haz_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="haz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0
res_haz_adj





#Mark extreme RR for diarrhea outcomes
res_diar$sparse[res_diar$RR >32 | 1/res_diar$RR > 32] <- "yes"
res_diar$coef[res_diar$RR >32 | 1/res_diar$RR > 32] <- 0
res_diar$RR[res_diar$RR >32 | 1/res_diar$RR > 32] <- 1

res_diar_adj$sparse[res_diar$RR >32 | 1/res_diar_adj$RR > 32] <- "yes"
res_diar_adj$coef[res_diar$RR >32 | 1/res_diar_adj$RR > 32] <- 0
res_diar_adj$RR[res_diar$RR >32 | 1/res_diar_adj$RR > 32] <- 1

fullres <- bind_rows( res_haz, res_diar)
fullres_adj <- bind_rows(res_haz_adj, res_diar_adj)








#need to run STH analysis


saveRDS(fullres, file=here("results/unadjusted_aim2_abund_res.Rds"))
saveRDS(fullres_adj, file=here("results/adjusted_aim2_abund_res.Rds"))




