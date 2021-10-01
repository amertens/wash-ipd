
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
head(d)
d <- droplevels(d)
table(d$study, d$pos)
table(d$study)
table(d$study, d$sample)
table(d$study, d$sample, d$pos)

d <- d %>% filter(sample!="FP") %>% droplevels()

#Separate STH from MST abundances
d <- d %>% filter(!is.na(abund)) %>% droplevels(.)

table(d$sample, d$target)
sth <- d %>% filter(target %in% c("Any STH","Ascaris","Trichuris") & sample!="Fly")  %>% droplevels(.)
d <- d %>% filter(!(target %in% c("Any STH","Ascaris","Trichuris"))|sample=="Fly")  %>% droplevels(.)


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


#-----------------------------------
# Unadjusted RR
#-----------------------------------

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



fullres <- NULL
res_diar <- d %>% group_by(study, sample, target) %>%
   mutate(abund=log10(abund)) %>%
   do(aim2_glm(., outcome="diar7d", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar$sparse <- ifelse(is.na(res_diar$RR), "yes", "no")
res_diar$RR[is.na(res_diar$RR)] <- 1
fullres <- bind_rows(fullres, res_diar)

res_haz <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="haz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz$sparse <- ifelse(is.na(res_haz$coef), "yes", "no")
res_haz$coef[is.na(res_haz$coef)] <- 0
res_haz
fullres <- bind_rows(fullres, res_haz)


fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="diar7d", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial")) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_diar_adj)

res_haz_adj <- d %>% group_by(study, sample, target) %>%
  mutate(abund=log10(abund)) %>%
  do(aim2_glm(., outcome="haz", exposure="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0
res_haz_adj
fullres <- bind_rows(fullres_adj, res_haz_adj)



saveRDS(fullres, file=here("results/unadjusted_aim2_abund_res.Rds"))
saveRDS(fullres_adj, file=here("results/adjusted_aim2_abund_res.Rds"))




