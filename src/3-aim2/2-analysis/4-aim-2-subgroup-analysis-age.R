
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))

head(d)
d <- droplevels(d)
table(d$study, d$pos)
table(d$study)
table(d$study, d$hhwealth)
for(i in unique(d$study)){
  print(summary(d$hhwealth_cont[d$study==i]))
}
table(d$study, d$sample, d$pos)

d <- d %>% filter(sample!="FP") %>% droplevels()


#convert all ages to days
d %>% group_by(study) %>% summarise(mean(age), mean(age_anthro))
d$age[d$study=="Odagiri 2016"] <- d$age[d$study=="Odagiri 2016"] *364
d$age_anthro[d$study=="Odagiri 2016"] <- d$age_anthro[d$study=="Odagiri 2016"] *364
d$age[d$study %in% c("Reese 2017","Capone 2021","Capone 2022 in prep","Holcomb 2021")] <- d$age[d$study %in% c("Capone 2021","Capone 2022 in prep","Holcomb 2021")] *30.4167
d$age_anthro[d$study=="Reese 2017"] <- d$age_anthro[d$study=="Reese 2017"] *30.4167
d %>% group_by(study) %>% summarise(mean(age), mean(age_anthro))

#demarcate child age
summary(d$age/365)
summary(d$age_anthro/365)

#get ages category (immobile vs. crawling vs. walking pre-school-age vs. school-age, Aim 2)
#using median ages here
#https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/motor-development-milestones/mm_percentiles_table.pdf?sfvrsn=81f3b60b_5
#school age >5

d$agecat <- cut(d$age, breaks = c(0, 254, 365, 5*12*30.467, 100000))
table(d$agecat)

d$agecat <- cut(d$age, breaks = c(0, 254, 365, 5*12*30.467, 100000), labels =c("immobile", "crawling", "walking", "school-age"))
table(d$agecat)
table(d$study, d$agecat)

d$agecat_anthro <- cut(d$age_anthro, breaks = c(0, 254, 365, 5*12*30.467, 100000), labels =c("immobile", "crawling", "walking", "school-age"))
table(d$agecat_anthro)
table(d$study, d$agecat_anthro)


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

Wvars = c("sex","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#-----------------------------------
# Unadjusted RR
#-----------------------------------
# 
# outcome="haz"
# exposure="pos"
# study="Steinbaum 2019"
# sample="any sample type"
# target= "Any pathogen"
# family="gaussian"
# forcedW=c("age", "hhwealth")
# Ws=Wvars
# Vvar="agecat_anthro"
# minN_thres = 1
# temp <- aim2_subgroup_age_lrtest(d, Ws = Wvars, Vvar="agecat_anthro", forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=study, sample=sample, target=target, family="gaussian") 


fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target, agecat) %>%
  do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar_adj$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_diar_adj)

#get interaction P-value
res_diar_adj_lrtest <- d %>% group_by(study, sample, target) %>%
  do(aim2_subgroup_age_lrtest(., Ws = Wvars, Vvar="agecat", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
fullres_adj <- left_join(fullres_adj, res_diar_adj_lrtest, by=c("study","sample","target"))


res_haz_adj <- d %>% group_by(study, sample, target, agecat_anthro) %>%
  do(aim2_glm(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0

res_haz_adj_lrtest <- d %>% group_by(study, sample, target) %>%
  do(aim2_subgroup_age_lrtest(., Ws = Wvars, Vvar="agecat_anthro", forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz_adj <- left_join(res_haz_adj, res_haz_adj_lrtest, by=c("study","sample","target"))
fullres_adj <- bind_rows(fullres_adj, res_haz_adj)


temp <- aim2_subgroup_age_lrtest(d, Ws = Wvars, Vvar="agecat_anthro", forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=d$study[1], sample=d$sample[1], target=d$target[1], family="gaussian") 


fullres_adj_PD <- NULL
res_diar_adj_PD <- d %>% group_by(study, sample, target, agecat) %>%
  do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_diar_adj_PD$sparse <- ifelse(is.na(res_diar_adj_PD$coef), "yes", "no")
res_diar_adj_PD$coef[is.na(res_diar_adj_PD$coef)] <- 0
fullres_adj_PD <- bind_rows(fullres_adj_PD, res_diar_adj_PD)

res_diar_adj_lrtest_PD <- d %>% group_by(study, sample, target) %>%
  do(aim2_subgroup_age_lrtest(., Ws = Wvars, Vvar="agecat", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
fullres_adj_PD <- left_join(fullres_adj_PD, res_diar_adj_lrtest_PD, by=c("study","sample","target"))


#saveRDS(fullres, file=here("results/unadjusted_aim2_res_subgroup_age.Rds"))
saveRDS(fullres_adj, file=here("results/adjusted_aim2_res_subgroup_age.Rds"))
saveRDS(fullres_adj_PD, file=here("results/adjusted_aim2_res_subgroup_age_PD.Rds"))




