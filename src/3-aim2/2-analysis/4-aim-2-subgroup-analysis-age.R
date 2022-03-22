
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
d$age[d$study %in% c("Reese 2017","Capone 2021","Capone 2022 in prep","Holcomb 2020")] <- d$age[d$study %in% c("Capone 2021","Capone 2022 in prep","Holcomb 2020")] *30.4167
d$age_anthro[d$study=="Reese 2017"] <- d$age_anthro[d$study=="Reese 2017"] *30.4167
d %>% group_by(study) %>% summarise(mean(age), mean(age_anthro))

#demarcate child age
summary(d$age/365)
summary(d$age_anthro/365)

#get ages category (immobile vs. crawling vs. walking pre-school-age vs. school-age, Aim 2)
#using median ages here
#https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/motor-development-milestones/mm_percentiles_table.pdf?sfvrsn=81f3b60b_5
#school age >5

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

outcome="diar7d"
exposure="pos"
study="Holcomb 2020"
sample="any sample type"
target= "Any MST"
family="binomial"
forcedW=c("age", "hhwealth")
Ws=Wvars


# fullres <- NULL
# res_diar <- d %>% group_by(study, sample, target, agecat) %>%
#    do(aim2_glm(., outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
# res_diar$sparse <- ifelse(is.na(res_diar$RR), "yes", "no")
# res_diar$RR[is.na(res_diar$RR)] <- 1
# fullres <- bind_rows(fullres, res_diar)
# 
# # res_stunt<- d %>% group_by(study, sample, target, agecat_anthro) %>%
# #   do(aim2_glm(., outcome="stunt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
# # res_stunt$sparse <- ifelse(is.na(res_stunt$RR), "yes", "no")
# # res_stunt$RR[is.na(res_stunt$RR)] <- 1
# # fullres <- bind_rows(fullres, res_stunt)
# # 
# # res_wast<- d %>% group_by(study, sample, target, agecat_anthro) %>%
# #   do(aim2_glm(., outcome="wast", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
# # res_wast$sparse <- ifelse(is.na(res_wast$RR), "yes", "no")
# # res_wast$RR[is.na(res_wast$RR)] <- 1
# # fullres <- bind_rows(fullres, res_wast)
# # 
# # res_underwt <- d %>% group_by(study, sample, target, agecat_anthro) %>%
# #   do(aim2_glm(., outcome="underwt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
# # res_underwt$sparse <- ifelse(is.na(res_underwt$RR), "yes", "no")
# # res_underwt$RR[is.na(res_underwt$RR)] <- 1
# # fullres <- bind_rows(fullres, res_underwt)
# 
# res_haz <- d %>% group_by(study, sample, target, agecat_anthro) %>%
#    do(aim2_glm(., outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
# res_haz$sparse <- ifelse(is.na(res_haz$coef), "yes", "no")
# res_haz$coef[is.na(res_haz$coef)] <- 0
# res_haz
# fullres <- bind_rows(fullres, res_haz)
# 
# # res_whz <- d %>% group_by(study, sample, target, agecat_anthro) %>%
# #   do(aim2_glm(., outcome="whz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
# # res_whz$sparse <- ifelse(is.na(res_whz$coef), "yes", "no")
# # res_whz$coef[is.na(res_whz$coef)] <- 0
# # res_whz
# # fullres <- bind_rows(fullres, res_whz)
# # 
# # res_waz <- d %>% group_by(study, sample, target, agecat_anthro) %>%
# #   do(aim2_glm(., outcome="waz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
# # res_waz$sparse <- ifelse(is.na(res_waz$coef), "yes", "no")
# # res_waz$coef[is.na(res_waz$coef)] <- 0
# # res_waz
# # fullres <- bind_rows(fullres, res_waz)





fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target, agecat) %>%
  do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar_adj$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_diar_adj)

# res_stunt_adj <- d %>% group_by(study, sample, target, agecat_anthro) %>%
#   do(aim2_glm(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="stunt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
# res_stunt_adj$sparse <- ifelse(is.na(res_stunt_adj$RR), "yes", "no")
# res_stunt_adj$RR[is.na(res_stunt_adj$RR)] <- 1
# fullres_adj <- bind_rows(fullres_adj, res_stunt_adj)
# 
# res_wast_adj <- d %>% group_by(study, sample, target, agecat_anthro) %>%
#   do(aim2_glm(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="wast", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
# res_wast_adj$sparse <- ifelse(is.na(res_wast_adj$RR), "yes", "no")
# res_wast_adj$RR[is.na(res_wast_adj$RR)] <- 1
# fullres_adj <- bind_rows(fullres_adj, res_wast_adj)
# 
# res_underwt_adj <- d %>% group_by(study, sample, target, agecat_anthro) %>%
#   do(aim2_glm(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="underwt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
# res_underwt_adj$sparse <- ifelse(is.na(res_underwt_adj$RR), "yes", "no")
# res_underwt_adj$RR[is.na(res_underwt_adj$RR)] <- 1
# fullres_adj <- bind_rows(fullres_adj, res_underwt_adj)


res_haz_adj <- d %>% group_by(study, sample, target, agecat_anthro) %>%
  do(aim2_glm(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0
res_haz_adj
fullres_adj <- bind_rows(fullres_adj, res_haz_adj)

# res_waz_adj <- d %>% group_by(study, sample, target, agecat_anthro) %>%
#   do(aim2_glm(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="waz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
# res_waz_adj$sparse <- ifelse(is.na(res_waz_adj$coef), "yes", "no")
# res_waz_adj$coef[is.na(res_waz_adj$coef)] <- 0
# res_waz_adj
# fullres_adj <- bind_rows(fullres_adj, res_waz_adj)
# 
# res_whz_adj <- d %>% group_by(study, sample, target, agecat_anthro) %>%
#   do(aim2_glm(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="whz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
# res_whz_adj$sparse <- ifelse(is.na(res_whz_adj$coef), "yes", "no")
# res_whz_adj$coef[is.na(res_whz_adj$coef)] <- 0
# res_whz_adj
# fullres_adj <- bind_rows(fullres_adj, res_whz_adj)


# Primary outcomes for this aim include: 
#   Prevalence of diarrhea and length-for-age z-scores (LAZ).
# Secondary outcomes include z-scores for weight-for-age (WAZ), 
# weight-for-length (WLZ), head circumference and middle-upper-arm-circumference,
# prevalence of stunting, wasting and underweight, prevalence and intensity of infection with 
# specific enteropathogens, and prevalence of respiratory infections. We will address Aim 2 
# by estimating PRs and PDs for the binary health outcomes and mean differences for the 
# continuous health outcomes for individuals with vs. without exposure to pathogens and 
# MST markers in environmental samples. The primary outcomes of Aim 1 (prevalence of any enteropathogen, 
# and any general, human or animal MST markers 
# in environmental samples) will be used as the exposure variables for this aim. 

# For the LAZ, stunting and head circumference outcomes,
# we will consider all environmental samples collected over the child's 
# lifetime prior to the anthropometry measurement. For the other outcomes, 
# we will only consider environmental samples collected up to three months 
# before the measurement of the health outcome. 


#saveRDS(fullres, file=here("results/unadjusted_aim2_res_subgroup_age.Rds"))
saveRDS(fullres_adj, file=here("results/adjusted_aim2_res_subgroup_age.Rds"))




