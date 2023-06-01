
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



outcome="haz"
exposure="pos"
study="Reese 2017"
sample="W"
target= "Shigella"
family="gaussian"
forcedW=c("age", "hhwealth")
Ws=Wvars
overwrite=F

#d <- d %>% filter(study=="Holcomb 2021", sample=="any sample type", target=="Any MST")



fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target) %>%
  do(res=aim2_tmle(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos",
                   study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", overwrite=T)) 
saveRDS(res_diar_adj, file=here("results/tmle_aim2_res_diarlist.Rds"))


# res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
# res_diar_adj$RR[is.na(res_diar_adj$RR)] <- 1
# fullres_adj <- bind_rows(fullres_adj, res_diar_adj)


res_haz_adj <- d %>% group_by(study, sample, target) %>%
  do(res=aim2_tmle(., Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", 
                   study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", overwrite=T)) 
saveRDS(res_haz_adj, file=here("results/tmle_aim2_res_hazlist.Rds"))

# res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
# res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0
# res_haz_adj
# fullres_adj <- bind_rows(fullres_adj, res_haz_adj)



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






