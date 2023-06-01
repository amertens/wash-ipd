
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


#NOTE Need to check this one
outcome="haz"
exposure="pos"
study="Boehm 2016"
sample="CH"
target= "Any MST "
family="binomial"
Ws=Wvars
overwrite=F
test =aim2_tmle(d, Ws = Wvars, outcome="diar7d", exposure="pos", study=study, sample=sample, target=target, family="binomial", overwrite=T)



fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target) %>%
  do(res=aim2_tmle(., Ws = Wvars, outcome="diar7d", exposure="pos",
                   study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", overwrite=T)) 
saveRDS(res_diar_adj, file=here("results/tmle_aim2_res_diarlist.Rds"))




res_haz_adj <- d %>% group_by(study, sample, target) %>%
  do(res=aim2_tmle(., Ws = Wvars_anthro, outcome="haz", exposure="pos", 
                   study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", overwrite=T)) 
saveRDS(res_haz_adj, file=here("results/tmle_aim2_res_hazlist.Rds"))

