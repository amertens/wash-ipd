
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)

table(d$study, !is.na(d$animals))
table(d$study, d$animals)
table(d$study, d$wet)

df <- d %>% filter(sample=="FP")
table(df$target, df$pos, df$tr)
prop.table(table(df$target, df$pos),1)
#drop baseline observations and food because only in one study and no estimates, and odigari as it has no variation
table(is.na(d$round))
d <- d %>% filter(round!="bl", sample!="FP", trial!="Odisha") %>% droplevels()
Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")    

#clean covariates
d <- aim1_clean_covariates(d)


d_wet <- d %>% filter(!is.na(wet))%>% droplevels() 
d_animals <- d %>% filter(!is.na(animals)) %>% droplevels()


# #check Kwong numbers
# df <- d %>% filter(study=="Kwong 2021")
# table(df$tr, df$target)
# table(df$tr, df$target, df$sample) 
# table(df$tr, df$target, df$sample, df$wet) 
# 
# df <- df %>% filter(sample=="S", target=="Any pathogen")
# table(df$tr, df$pos)
# table(df$tr,  df$wet)
# prop.table(table(df$tr, df$pos),1)

table(d$study, d$trial)
outcome="pos"
study="Boehm 2016"
sample="any sample type"
target="Any MST"
Ws=Wvars
#Ws=NULL
Vvar="wet"
family="binomial"
d=d_wet
#
# temp <- d %>% filter(study==!!(study), target==!!(target))
# table(temp$sample, temp$pos)
#
#
#aim1_subgroup(d=d_wet, Vvar="wet", outcome="pos", study=d_wet$study[1], sample=d_wet$sample[1], target=d_wet$target[1], family="binomial")



#Error in names(dn) <- dnn : attempt to set an attribute on NULL 


#-----------------------------------
# Unadjusted RR
#-----------------------------------

res_wet <- d_wet %>% group_by(study, sample, target, aggregate_Y) %>%
   do(aim1_subgroup(., Vvar="wet", outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res_animals <- d_animals %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_subgroup(., Vvar="animals", outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res <- bind_rows(res_wet, res_animals)


temp <- res %>% filter(int.p < 0.05)


# res$sparse <- ifelse(is.na(res$RR), "yes", "no")
# res$RR[is.na(res$RR)] <- 1

saveRDS(res, file=here("results/unadjusted_aim1_emm.Rds"))




#-----------------------------------
# Adjusted RR
#-----------------------------------
res_wet_adj <- d_wet %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_subgroup(., Vvar="wet", outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial"))
res_animals_adj <- d_animals %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_subgroup(., Vvar="animals", outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1],  Ws=Wvars, family="binomial"))
res_adj <- bind_rows(res_wet_adj, res_animals_adj)

temp <- res_adj %>% filter(int.p < 0.05)

saveRDS(res_adj, file=here("results/adjusted_aim1_emm.Rds"))




#-----------------------------------
# Adjusted RD
#-----------------------------------
res_wet_adj_RD <- d_wet %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_subgroup(., Vvar="wet", outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian"))
res_animals_adj_RD <- d_animals %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_subgroup(., Vvar="animals", outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1],  Ws=Wvars, family="gaussian"))
res_adj_RD <- bind_rows(res_wet_adj_RD, res_animals_adj_RD)
# res$sparse <- ifelse(is.na(res$RR), "yes", "no")
# res$RR[is.na(res$RR)] <- 1
saveRDS(res_adj_RD, file=here("results/adjusted_aim1_emm_RD.Rds"))

