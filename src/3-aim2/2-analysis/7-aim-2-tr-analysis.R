
rm(list=ls())
source(here::here("0-config.R"))

dfull <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds")) %>% filter(!is.na(pos)|!is.na(abund))

#make one observation per child health outcome (instead of multiple for many targets/samples)
colnames(dfull)
ddiar <- dfull %>% distinct(study, dataid, hhid, childid, tr, round, age, child_date, diar7d, .keep_all=T) %>% filter(!is.na(diar7d)) %>% mutate(study="", sample="", target="")
ddiar_sens <- dfull %>% distinct(study, dataid, hhid, childid, tr, round, age, child_date, diar7d_full, .keep_all=T) %>% filter(!is.na(diar7d_full)) %>% mutate(study="", sample="", target="")
dhaz <- dfull %>% distinct(study, dataid, hhid, childid, tr, round,  age, child_date_anthro, haz, .keep_all=T)  %>% filter(!is.na(haz)) %>% mutate(study="", sample="", target="")

table(dfull$trial)
table(ddiar$trial)
table(ddiar$trial, ddiar$diar7d, ddiar$tr)
table(dhaz$trial)
table(dhaz$trial, dhaz$tr)

temp <- ddiar%>% filter(trial=="Gram Vikas")

Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu")         

ddiar$tr <- ifelse(ddiar$tr=="Intervention",1,0)
dhaz$tr <- ifelse(dhaz$tr=="Intervention",1,0)


fullres_unadj <- NULL
res_diar_unadj <- ddiar %>% group_by(trial) %>%
  do(aim2_glm(., Ws = NULL,  outcome="diar7d", exposure="tr", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres=1)) 
res_diar_unadj$sparse <- ifelse(is.na(res_diar_unadj$RR), "yes", "no")
res_diar_unadj$RR[is.na(res_diar_unadj$RR)] <- 1

# res_diar_unadj_sens <- ddiar_sens %>% group_by(trial) %>%
#   do(aim2_glm(., Ws = NULL,  outcome="diar7d_full", exposure="tr", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres=1)) 
# res_diar_unadj_sens$sparse <- ifelse(is.na(res_diar_unadj_sens$RR), "yes", "no")
# res_diar_unadj_sens$RR[is.na(res_diar_unadj_sens$RR)] <- 1


res_haz_unadj <- dhaz %>% group_by(trial) %>% filter(study!="Odagiri 2016") %>% droplevels() %>%
  do(aim2_glm(., Ws = NULL, outcome="haz", exposure="tr", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz_unadj$sparse <- ifelse(is.na(res_haz_unadj$coef), "yes", "no")
res_haz_unadj$coef[is.na(res_haz_unadj$coef)] <- 0

fullres_unadj <- bind_rows(res_diar_unadj, res_haz_unadj)


fullres_adj <- NULL
res_diar_adj <- ddiar %>% group_by(trial) %>%
  do(aim2_glm(., Ws = Wvars,  outcome="diar7d", exposure="tr", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres=1)) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar_adj$RR)] <- 1


res_haz_adj <- dhaz %>% group_by(trial) %>% filter(study!="Odagiri 2016") %>% droplevels() %>%
  do(aim2_glm(., Ws = Wvars_anthro, outcome="haz", exposure="tr", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0

fullres_adj <- bind_rows(res_diar_adj, res_haz_adj)


saveRDS(fullres_unadj, file=here("results/unadjusted_aim2_tr_res.Rds"))
saveRDS(fullres_adj, file=here("results/adjusted_aim2_tr_res.Rds"))





