
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
head(d)
d <- droplevels(d)

d <- d %>% filter(sample!="FP") %>% droplevels()


#ensure time ordering of diarrhea (anthro has been set in individual studies)
table(d$diar7d)
d$diar7d[d$child_date <= d$env_date | d$child_date > d$env_date+124] <- NA
table(d$diar7d)



#Harmonize sex coding
table(d$sex)
table(d$study, d$sex)
table(as.numeric(d$sex), d$sex)
d <- d %>%
  rename(sex_factor=sex) %>%
  mutate(
    sex = case_when(
      sex_factor=="female" ~0,
      sex_factor=="male" ~1,
      sex_factor=="Female" ~0,
      sex_factor=="Male" ~1,
      sex_factor=="1" & study %in% c("Holcomb 2021","Capone 2022 in prep","Capone 2021") ~0,
      sex_factor=="0" & study %in% c("Holcomb 2021","Capone 2022 in prep","Capone 2021") ~1,
      sex_factor=="1" & study %in% c("Reese 2017","Odagiri 2016") ~1,
      sex_factor=="2" & study %in% c("Reese 2017","Odagiri 2016") ~0)
  )
table(d$study, d$sex, !is.na(d$haz))


#Code wet season based on child health date
d <- d %>%
  mutate(
    month=month(child_date),
    wet_CH=
      case_when(
        #bangladesh monsoon may to october: https://www.weather-atlas.com/en/bangladesh/mymensingh-climate
        #India is may to october https://www.weather-atlas.com/en/india/bhubaneswar-climate#rainfall
        trial%in% c("WBB","Gram Vikas","Odisha") & month>=5 & month<=10  ~ 1,
        trial%in% c("WBB","Gram Vikas","Odisha") & (month<5 | month>10)  ~ 0,
        # Kenya has two peaks, march-may and oct-dec:  https://www.weather-atlas.com/en/kenya/kakamega-climate
        trial=="WBK" & month %in% c(3,4,5,10,11,12)  ~ 1,
        trial=="WBK" & month %in% c(1,2,6,7,8,9)  ~ 0,
        #Maputo is November to april: https://www.weather-atlas.com/en/mozambique/maputo-climate#rainfall
        trial=="MapSan" & month %in% c(11,12,1,2,3,4)  ~ 1,
        trial=="MapSan" & month %in% c(5,6,7,8,9,10)  ~ 0
      )
  )


d_wet <- d %>% filter(!is.na(wet))%>% droplevels() 
#d_wet_ch <- d %>% filter(!is.na(wet_CH), study!="Holcomb 2021")%>% droplevels() 
d_wet_ch <- d %>% filter(!is.na(wet_CH))%>% droplevels() 
d_animals <- d %>% filter(!is.na(animals)) %>% droplevels()
d_sex <- d %>% filter(!is.na(sex))%>% droplevels() 
d_tr <- d %>% filter(!is.na(tr))%>% droplevels() 



table(d$study, d$wet)
table(d$study, d$wet_CH)
table(d$study, is.na(d$wet))
table(d$study, is.na(d$wet_CH))
table(d$study, is.na(d$child_date))
table(d$study, d$animals)
table(d$study, d$animals, d$diar7d)


Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro_sex = c("age_anthro","hfiacat","momage","hhwealth", "tr","Nhh","nrooms","walls","roof", "floor","elec","dadagri","landown","momedu")         



#-----------------------------------
# Unadjusted RR
#-----------------------------------

# outcome="diar7d"
# exposure="pos"
# study="Fuhrmeister 2020"
# sample="any sample type"
# target= "Any pathogen"
# Vvar="sex"
# family="gaussian"
# minN_thres = 5
# forcedW=NULL
# Ws=NULL
minN = 1


# res_diar_animals_unadj <- d_animals %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = NULL, Vvar="animals", forcedW=NULL, outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN)) 
# res_diar_wet_unadj <- d_wet %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = NULL, Vvar="wet", forcedW=NULL, outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN)) 
# res_diar_sex_unadj <- d_sex %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = NULL, Vvar="sex", forcedW=NULL, outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN)) 
# 
# res_haz_animals_unadj <- d_animals %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = NULL, Vvar="animals", forcedW=NULL, outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN)) 
# res_haz_wet_unadj <- d_wet %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = NULL, Vvar="wet", forcedW=NULL, outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN)) 
# res_haz_sex_unadj <- d_sex %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = NULL, Vvar="sex", forcedW=NULL, outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN)) 
# 
# res_bin <- bind_rows(res_diar_animals_unadj, res_diar_wet_unadj, res_diar_sex_unadj)
# res_bin$sparse <- ifelse(is.na(res_bin$RR), "yes", "no")
# res_bin$RR[is.na(res_bin$RR)] <- 1
# 
# res_cont <- bind_rows(res_haz_animals_unadj, res_haz_wet_unadj, res_haz_sex_unadj)
# res_cont$sparse <- ifelse(is.na(res_cont$coef), "yes", "no")
# res_cont$coef[is.na(res_cont$coef)] <- 0
# 
# res <- bind_rows(res_bin, res_cont)
# saveRDS(res, file=here("results/unadjusted_aim2_emm.Rds"))
# 
# 
# table(d_sex$sex, d_sex$diar7d, d_sex$pos)

#-----------------------------------
# Adjusted RR
#-----------------------------------
res_diar_animals_adj <- d_animals %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars, Vvar="animals", forcedW=c("age", "hhwealth"),outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN))
res_diar_wet_adj <- d_wet %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars, Vvar="wet", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN))


Wvars_diar_sex  = c("age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
res_diar_sex_adj <- d_sex %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars_diar_sex, Vvar="sex", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN))


res_diar_animals_adj_PD <- d_animals %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars, Vvar="animals", forcedW=c("age", "hhwealth"),outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))
res_diar_wet_adj_PD <- d_wet %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars, Vvar="wet", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))
res_diar_sex_adj_PD <- d_sex %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars_diar_sex, Vvar="sex", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))


res_haz_animals_adj <- d_animals %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars_anthro, Vvar="animals", forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))
res_haz_wet_adj <- d_wet %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars_anthro, Vvar="wet", forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))

#Wvars without sex
Wvars_anthro_sex = c("age_anthro","hfiacat","momage","hhwealth", "tr","Nhh","nrooms","walls","roof", "floor","elec","dadagri","landown","momedu")         
res_haz_sex_adj <- d_sex %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars_anthro_sex, Vvar="sex", forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))
# res_waz_sex_adj <- d_sex %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = Wvars_anthro_sex, Vvar="sex", forcedW=c("age", "hhwealth"), outcome="waz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))

res_diar_wet_CH_adj <- d_wet_ch %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars, Vvar="wet_CH", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN))
res_diar_wet_CH_adj_PD <- d_wet_ch %>% group_by(study, sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars, Vvar="wet_CH", 
                   forcedW=c("age", "hhwealth"), 
                   outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))

res_diar_wet_CH_adj%>%filter(V=="wet_CH", sample=="any sample type", target=="Any pathogen")
res_diar_wet_CH_adj_PD%>%filter(V=="wet_CH", sample=="any sample type", target=="Any pathogen")



#pooled together
res_diar_wet_CH_adj_pooled <- d_wet_ch %>% mutate(study=="all") %>% group_by(sample, target) %>%
  do(aim2_subgroup(., Ws = Wvars, Vvar="wet_CH", forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN))
res_diar_wet_CH_adj_pooled

res_bin_adj <- bind_rows(res_diar_animals_adj, res_diar_wet_adj, res_diar_wet_CH_adj, res_diar_sex_adj)
res_bin_adj$sparse <- ifelse(is.na(res_bin_adj$RR), "yes", "no")
res_bin_adj$RR[is.na(res_bin_adj$RR)] <- 1


res_PD_adj <- bind_rows(res_diar_animals_adj_PD, res_diar_wet_adj_PD, res_diar_wet_CH_adj_PD, res_diar_sex_adj_PD)
res_PD_adj$sparse <- ifelse(is.na(res_PD_adj$coef), "yes", "no")
res_PD_adj$coef[is.na(res_PD_adj$coef)] <- 0

res_cont_adj <- bind_rows(res_haz_animals_adj, res_haz_wet_adj, res_haz_sex_adj, res_waz_sex_adj)
res_cont_adj$sparse <- ifelse(is.na(res_cont_adj$coef), "yes", "no")
res_cont_adj$coef[is.na(res_cont_adj$coef)] <- 0

table(res_cont_adj$sparse)
table(res_bin_adj$sparse)

res_adj <- bind_rows(res_bin_adj, res_cont_adj)
saveRDS(res_adj, file=here("results/adjusted_aim2_emm.Rds"))

saveRDS(res_PD_adj, file=here("results/adjusted_aim2_emm_PD.Rds"))


unique(res_PD_adj$V)
unique(res_PD_adj$Y)

res_PD_adj %>% filter(V=="wet_CH", target=="Any pathogen", Y=="diar7d")


# #Treatment subgroups
# table(d_tr$tr, d_tr$diar7d, d_tr$study)
# 
# res_diar_tr_adj <- d_tr %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = Wvars[!(Wvars=="tr")], Vvar="tr", forcedW=c("age", "hhwealth"),outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial", minN_thres = minN))
# res_haz_tr_adj <- d_tr %>% group_by(study, sample, target) %>%
#   do(aim2_subgroup(., Ws = Wvars_anthro[!(Wvars_anthro=="tr")], Vvar="tr", forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian", minN_thres = minN))
# 
# res_diar_tr_adj<- res_diar_tr_adj %>% filter(!is.na(coef)) 
# res_haz_tr_adj<- res_haz_tr_adj %>% filter(!is.na(coef)) 
# res_haz_tr_adj_sig <- res_haz_tr_adj  %>% filter(int.p<0.05)
