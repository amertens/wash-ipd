
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds")) %>% filter(sample!="FP") %>% droplevels()

table(d$study)
df1 <- d %>% filter(study=="Holcomb 2021") %>% filter(!is.na(diar7d),!is.na(pos)) %>% distinct(dataid, hhid,childid,childNo)
df2 <- d %>% filter(study=="Holcomb 2021") %>% filter(!is.na(haz),!is.na(pos)) %>% distinct(dataid, hhid,childid,childNo)
# df <- d %>% filter(study=="Kwong 2021") %>% filter(!is.na(haz),!is.na(pos)) 
# df %>% select(child_date_anthro,env_date)

Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         



df <- d %>% filter(!is.na(pos), !is.na(waz), target=="Any pathogen", sample=="SW", study=="Odagiri 2016")

res <- aim2_glm(df, Ws = Wvars,  outcome="waz", exposure="pos", study=df$study[1], sample=df$sample[1], target=df$target[1], family="gaussian")
res


#-----------------------------------
# Unadjusted RR
#-----------------------------------



#ensure time ordering of diarrhea (anthro has been set in individual studies)
table(d$diar7d)
d$diar7d[d$child_date <= d$env_date | d$child_date > d$env_date+124] <- NA
table(d$diar7d)



fullres <- NULL
res_diar <- d %>% group_by(study, sample, target) %>%
   do(aim2_glm(., outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar$sparse <- ifelse(is.na(res_diar$RR), "yes", "no")
res_diar$RR[is.na(res_diar$RR)] <- 1
fullres <- bind_rows(fullres, res_diar)

temp <- res_diar %>% filter(study=="Holcomb 2021")
temp

res_stunt<- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., outcome="stunt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_stunt$sparse <- ifelse(is.na(res_stunt$RR), "yes", "no")
res_stunt$RR[is.na(res_stunt$RR)] <- 1
fullres <- bind_rows(fullres, res_stunt)

res_wast<- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., outcome="wast", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_wast$sparse <- ifelse(is.na(res_wast$RR), "yes", "no")
res_wast$RR[is.na(res_wast$RR)] <- 1
fullres <- bind_rows(fullres, res_wast)

res_underwt <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., outcome="underwt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_underwt$sparse <- ifelse(is.na(res_underwt$RR), "yes", "no")
res_underwt$RR[is.na(res_underwt$RR)] <- 1
fullres <- bind_rows(fullres, res_underwt)

res_haz <- d %>% group_by(study, sample, target) %>% filter(!is.na(haz)) %>%
   do(aim2_glm(., outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz$sparse <- ifelse(is.na(res_haz$coef), "yes", "no")
res_haz$coef[is.na(res_haz$coef)] <- 0
res_haz %>% filter(study=="Kwong 2021")
fullres <- bind_rows(fullres, res_haz)

res_whz <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., outcome="whz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_whz$sparse <- ifelse(is.na(res_whz$coef), "yes", "no")
res_whz$coef[is.na(res_whz$coef)] <- 0
res_whz
fullres <- bind_rows(fullres, res_whz)

res_waz <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., outcome="waz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_waz$sparse <- ifelse(is.na(res_waz$coef), "yes", "no")
res_waz$coef[is.na(res_waz$coef)] <- 0
res_waz
fullres <- bind_rows(fullres, res_waz)




set.seed(12345)
fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars,  outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar_adj$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_diar_adj)
temp <- res_diar_adj %>% filter(target=="V. cholerae")

res_stunt_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars_anthro,  outcome="stunt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_stunt_adj$sparse <- ifelse(is.na(res_stunt_adj$RR), "yes", "no")
res_stunt_adj$RR[is.na(res_stunt_adj$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_stunt_adj)

res_wast_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars_anthro,  outcome="wast", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_wast_adj$sparse <- ifelse(is.na(res_wast_adj$RR), "yes", "no")
res_wast_adj$RR[is.na(res_wast_adj$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_wast_adj)

res_underwt_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars_anthro,  outcome="underwt", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_underwt_adj$sparse <- ifelse(is.na(res_underwt_adj$RR), "yes", "no")
res_underwt_adj$RR[is.na(res_underwt_adj$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_underwt_adj)


res_haz_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars_anthro,  outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0

res_haz_adj  %>% filter(target=="Any pathogen", sample!="S") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(Nstudies=n()) %>%
  filter(Nstudies>=4) %>%
  do(try(pool.cont(.)))

# adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 
# temp<-adj_RR%>%filter(target=="Any pathogen", sample=="any sample type", Y=="haz")

fullres_adj <- bind_rows(fullres_adj, res_haz_adj)

res_waz_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars_anthro,  outcome="waz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_waz_adj$sparse <- ifelse(is.na(res_waz_adj$coef), "yes", "no")
res_waz_adj$coef[is.na(res_waz_adj$coef)] <- 0
res_waz_adj
fullres_adj <- bind_rows(fullres_adj, res_waz_adj)

res_whz_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars_anthro,  outcome="whz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_whz_adj$sparse <- ifelse(is.na(res_whz_adj$coef), "yes", "no")
res_whz_adj$coef[is.na(res_whz_adj$coef)] <- 0
res_whz_adj
fullres_adj <- bind_rows(fullres_adj, res_whz_adj)


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


saveRDS(fullres, file=here("results/unadjusted_aim2_res.Rds"))
saveRDS(fullres_adj, file=here("results/adjusted_aim2_res.Rds"))

#examine N's by study and outcome
tab <- fullres_adj %>% group_by(study, Y) %>% 
  filter(study!="Pooled") %>%
  summarise(
    medianN=median(N, na.rm=T), 
    maxN=max(N, na.rm=T),
    median_case=median(a+c, na.rm=T), 
    max_case=max(a+c, na.rm=T)) %>% 
  filter(maxN!=0) %>%
  as.data.frame()
tab



