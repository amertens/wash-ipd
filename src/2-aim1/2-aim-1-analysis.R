
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)

table(d$target)

#Why is aggregation function leading to bl dropping of all Any_ variables?

#drop baseline observations and food because only in one study and no estimates
table(is.na(d$round))
d <- d %>% filter(round!="bl", sample!="FP") %>% droplevels()

table(d$target)
table(is.na(d$tr))
table(is.na(d$sample))
table(is.na(d$target))
table(d$study,d$sample)
table(d$study, d$round)
table(d$study, d$target)
table(d$study, d$sample)
table(d$sample, d$target, d$study)


# 1.	Child birth order/parity -aim2 only
# 2.	Asset-based wealth index 
# 3.	Number of individuals and children in household
# 4.	Household food security  -aim2 only
# 5.	Household electrification and construction, including wall/roof material 
# 6.	Parental age 
# 7.	Parental education 
# 8.	Parental employment 
# a.	Indicator for works in agriculture 
# 9.	Land ownership 

colnames(d)
Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")         





# d %>% distinct(study, sample, target) %>% as.data.frame()
# 
# table(d$study, d$trial)
# outcome="pos"
# study="Boehm 2016"
# sample="any sample type"
# target="Any general MST"
# Ws=Wvars
# Ws=NULL
# family="binomial"
# 
# temp <- d %>% filter(study==!!(study), target==!!(target))
# table(temp$sample, temp$pos)




#-----------------------------------
# Unadjusted RR
#-----------------------------------

# d <- d %>% 
#   filter(target %in% c("Any pathogen","Any MST")) %>% filter(study=="Holcomb 2020")


res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
   do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res$sparse <- ifelse(is.na(res$RR), "yes", "no")
res$RR[is.na(res$RR)] <- 1


saveRDS(res, file=here("results/unadjusted_aim1_RR.Rds"))


# #-----------------------------------
# # Unadjusted RD 
# #-----------------------------------
# res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
#   do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian"))
# res$sparse <- ifelse(is.na(res$RR), "yes", "no")
# res$RR[is.na(res$RR)] <- 1
# 
# saveRDS(res, file=here("results/unadjusted_aim1_RD.Rds"))



#-----------------------------------
# Adjusted RR
#-----------------------------------
res <- d %>% 
  group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial"))
res$sparse <- ifelse(is.na(res$RR), "yes", "no")
res$RR[is.na(res$RR)] <- 1


saveRDS(res, file=here("results/adjusted_aim1_RR.Rds"))

table(res$sample)
temp<-res %>% filter(sample=="FlyLat", target=="Pathogenic E. coli")

# #-----------------------------------
# # Adjusted RD 
# #-----------------------------------
# res <- d %>% 
#   group_by(study, sample, target, aggregate_Y) %>%
#   do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian"))
# res$sparse <- ifelse(is.na(res$RR), "yes", "no")
# res$RR[is.na(res$RR)] <- 1
# saveRDS(res, file=here("results/adjusted_aim1_RD.Rds"))






