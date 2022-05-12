
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)

table(d$study)
table(d$target)


#drop baseline observations and food because only in one study and no estimates
table(is.na(d$round))
food <- d %>% filter(sample=="FP") %>% droplevels()
table(food$study)
table(food$target, food$pos)
d <- d %>% filter(round!="bl", sample!="FP") %>% droplevels()

#clean covariates
d <- aim1_clean_covariates(d)


table(d$study)
table(d$target)
table(is.na(d$tr))
table(is.na(d$sample))
table(d$study, is.na(d$sample))
table(is.na(d$target))
table(d$study,d$sample)
table(d$study, d$round)
table(d$study, d$target)
table(d$study, d$sample)
table(d$sample, d$target, d$study)

df <- d %>% filter(!is.na(pos)) %>% distinct(study, sampleid)
dim(df)
table(df$study)

df2 <- df %>% filter(study=="Fuhrmeister 2020")


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


table(d$study, is.na(d$hhwealth))
table(d$study, is.na(d$hhwealth_cont))


table(d$study, is.na(d$landacre))
table(d$study, is.na(d$elec))
table(d$study, is.na(d$floor))
table(d$study, is.na(d$momedu))
table(d$study, is.na(d$dadagri))
table(d$study, is.na(d$momage))


# df <- d %>% filter(target=="Animal (BacCow)", sample=="CH", study=="Fuhrmeister 2020")
# 
# res1 <- aim1_glm(df, outcome="pos", study=df$study[1], sample=df$sample[1], target=df$target[1], Ws=Wvars, family="binomial")
# 
# table(df$pos)
# df$pos <- (1-df$pos)
# table(df$pos)
# 
# res2 <- aim1_glm(df, outcome="pos", study=df$study[1], sample=df$sample[1], target=df$target[1], Ws=Wvars, family="binomial")
# 
# res1



# Wvars2 = c("hhwealth_cont", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")         
# res2 <- aim1_glm(df, outcome="pos", study=df$study[1], sample=df$sample[1], target=df$target[1], Ws=Wvars2, family="binomial")


# d=df
# outcome="pos"
# study=df$study[1]
# sample=df$sample[1]
# target=df$target[1]
# Ws=Wvars
# family="binomial"
# 



#-----------------------------------
# Unadjusted RR
#-----------------------------------

# d <- d %>% 
#   filter(target %in% c("Any pathogen","Any MST")) %>% filter(study=="Holcomb 2021")


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






