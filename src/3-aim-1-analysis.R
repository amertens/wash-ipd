
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)

#d$study <- as.numeric(d$study)

table(d$study, d$round)
table(d$study, d$target)
table(d$sample, d$target, d$study)

# #temporarily permute treatment assignment
# set.seed(12345)
# d <- d %>%  group_by(study, round) %>%
#   mutate(tr = sample(tr, replace=FALSE))




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
Ws = Wvars = c("hhwealth", "Nhh","momedu",
     "hfiacat", "nrooms","walls", "floor","elec")         





# d %>% distinct(study, sample, target) %>% as.data.frame()
# 
# outcome="pos"
# study="Boehm et al. 2016"
# sample="any sample type"
# target="Any general MST"
# Ws=Wvars
# Ws=NULL
# family="binomial"
# 
# temp <- d %>% filter(study==!!(study), target==!!(target)) 
# table(temp$sample, temp$pos)
# 
# 
# 
# 
# #df <- d %>% filter(study=="6", sample=="any sample type",target=="Any human MST") 
# df <- d %>% filter(study==!!(study), sample==!!(sample),target==!!(target)) 
# res <- aim1_glm(df, outcome=outcome, study=study, sample=sample, target=target, Ws=NULL, family="binomial")
# res
# 
# df1 <- d %>% filter(study==!!(study), sample=="MH",target==!!(target) )
# df2 <- d %>% filter(study==!!(study), sample=="CH",target==!!(target) )
# table(df$sample, df$pos)
# table(df1$sample, df1$pos)
# table(df2$sample, df2$pos)
# res <- aim1_glm(df, outcome=outcome, study=study, sample=sample, target=target, Ws=NULL, family="binomial")
# res

# res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
#   do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
# res




#-----------------------------------
# Unadjusted RR
#-----------------------------------
res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
   do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res %>% filter(!is.na(coef))


summary(res$RR)

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/unadjusted_aim1_RR.Rds"))


#-----------------------------------
# Unadjusted RD 
#-----------------------------------
res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian"))
res %>% filter(!is.na(coef))
table(res$study)
summary(res$coef)

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/unadjusted_aim1_RD.Rds"))



#-----------------------------------
# Adjusted RR
#-----------------------------------
res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial"))
res %>% filter(!is.na(coef))

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/adjusted_aim1_RR.Rds"))


#-----------------------------------
# Adjusted RD 
#-----------------------------------
res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian"))
res %>% filter(!is.na(coef))

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/adjusted_aim1_RD.Rds"))

#-----------------------------------
# Unadjusted abundance (negative binomial)
#-----------------------------------

#TO DO:
# impute low values
# check if they should be log transformed

res <- d %>% filter(!is.na(abund)) %>% droplevels(.) %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=NULL, family="neg.binom"))
res


saveRDS(res, file=here("results/unadjusted_aim1_diff.Rds"))

#-----------------------------------
# Adjusted abundance (negative binomial)
#-----------------------------------

res <- d %>% filter(!is.na(abund)) %>% droplevels(.) %>%  group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="neg.binom"))
res

saveRDS(res, file=here("results/adjusted_aim1_diff.Rds"))





#-----------------------------------
# Adjusted RR - subgroup analyses
#-----------------------------------
# res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
#   do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="binomial"))
# res
# 
# #drop estimates with less than 10 values
# res <- res %>% filter(minN>=10)
# 
# saveRDS(res, file=here("results/adjusted_aim1_RR_subgroup.Rds"))






