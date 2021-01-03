
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)

#Drop baseline measure from mapsan
d <- d %>% filter(round != "0m") %>% droplevels(.)


#temporarily permute treatment assignment
d <- d %>%  group_by(study) %>%
  mutate(tr = sample(tr, replace=FALSE))  




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
Ws = Wvars = c("hhwealth",
     "Nhh",
     "momedu",
     "hfiacat",
  "hhwealth","nrooms","momedu","walls" ,    
"floor","elec")         




outcome="pos"
study="WBB"
type="S"
target="ascaris"
Ws=NULL
family="binomial"


outcome="abund"
study="mapsan"
type="ds"
target="HF183"
Ws=Wvars
family="neg.binom"


d %>% distinct(study, type, target) %>% as.data.frame()

res <- aim1_glm(d, outcome="abund", study="mapsan", type="ds", target="HF183", Ws=Wvars, family="neg.binom")
res

#





#-----------------------------------
# Unadjusted RR
#-----------------------------------
res <- d %>% group_by(study, round, type, target, aggregate_Y) %>%
   do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], family="binomial"))
res


summary(res$RR)

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/unadjusted_aim1_RR.Rds"))


#-----------------------------------
# Unadjusted RD 
#-----------------------------------
res <- d %>% group_by(study, round, type, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], family="gaussian"))
res

summary(res$coef)

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/unadjusted_aim1_RD.Rds"))



#-----------------------------------
# Adjusted RR
#-----------------------------------
res <- d %>% group_by(study, round, type, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], Ws=Wvars, family="binomial"))
res

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/adjusted_aim1_RR.Rds"))


#-----------------------------------
# Adjusted RD 
#-----------------------------------
res <- d %>% group_by(study, round, type, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], Ws=Wvars, family="gaussian"))
res

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/adjusted_aim1_RD.Rds"))

#-----------------------------------
# Unadjusted abundance (negative binomial)
#-----------------------------------

#TO DO:
# impute low values
# check if they should be log transformed

res <- d %>% group_by(study, round, type, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], type=.$type[1], target=.$target[1], Ws=NULL, family="neg.binom"))
res

saveRDS(res, file=here("results/unadjusted_aim1_diff.Rds"))

#-----------------------------------
# Adjusted abundance (negative binomial)
#-----------------------------------

res <- d %>% group_by(study, round, type, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], type=.$type[1], target=.$target[1], Ws=Wvars, family="neg.binom"))
res

saveRDS(res, file=here("results/adjusted_aim1_diff.Rds"))





#-----------------------------------
# Adjusted RR - subgroup analyses
#-----------------------------------
res <- d %>% group_by(study, round, type, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], Ws=Wvars, family="binomial"))
res

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/adjusted_aim1_RR_subgroup.Rds"))






