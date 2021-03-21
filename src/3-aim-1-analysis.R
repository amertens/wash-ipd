
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)

#Drop baseline measure from mapsan
d <- d %>% filter(round != "0m") %>% droplevels(.)
d$study[d$round=="World Bank"] <- "WBB-World Bank"

table(d$study, d$round)
table(d$study, d$target)
table(d$sample, d$target, d$study)

df <- d %>% filter(study=="WBB",target=="Any protozoa")
table(df$sample, df$pos)

#temporarily permute treatment assignment
set.seed(12345)
d <- d %>%  group_by(study, round) %>%
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
Ws = Wvars = c("hhwealth", "Nhh","momedu",
     "hfiacat", "nrooms","walls", "floor","elec")         




outcome="pos"
study="WBB"
sample="S"
target="ascaris"
Ws=NULL
family="binomial"


outcome="abund"
study="mapsan"
sample="ds"
target="HF183"
Ws=Wvars
family="neg.binom"


d %>% distinct(study, sample, target) %>% as.data.frame()

df <- d %>% filter(round=="4")
res <- aim1_glm(df, outcome="pos", study="Gram Vikas", sample="SW", target="Shigella", Ws=NULL, family="binomial")
res




#-----------------------------------
# Unadjusted RR
#-----------------------------------
res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
   do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res


summary(res$RR)

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/unadjusted_aim1_RR.Rds"))


#-----------------------------------
# Unadjusted RD 
#-----------------------------------
res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian"))
res
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
res

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/adjusted_aim1_RR.Rds"))


#-----------------------------------
# Adjusted RD 
#-----------------------------------
res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian"))
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






