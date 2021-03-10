
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



#-----------------------------------
# Unadjusted RR
#-----------------------------------
res <- d %>% group_by(study, round, sample, target, aggregate_Y) %>%
   do(aim2_glm(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res


summary(res$RR)

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/unadjusted_aim2_RR.Rds"))




