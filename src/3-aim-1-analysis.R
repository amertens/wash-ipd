
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)

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




outcome="abund"
study="WBK"
type="S"
target="sth"
family="neg.binom"


d %>% distinct(study, type, target) %>% as.data.frame()

res <- aim1_glm(d, outcome="pos", study="mapsan", type="ds", target="HF183", Ws=Wvars, family="neg.binom")
res




#NOTES:
#add code to skip estimate if sparse N <10, and add code to only pick n/10 covariates


#-----------------------------------
# Unadjusted RR
#-----------------------------------
res <- d %>% group_by(study, type, target) %>%
   do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], family="binomial"))
res

summary(res$RR)
res[res$RR<0.1,]
res[res$RR>20,]
#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

save(res, file=here("results/unadjusted_aim1_RR.Rds"))


#-----------------------------------
# Unadjusted RD 
#-----------------------------------
res <- d %>% group_by(study, round, type, target) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], family="gaussian"))
res

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

save(res, file=here("results/unadjusted_aim1_RD.Rds"))



#-----------------------------------
# Adjusted RR
#-----------------------------------
res <- d %>% group_by(study, round, type, target) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], Ws=Wvars, family="binomial"))
res

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

save(res, file=here("results/adjusted_aim1_RR.Rds"))


#-----------------------------------
# Adjusted RD 
#-----------------------------------
res <- d %>% group_by(study, round,  type, target) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], Ws=Wvars, family="gaussian"))
res

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

save(res, file=here("results/adjusted_aim1_RD.Rds"))

#-----------------------------------
# Unadjusted abundance (negative binomial)
#-----------------------------------

#TO DO:
# implement neg binomial
# impute low values
# check if they should be log transformed

res <- d %>% group_by(study, round,  type, target) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], type=.$type[1], target=.$target[1], Ws=NULL, family="neg.binom"))
res

save(res, file=here("results/unadjusted_aim1_diff.Rds"))

#-----------------------------------
# Adjusted abundance (negative binomial)
#-----------------------------------

res <- d %>% group_by(study, round, type, target) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], type=.$type[1], target=.$target[1], Ws=Wvars, family="neg.binom"))
res

save(res, file=here("results/adjusted_aim1_diff.Rds"))

#Primary figure
res %>% filter(RR>0.1, RR < 20) %>%
  #filter(adjusted==1, binary==1) %>% 
  droplevels(.) %>%
  ggplot(., aes(y=RR, x=target, color=Y)) +
  facet_grid(type~study) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau10[4:1]) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")






