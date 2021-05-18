

rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)

#TODO: need to add  Indicator for works in agriculture and Land ownership
Ws = Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","elec")         

#Separate STH from MST abundances
d <- d %>% filter(!is.na(abund)) %>% droplevels(.)

sth <- d %>% filter(target %in% c("Any STH","Ascaris","Trichuris"))
d <- d %>% filter(!(target %in% c("Any STH","Ascaris","Trichuris")))

# 
# outcome="abund"
# study=d$study[1]
# sample=d$sample[1]
# target=d$target[1]
# Ws=NULL
# family="gaussian"


#-----------------------------------
# Unadjusted abundance (negative binomial)
#-----------------------------------

#TO DO:
# impute low values
# check if they should be log transformed

res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
  mutate(abund=log10(abund)) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=NULL, family="gaussian"))
res <- res %>% filter(!is.na(coef))
res$model <- "linear"


res_sth <- sth %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=NULL, family="neg.binom"))
res_sth$model <- "neg. binomial"
res <- res %>% filter(!is.na(coef))

res <- bind_rows(res, res_sth)
saveRDS(res, file=here("results/unadjusted_aim1_diff.Rds"))

#-----------------------------------
# Adjusted abundance (negative binomial)
#-----------------------------------

res <- d %>% group_by(study, sample, target, aggregate_Y) %>%
  mutate(abund=log10(abund)) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="gaussian"))
res <- res %>% filter(!is.na(coef))
res$model <- "linear"

res_sth <- sth %>% group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_glm(., outcome="abund", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="neg.binom"))
res <- res %>% filter(!is.na(coef))
res_sth$model <- "neg. binomial"

res <- bind_rows(res, res_sth)

saveRDS(res, file=here("results/adjusted_aim1_diff.Rds"))