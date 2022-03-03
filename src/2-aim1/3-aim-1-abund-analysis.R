

rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)

table(d$qual)
round(prop.table(table(d$qual))*100,1)

Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")         

#drop aggregate groups
d <- d %>% filter(sample!="any sample type", !grepl("Any ",target))



#clean covariates
d <- aim1_clean_covariates(d)


df <- d %>% filter(study=="Holcomb 2020", sample=="FlyLat", target== "Trichuris")
table(df$abund)


#-----------------------------------------------
# Drop abundances without variation
#-----------------------------------------------

# temp <- d %>% filter(study=="Boehm 2016") %>% droplevels()
# table(temp$target, !is.na(temp$abund), temp$sample)
# 
# #Drop abundances with less than or equal to 5 unique values or continuous estimation doesn't work
# d <- d %>% group_by(study, target, sample) %>% 
#   mutate(Nunique=length(unique(abund)), abund=ifelse(Nunique<=5,NA, abund))
# abund_dropped <- d %>% filter(Nunique<=5) %>% group_by(study, target, sample) %>% slice(1) %>% 
#   distinct(study, target, sample, Nunique ) %>% as.data.frame()
# abund_dropped
# nrow(abund_dropped)
# 
# temp <- d %>% filter(study=="Boehm 2016") %>% droplevels()
# table(temp$target, !is.na(temp$abund), temp$sample)


#Separate STH from MST abundances
d <- d %>% filter(!is.na(abund)) %>% droplevels(.)

table(d$sample, d$target)
sth <- d %>% filter(target %in% c("Any STH","Ascaris","Trichuris") & sample!="Fly")  %>% droplevels(.)
d <- d %>% filter(!(target %in% c("Any STH","Ascaris","Trichuris"))|sample=="Fly")  %>% droplevels(.)

table(d$sample, d$target, d$study)

table(d$qual)
table(d$study,is.na(d$qual))





# table(d$sample, d$target)
# 
# #
# outcome="abund"
# study=d$study[1]
# sample=d$sample[1]
# target=d$target[1]
# study="Boehm 2016"
# sample="W"
# target="Avian (GFD)"
# Ws=NULL
# family="gaussian"


#-----------------------------------
# Unadjusted abundance (negative binomial)
#-----------------------------------

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