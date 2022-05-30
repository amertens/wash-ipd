
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)

table(d$study, !is.na(d$animals))
table(d$study, d$animals)
table(d$study, d$wet)

#drop baseline observations and food because only in one study and no estimates
table(is.na(d$round))
d <- d %>% filter(round!="bl", sample!="FP") %>% droplevels()
Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")    

#clean covariates
d <- aim1_clean_covariates(d)


table(d$target)
# df <- d %>% filter(!is.na(animals), target %in% c("Any zoonotic","Any non-zoonotic")) %>%
#   mutate(tr=animals) %>% 
#   droplevels()
df <- d %>% filter(target %in% c("Any zoonotic","Any non-zoonotic")) %>%
  droplevels()

table(df$study)

table(df$animals, df$pos, df$target)
dzoo <- df %>% filter(target=="Any zoonotic")
dnozoo <- df %>% filter(target=="Any non-zoonotic")
prop.table(table(dzoo$animals, dzoo$pos ),1)
prop.table(table(dnozoo$animals, dnozoo$pos ),1)

prop.table(table(df$target, df$pos ),1)*100

animal_zoonotic_tab <- round(prop.table(table(dzoo$animals, dzoo$pos ),1)*100,1)



# #-----------------------------------
# # Adjusted RR
# #-----------------------------------
# res_animals_adj <- df %>% group_by(study, sample, target, aggregate_Y) %>%
#   do(aim1_glm(.,  outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1],  Ws=Wvars, family="binomial"))
# res_animals_adj %>% filter(!is.na(RR))


#-----------------------------------
# Adjusted RR - with interaction
#-----------------------------------
table(df$target)
table(df$study, df$target)


res_animals_emm <- df %>% group_by(study, sample, aggregate_Y) %>%
  mutate(zoonotic=ifelse(target=="Any zoonotic",1,0), target="any pathogen") %>%
  do(aim1_subgroup(.,  Vvar="zoonotic", outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1],  Ws=Wvars, family="gaussian"))
res_animals_emm %>% filter(!is.na(coef))

df2 <- df %>% group_by(study, sample, aggregate_Y) %>%
  mutate(zoonotic=ifelse(target=="Any zoonotic",1,0), target="any pathogen") %>%
  filter(study=="Odagiri 2016")
table(df2$pos, df2$tr, df2$zoonotic)

res<-(aim1_subgroup(df2,  Vvar="zoonotic", outcome="pos", study=df2$study[1], sample=df2$sample[1], target=df2$target[1],  Ws=Wvars, family="gaussian"))

#pooling function

# res_pool <- res_animals_adj %>% filter(!is.na(se)) %>%
#   group_by(sample, study) %>%
#   mutate(N=n()) %>% filter(N==2) %>%
#   group_by(sample, target) %>%
#   mutate(N=n()) %>%
#   filter(N>=3) %>% group_by(sample, target) %>%
#   do(poolRR(.))
# res_pool


res_pool <- res_animals_emm %>% filter(!is.na(se)) %>% 
  group_by(sample, study) %>% 
  mutate(N=n()) %>% filter(N==2) %>%
  group_by(sample, target) %>% 
  mutate(N=n()) %>%
  filter(N>=3) %>% group_by(sample, target, Vlevel) %>%
  do(pool.cont(.)) 
res_pool

res_pool <- bind_rows(res_animals_emm, res_pool)
res_pool$study <- factor(res_pool$study, levels = rev(unique(res_pool$study)))
levels(res_pool$study)
res_pool %>% filter(!is.na(coef))
table(res_pool$study[!is.na(res_pool$coef)])


saveRDS(res_pool, file=here("results/adjusted_zoonotic_animals.Rds"))




