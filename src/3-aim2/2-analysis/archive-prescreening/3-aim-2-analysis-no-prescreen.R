
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds")) %>% filter(sample!="FP") %>% droplevels()

Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         





#-----------------------------------
# Unadjusted RR
#-----------------------------------



#ensure time ordering of diarrhea (anthro has been set in individual studies)
table(d$diar7d)
d$diar7d[d$child_date <= d$env_date | d$child_date > d$env_date+124] <- NA
table(d$diar7d)




set.seed(12345)
fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars, forcedW=Wvars, outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar_adj$RR)] <- 1
fullres_adj <- bind_rows(fullres_adj, res_diar_adj)
temp <- res_diar_adj %>% filter(target=="V. cholerae")

res_haz_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars_anthro, forcedW=Wvars_anthro, outcome="haz", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="gaussian")) 
res_haz_adj$sparse <- ifelse(is.na(res_haz_adj$coef), "yes", "no")
res_haz_adj$coef[is.na(res_haz_adj$coef)] <- 0


fullres_adj <- bind_rows(res_diar_adj, res_haz_adj)

saveRDS(fullres_adj, file=here("results/adjusted_aim2_res_no_prescreen.Rds"))
#fullres_adj <- readRDS(here("results/adjusted_aim2_res_no_prescreen.Rds"))


adj_RR <- clean_res(fullres_adj) #%>% distinct()


binary_Y =c("diar7d","stunt","wast","underwt")
cont_Y =c("haz","waz","whz")

#pool primary estimates by study
res_RR_adj <- adj_RR %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(poolRR(.)) 

res_cont_adj <- adj_RR %>% filter(Y%in%cont_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(try(pool.cont(.))) 



adj_pool <- bind_rows(adj_RR, res_cont_adj, res_RR_adj)
adj_pool$study <- factor(adj_pool$study, levels = unique(adj_pool$study))

saveRDS(adj_pool, file=here("results/adjusted_aim2_pooled_no_prescreen.Rds"))




