
rm(list=ls())
source(here::here("0-config.R"))
unadj_RR <- readRDS(here("results/unadjusted_aim2_res.Rds"))
adj_RR <- readRDS(here("results/adjusted_aim2_res.Rds"))

unadj_RR <- clean_res(unadj_RR) #%>% distinct()
adj_RR <- clean_res(adj_RR) #%>% distinct()
head(unadj_RR)
table(unadj_RR$sample_cat)

Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         



binary_Y =c("diar7d","stunt","wast","underwt")
cont_Y =c("haz","waz","whz","underwt")

#pool primary estimates by study
res_RR_unadj <- res_RR_adj <- NULL
res_RR_unadj <- unadj_RR %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>%  
  do(try(poolRR(.)))

res_RR_adj <- adj_RR %>% filter(Y%in%binary_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% 
  do(poolRR(.)) 

res_cont_unadj <- unadj_RR %>% filter(Y%in%cont_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(try(pool.cont(.))) 

#adj_RR <- adj_RR %>% filter(Y=="haz")
res_cont_adj <- adj_RR %>% filter(Y%in%cont_Y, sample_cat!="Sparse data") %>%
  group_by(Y, sample, target) %>% 
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4) %>% 
  do(try(pool.cont(.))) 


unadj_pool <- bind_rows(unadj_RR, res_cont_unadj, res_RR_unadj)
unadj_pool$study <- factor(unadj_pool$study, levels = rev(c(levels(unadj_RR$study),"Pooled")))

adj_pool <- bind_rows(adj_RR, res_cont_adj, res_RR_adj)
adj_pool$study <- factor(adj_pool$study, levels = levels(unadj_pool$study))

#adj_pool<-adj_pool %>% filter(Y=="haz", sample=="any sample type", target=="Any pathogen")

saveRDS(unadj_pool, file=here("results/unadjusted_aim2_pooled.Rds"))
saveRDS(adj_pool, file=here("results/adjusted_aim2_pooled.Rds"))

