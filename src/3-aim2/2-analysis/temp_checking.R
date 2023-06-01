rm(list=ls())
source(here::here("0-config.R"))
unadj_RR <- readRDS(here("results/unadjusted_aim2_res.Rds"))
adj_RR <- readRDS(here("results/adjusted_aim2_res.Rds"))
unadj_emm <- readRDS(here("results/unadjusted_aim2_emm.Rds"))
adj_emm <- readRDS(here("results/adjusted_aim2_emm.Rds"))
adj_emm_PD <- readRDS(here("results/adjusted_aim2_emm_PD.Rds"))


adj_RR <- clean_res(adj_RR) #%>% distinct()

res1 <- adj_RR[4,]



d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds")) %>% filter(sample!="FP") %>% droplevels()
d <- d %>% filter(study=="Fuhrmeister 2020", target=="Any bacteria", !is.na(pos), !is.na(diar7d), sample=='CH') %>% droplevels()

#no variation in rooms, landown
table(d$tr)
#res <- glm(diar7d ~ pos + sex+age+hfiacat+momage+hhwealth+ Nhh+nrooms+walls+ roof+ floor+elec+dadagri+landacre+landown+ momedu+ tr , family=binomial(link="log"), data=d)
res <- glm(diar7d ~ pos + sex+age+hfiacat+momage+hhwealth+ Nhh+walls+ roof+ floor+elec+dadagri+landacre+ momedu+ tr , family=poisson(link="log"), data=d)
res <- summary(res)
exp(res$coefficients)

#check through function and sample size


Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#-----------------------------------
# Unadjusted RR
#-----------------------------------



#ensure time ordering of diarrhea (anthro has been set in individual studies)
table(d$diar7d)
d$diar7d[d$child_date <= d$env_date | d$child_date > d$env_date+124] <- NA
table(d$diar7d)



fullres <- NULL
res_diar <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar$sparse <- ifelse(is.na(res_diar$RR), "yes", "no")
res_diar$RR[is.na(res_diar$RR)] <- 1
fullres <- bind_rows(fullres, res_diar)
