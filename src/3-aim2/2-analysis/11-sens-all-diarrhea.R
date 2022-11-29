
rm(list=ls())
source(here::here("0-config.R"))

#make full env. dataset
dfull <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds")) %>% filter(!is.na(diar7d))

colnames(dfull)

dim(dfull)
dfull <- dfull %>% select(study, trial, sampleid, dataid, hhid, clusterid, 
                  sample, env_date, target, pos,
                  hhwealth, hfiacat,
                  momage, hhwealth_cont, Nhh, nrooms, walls, roof,
                  floor, elec, dadagri, landacre, landown, momedu, momage,
                  tr, miss_landacre,
                  miss_momage, miss_hhwealth_cont, miss_Nhh
                  ) %>%
      filter(study!="Odagiri 2016") %>%
      distinct(study, trial, sampleid, dataid, hhid, clusterid, 
               sample, env_date, target, pos, .keep_all = T)
dim(dfull)

unique(dfull$study)
                                      
env_fuhr <- dfull %>% filter(study=="Fuhrmeister 2020")
env_boehm <- dfull %>% filter(study=="Boehm 2016")
env_kwong <- dfull %>% filter(study=="Kwong 2021")
env_stein <- dfull %>% filter(study=="Steinbaum 2019")
env_cap <- dfull %>% filter(study=="Capone 2021")
env_cap2 <- dfull %>% filter(study=="Capone 2022 in prep")
env_hol <- dfull %>% filter(study=="Holcomb 2020")
env_gv <- dfull %>% filter(study=="Reese 2017")



#load all diarrhea datasets
gv <- readRDS(paste0(dropboxDir, "Data/WBK/clean-gv-diar.RDS"))  %>% filter(!is.na(diar7d))%>% mutate(study="Reese 2017") %>% select(study,dataid,clusterid,hhid, child_date, diar7d, sex, age)
odisha <- readRDS(paste0(dropboxDir, "Data/WBK/clean-odisha-diar.RDS")) %>% filter(!is.na(diar7d)) %>% mutate(study="Odagiri 2016") #%>% select(study,dataid,clusterid,hhid, child_date, diar7d)
mapsan <- readRDS(paste0(dropboxDir, "Data/WBK/clean-mapsan-diar.RDS")) %>% filter(!is.na(diar7d)) %>% select(dataid,clusterid,hhid, child_date, diar7d, sex, age)
wbb <- readRDS(paste0(dropboxDir, "Data/WBK/clean-wbb-diar.RDS")) %>% filter(!is.na(diar7d)) %>% select(dataid,clusterid,hhid, child_date, diar7d, sex, agedays) %>% rename(age=agedays)
wbk <- readRDS(paste0(dropboxDir, "Data/WBK/clean-wbk-diar.RDS")) %>% filter(!is.na(diar7d)) %>% rename(dataid=compoundid) %>% mutate(study="Steinbaum 2019") %>% select(study,dataid,clusterid,hhid, child_date, diar7d, sex, aged) %>% rename(age=aged)

table(dfull$study)
fuhr <- wbb %>% mutate(study="Fuhrmeister 2020")
boehm <- wbb %>% mutate(study="Boehm 2016")
kwong <- wbb %>% mutate(study="Kwong 2021")
cap <- mapsan %>% mutate(study="Capone 2021")
cap2 <- mapsan %>% mutate(study="Capone 2022 in prep")
hol <- mapsan %>% mutate(study="Holcomb 2020")

#Function to split data by hh versus compound samples
merge_ch <- function(env, ch){
  ch$ch_data <- 1
  env_compound <- env %>% filter(sample %in% c("LS","Fly","SW","any sample type")) %>% subset(., select = -c(hhid))
  env_hh <- env %>% filter(sample %in% c("S","MH", "CH", "W" ))
  
  d_compound <- left_join(env_compound, ch, by = c("study","dataid","clusterid"))
  d_hh <- NULL
  if(nrow(env_hh)>0){
    d_hh <- left_join(env_hh, ch, by = c("study","dataid","clusterid", "hhid"))
  }
  
  d<-bind_rows(d_compound,d_hh)
  return(d)
}

env_fuhr <- merge_ch(env_fuhr, fuhr) %>% distinct()
env_boehm <- merge_ch(env_boehm, boehm) %>% distinct()
env_kwong <- merge_ch(env_kwong, kwong) %>% distinct()
env_stein <- merge_ch(env_stein, wbk) %>% distinct()
env_cap <- merge_ch(env_cap, cap) %>% distinct()
env_cap2 <- merge_ch(env_cap2, cap2) %>% distinct()
env_hol <- merge_ch(env_hol, hol) %>% distinct()
env_gv <- merge_ch(env_gv, gv) %>% distinct()

head(env_cap)
head(cap)
# 
# dim(dfull)
# env_fuhr <- left_join(env_fuhr, fuhr, by=c("study","dataid","clusterid","hhid")) %>% distinct()
# env_boehm <- left_join(env_boehm, boehm, by=c("study","dataid","clusterid","hhid")) %>% distinct()
# env_kwong <- left_join(env_kwong, kwong, by=c("study","dataid","clusterid","hhid")) %>% distinct()
# env_stein <- left_join(env_stein, wbk, by=c("study","dataid","clusterid","hhid")) %>% distinct()
# env_cap <- left_join(env_cap, cap, by=c("study","dataid","clusterid","hhid")) %>% distinct()
# env_cap2 <- left_join(env_cap2, cap2, by=c("study","dataid","clusterid","hhid")) %>% distinct()
# env_hol <- left_join(env_hol, hol, by=c("study","dataid","clusterid","hhid")) %>% distinct()
# env_gv <- left_join(env_gv, gv, by=c("study","dataid","clusterid","hhid")) %>% distinct()

odisha$childid <- as.character(odisha$childid)
odisha$sex <- factor(odisha$sex)
odisha$Nhh <- factor(odisha$Nhh)
odisha$walls <- factor(odisha$walls)
env_cap$sex <- factor(env_cap$sex)
env_cap2$sex <- factor(env_cap2$sex)
env_hol$sex <- factor(env_hol$sex)
env_gv$sex <- factor(env_gv$sex)
d <- bind_rows(env_fuhr,
               env_boehm,
               env_kwong,
               env_stein,
               env_cap,
               env_cap2,
               env_hol,
               env_gv,
               odisha) %>% filter(sample!="FP") %>% droplevels()
dim(d)
colnames(d)

        
saveRDS(d, file = paste0(dropboxDir, "Data/all-diar.RDS"))

# #temp
df <- d %>% filter(sample=="Fly", target=="Any pathogen", child_date>env_date & child_date-env_date <=31) %>% distinct()
dim(df)



Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#all diarrhea measures
fullres_adj <- NULL
res_diar_adj <- d %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar_adj$sparse <- ifelse(is.na(res_diar_adj$RR), "yes", "no")
res_diar_adj$RR[is.na(res_diar_adj$RR)] <- 1
res_diar_adj <- res_diar_adj %>% mutate(time="All")








#--------------------------------------------------------------------------
#only diarrhea measures within 1 month
#--------------------------------------------------------------------------

df <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds")) %>% filter(sample!="FP") %>% droplevels()

#ensure time ordering of diarrhea (anthro has been set in individual studies)
table(d$diar7d)
d$diar7d[d$child_date <= d$env_date | d$child_date > d$env_date+31] <- NA
table(d$diar7d)


res_diar_adj_1mo <- df %>% group_by(study, sample, target) %>%
  do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial")) 
res_diar_adj_1mo$sparse <- ifelse(is.na(res_diar_adj_1mo$RR), "yes", "no")
res_diar_adj_1mo$RR[is.na(res_diar_adj_1mo$RR)] <- 1
res_diar_adj_1mo <- res_diar_adj_1mo %>% mutate(time="1 month")

#Pool results
res_diar_adj_1mo <- clean_res(res_diar_adj_1mo)
res_diar_adj <- clean_res(res_diar_adj)

unique(res_diar_adj_1mo$study)
res_diar_adj_1mo %>% filter(study=="Capone 2022 in prep", !is.na(coef), target=="Any pathogen")

res_RR_adj1mo <- res_diar_adj_1mo %>% filter(sample_cat!="Sparse data") %>%
  group_by(sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% 
  do(poolRR(.)) %>% do(clean_res(.)) %>% mutate(time="1 month", Y="diar7d")

res_RR_adj <- res_diar_adj %>% filter(sample_cat!="Sparse data") %>%
  group_by(sample, target) %>%
  filter(!is.na(se)) %>% mutate(N=n()) %>%
  filter(N>=4)%>% 
  do(poolRR(.))  %>% do(clean_res(.)) %>% mutate(time="All", Y="diar7d")

fullres_adj <- bind_rows(res_diar_adj, res_RR_adj, res_diar_adj_1mo, res_RR_adj1mo)


#load existing results
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) %>% 
  mutate(time="4 months") %>%
  filter(Y=="diar7d")
adj_RR$sample_cat_f[adj_RR$study=="Pooled"] <- "Any sample type"
fullres_adj <- bind_rows(fullres_adj, adj_RR)

fullres_adj <- fullres_adj %>% filter(RR!=1)

saveRDS(fullres_adj, file=here("results/aim2_sens_diar_time_res.Rds"))


#TEMP
res_diar_adj_1mo %>% filter(sample=="Fly", target=="Any pathogen")
adj_RR %>% filter(sample=="Fly", target=="Any pathogen")
