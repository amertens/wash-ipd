
rm(list=ls())
source(here::here("0-config.R"))

#make full env. dataset
dfull <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))

colnames(dfull)

dim(dfull)
dfull <- dfull %>% select(study, trial, sampleid, dataid, hhid, clusterid, 
                  sample, env_date, target, pos, agedays, hhwealth, sex, age, hfiacat, 
                  momage, hhwealth_cont, Nhh, nrooms, walls, roof, 
                  floor, elec, dadagri, landacre, landown, momedu, 
                  tr, age_anthro, miss_sex, miss_age, miss_landacre, miss_age_anthro, 
                  miss_momage, miss_hhwealth_cont, miss_Nhh) %>%
      filter(study!="Odagiri 2016") %>%
      distinct()
dim(dfull)

unique(dfull$study)
                                      
env_fuhr <- dfull %>% filter(study=="Fuhrmeister 2020")
env_boehm <- dfull %>% filter(study=="Boehm 2016")
env_kwong <- dfull %>% filter(study=="Kwong 2021")
env_stein <- dfull %>% filter(study=="Steinbaum 2019")
env_cap <- dfull %>% filter(study=="Capone 2021")
env_cap2 <- dfull %>% filter(study=="Capone 2021 in prep")
env_hol <- dfull %>% filter(study=="Holcomb 2020")
env_gv <- dfull %>% filter(study=="Reese 2017")


#load all diarrhea datasets
gv <- readRDS(paste0(dropboxDir, "Data/WBK/clean-gv-diar.RDS")) %>% mutate(study="Reese 2017") %>% select(study,dataid,clusterid,hhid, child_date, diar7d)
odisha <- readRDS(paste0(dropboxDir, "Data/WBK/clean-odisha-diar.RDS")) %>% mutate(study="Odagiri 2016") #%>% select(study,dataid,clusterid,hhid, child_date, diar7d)
mapsan <- readRDS(paste0(dropboxDir, "Data/WBK/clean-mapsan-diar.RDS")) %>% select(dataid,clusterid,hhid, child_date, diar7d)
wbb <- readRDS(paste0(dropboxDir, "Data/WBK/clean-wbb-diar.RDS")) %>% select(dataid,clusterid,hhid, child_date, diar7d)
wbk <- readRDS(paste0(dropboxDir, "Data/WBK/clean-wbk-diar.RDS")) %>% rename(dataid=compoundid) %>% mutate(study="Steinbaum 2019") %>% select(study,dataid,clusterid,hhid, child_date, diar7d)

table(dfull$study)
fuhr <- wbb %>% mutate(study="Fuhrmeister 2020")
boehm <- wbb %>% mutate(study="Boehm 2016")
kwong <- wbb %>% mutate(study="Kwong 2021")
cap <- mapsan %>% mutate(study="Capone 2021")
cap2 <- mapsan %>% mutate(study="Capone 2021 in prep")
hol <- mapsan %>% mutate(study="Holcomb 2020")


dim(dfull)
env_fuhr <- left_join(env_fuhr, fuhr, by=c("study","dataid","clusterid","hhid"))
env_boehm <- left_join(env_boehm, boehm, by=c("study","dataid","clusterid","hhid"))
env_kwong <- left_join(env_kwong, kwong, by=c("study","dataid","clusterid","hhid"))
env_stein <- left_join(env_stein, wbk, by=c("study","dataid","clusterid","hhid"))
env_cap <- left_join(env_cap, cap, by=c("study","dataid","clusterid","hhid"))
env_cap2 <- left_join(env_cap2, cap2, by=c("study","dataid","clusterid","hhid"))
env_hol <- left_join(env_hol, hol, by=c("study","dataid","clusterid","hhid"))
env_gv <- left_join(env_gv, gv, by=c("study","dataid","clusterid","hhid"))

odisha$childid <- as.character(odisha$childid)
odisha$sex <- factor(odisha$sex)
odisha$Nhh <- factor(odisha$Nhh)
odisha$walls <- factor(odisha$walls)
d <- bind_rows(env_fuhr,
               env_boehm,
               env_kwong,
               env_stein,
               env_cap,
               env_cap2,
               env_hol,
               env_gv,
               odisha)
dim(d)
colnames(d)
        
saveRDS(d, file = paste0(dropboxDir, "Data/all-diar.RDS"))


