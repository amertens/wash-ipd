
#Merge full env. with child health to see percent dropped 
rm(list=ls())
source(here::here("0-config.R"))
env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
ch <- readRDS(file=paste0(dropboxDir,"Data/merged_env_CH_data.rds"))

head(env)
head(ch)

dim(env)
env <- env %>% filter(!grepl("any",sample), !grepl("Any",target)) %>%
  subset(., select = c(study, trial, sampleid, dataid,  hhid, clusterid)) %>% distinct() %>%
  mutate(env_data=1)
dim(env)


dim(ch)
ch <- ch %>% 
  filter(!grepl("any",sample), !grepl("Any",target),!is.na(diar7d)|!is.na(haz)) %>%
  subset(., select = c(study, trial, sampleid, dataid,  hhid, clusterid)) %>%
  distinct() %>%
  mutate(ch_data=1)
dim(ch)

dim(env)
#d <- left_join(env, ch, by = c("study","trial", "dataid", "hhid", "clusterid", "sampleid"))
d <- left_join(env, ch, by = c("study","trial", "dataid", "clusterid", "sampleid"))
dim(d)

d$ch_data[d$study=="Odagiri 2016"] <- 1

round(prop.table(table(!is.na(d$ch_data))) *100,1)[2]
round(prop.table(table(d$study,!is.na(d$ch_data)),1) *100,1)
table(d$study,!is.na(d$ch_data))



dim(ch)
d <- left_join(ch, env, by = c("study","trial", "dataid", "hhid", "clusterid", "sampleid"))
#d <- left_join(ch, env, by = c("study","trial", "dataid", "clusterid", "sampleid"))
dim(d)

#d$env_data[d$study=="Odagiri 2016"] <- 1

round(prop.table(table(!is.na(d$env_data))) *100,1)[2]
round(prop.table(table(d$study,!is.na(d$env_data)),1) *100,1)
table(d$study,!is.na(d$env_data))







