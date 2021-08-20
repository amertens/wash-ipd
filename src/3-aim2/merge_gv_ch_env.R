
rm(list=ls())
source(here::here("0-config.R"))


#-----------------------------------------------------------
# Merge WBK
#-----------------------------------------------------------

env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study=="Holcomb 2020" ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 

env <- env %>% filter(trial == "Gram Vikas") %>% droplevels(.)


#Note that the GV lab data had diarrhea/anthro merged in, but 
ch <- readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds")) %>% 
  mutate(study="Reese 2017",
         trial="Gram Vikas",
         dataid=as.numeric(factor(hh_mid))*10+round,
         #dia7=ifelse(dia7==99,NA,dia7),
         #diar7d=ifelse(dia7==2,0,1),
         diar7d=as.numeric(dia7),
         haz=as.numeric(haz),
         whz=as.numeric(whz),
         #momedu=wom.edu4,
         child_date =env_date) %>%
  subset(., select = c(clusterid,  round,      hhid,     hh_mid,     hh_st,          
                       haz, whz, sex, age, study, trial,     
                       dataid, diar7d, child_date)) %>%
  distinct() %>% mutate(ch_data=1)

ch %>% group_by(dataid, round) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))

head(ch)

table(ch$round)
env$round <- as.numeric(env$round)

head(env)
head(ch)
dim(env)
dim(ch)

#Note: merging on round is fine, as samples and health outcomes collected on the same day
d <- full_join(env, ch, by = c("trial","study","dataid","clusterid","hhid","round"))
dim(d)
d <- d %>% filter(!is.na(sample), !is.na(ch_data))
dim(d)

colnames(d)


table(d$pos, d$diar7d)
summary(as.numeric(d$child_date- d$env_date))

d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
    diar7d = ifelse(child_date-env_date > 93, NA, diar7d))
table(d$pos, d$diar7d)
table(d$pos, !is.na(d$haz))





saveRDS(d, file=paste0(dropboxDir,"Data/gv_env_CH_data.rds"))
