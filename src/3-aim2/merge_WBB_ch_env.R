

rm(list=ls())
source(here::here("0-config.R"))


#-----------------------------------------------------------
# Merge WBB
#-----------------------------------------------------------
env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study=="Holcomb 2020" ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 

env_wbb <- env %>% filter(trial == "WBB") %>% droplevels(.)

table(env_wbb$study)



wbb <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS")) %>% 
  rename(haz=laz) %>%
  mutate(trial="WBB",
         aged=agedays
  )
table(wbb$study)
table(env_wbb$study)

head(wbb)
head(env_wbb)

table(wbb$round)
table(is.na(wbb$round))

table(env_wbb$round)
table(is.na(env_wbb$round))

#Merge WBB based on sampling round

#1) Fuhrmeister to health outcomes

# Specifically, Erica's samples were collected during Round 3 and Round 4 of the R01. 
#Therefore, if you merge them with the R01 health data with a one-round offset (i.e., match the R3 environmental data to the R4 
#health data, the R4 environmental data to the R5 health data), 
#then each environmental datapoint should have a subsequent diarrhea datapoint within the next 3ish months.
env_fuhr <- env_wbb %>% filter(study=="Fuhrmeister 2020") %>% mutate(merge_round=as.numeric(round))
diar_fuhr <- wbb %>% filter(round %in% c(4,5), !is.na(diar7d)) %>% 
  mutate(merge_round=as.numeric(round)-1, study="Fuhrmeister 2020") %>%
  select(study, block, clusterid, dataid, hhid, 
         merge_round, child_date, agedays, sex,             
         childid, diar7d,momage, hfiacat)
table(env_fuhr$round)
table(diar_fuhr$round)
table(diar_fuhr$merge_round)
table(is.na(diar_fuhr$diar7d))
table(env_fuhr$sample, env_fuhr$round)


dim(env_fuhr)
dim(diar_fuhr)
diar_env_fuhr <- full_join(env_fuhr, diar_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round","momage","hfiacat")) %>% filter(!is.na(pos)) %>% arrange(sampleid, dataid,clusterid, hhid,childid, merge_round)
#diar_env_fuhr <- left_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round"))
colnames(diar_env_fuhr)
dim(diar_env_fuhr)
tab <- table(diar_env_fuhr$pos, diar_env_fuhr$diar7d)
prop.table(tab,1)
(tab[2,2] * tab[1,1])/(tab[2,1] * tab[1,2])

#Get endline anthropometry
# Erica's R3 and R4 sampling definitely preceded the main trial endline anthro measurements so there should not be missings there either.
anthro_fuhr <- wbb %>% filter(round == "endline", !is.na(haz)|!is.na(waz)|!is.na(whz)) %>% 
  mutate(study="Fuhrmeister 2020") %>%
  select(study,block, clusterid, dataid, hhid, child_date_anthro, agedays, sex,             
         childid, haz, waz, whz, momage, hfiacat) %>%
  rename(agedays_anthro=agedays)

dim(diar_env_fuhr)
dim(anthro_fuhr)
ch_env_fuhr <- full_join(diar_env_fuhr, anthro_fuhr, by = c("study","dataid","clusterid", "hhid","sex","momage","hfiacat", "childid","block")) %>%  filter(!is.na(pos) & !(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) 
#diar_env_fuhr <- left_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round"))
dim(ch_env_fuhr)
colnames(ch_env_fuhr)

head(ch_env_fuhr)

table(is.na(ch_env_fuhr$sex))
#2) Boehm to health outcomes
# The Boehm sampling was done as part of the World Bank study which collected its own diarrhea data one week after the environmental samples 
# (and Amy shared that dataset with you) so those should be perfectly matched.


env_boehm <- env_wbb %>% filter(study=="Boehm 2016") 

#world bank diarrhea
diar_boehm <- haven::read_dta(paste0(dropboxDir,"Data/WBB/fecal_pathways_1_childhealth_micro_submit.dta")) %>% mutate(round="World Bank")
head(diar_boehm)
colnames(diar_boehm)
table(diar_boehm$loose7dprev) #diarrhea measure to use
diar_boehm <- diar_boehm %>% rename(diar7d=loose7dprev,
                                    clusterid=cluster_id,
                                    dataid=pid,
                                    childid=cid,
                                    agedays=age) %>%
  select(clusterid, dataid, 
         #hhid,  child_date_anthro, sex,    
         agedays,          
         childid, intday, intmo, diar7d,
         round) %>%
  mutate(childid=gsub("_","",childid),
         childid=gsub("tt","t2",childid),
         childid=gsub("t","t1",childid),
         childid=toupper(childid),
         study="Boehm 2016")


table(env_boehm$round)
table(diar_boehm$round)
table(is.na(diar_boehm$diar7d))
table(env_boehm$sample, env_boehm$round)


dim(env_boehm)
dim(diar_boehm)




#diar_env_boehm <- full_join(env_boehm, diar_boehm, by = c("dataid","clusterid", "hhid","merge_round","momage","hfiacat")) %>% filter(!is.na(pos) | !is.na(diar7d)) %>% arrange(sampleid, dataid,clusterid, hhid,childid, merge_round)
diar_env_boehm <- full_join(env_boehm, diar_boehm, by = c("study","dataid","clusterid","round")) %>% filter(!is.na(pos)) %>% arrange(sampleid, dataid,clusterid,childid)
#diar_env_boehm <- left_join(env_boehm, diar_boehm, by = c("dataid","clusterid", "hhid","merge_round"))
colnames(diar_env_boehm)
dim(diar_env_boehm)
table(diar_env_boehm$pos, diar_env_boehm$diar7d)


#Get endline anthropometry
# The timing of the Boehm sampling was on average 4 months after the initiation of interventions so it preceded the main trial
# midline anthro measurements and should be perfectly matched to those as well.
anthro_boehm <- wbb %>% filter(round == "midline", !is.na(haz)|!is.na(waz)|!is.na(whz)) %>% 
  select(block, clusterid, dataid, hhid, child_date_anthro, agedays, sex,             
         childid, haz, waz, whz, momage, hfiacat) %>%
  rename(agedays_anthro=agedays) %>%
  mutate(study="Boehm 2016")


dim(diar_env_boehm)
dim(anthro_boehm)

table(diar_env_boehm$clusterid)
table(anthro_boehm$clusterid)

temp1 <- diar_env_boehm %>% filter(dataid==5202)
temp2 <- anthro_boehm %>% filter(dataid==5202)

ch_env_boehm <- full_join(diar_env_boehm, anthro_boehm, by = c("study","dataid","clusterid", "hhid", "childid", "momage", "hfiacat" )) %>% filter(!is.na(pos) & !(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) 
#ch_env_boehm <- full_join(diar_env_boehm, anthro_boehm, by = c("study","dataid","clusterid", "hhid", "childid")) %>% filter(!is.na(pos) & !(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) 
#diar_env_boehm <- left_join(env_boehm, diar_boehm, by = c("dataid","clusterid", "hhid","merge_round"))
table(is.na(ch_env_boehm$haz))
dim(ch_env_boehm)
colnames(ch_env_boehm)

head(ch_env_boehm)

#3) Kwong to health outcomes- collected at endline
env_kwong <- env_wbb %>% filter(study=="Kwong 2021")
head(env_kwong)

#get endline WBB health data
WBB_main_health <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS"))
WBB_main_health <- WBB_main_health %>% filter(round=="endline")
head(WBB_main_health)
WBB_main_diar <- WBB_main_health %>% filter(!is.na(diar7d)) %>% subset(., select=c(block, clusterid, dataid, hhid, child_date, agedays,sex, diar7d))

head(env_kwong) 
table(env_kwong$pos)
table(WBB_main_diar$diar7d)

dim(env_kwong)
dim(WBB_main_diar)
env_diar_kwong<- left_join(env_kwong, WBB_main_diar, by=c("dataid",  "hhid", "clusterid"))
colnames(env_diar_kwong)
dim(env_diar_kwong)
summary(as.numeric(env_diar_kwong$child_date - env_diar_kwong$env_date))
table(env_diar_kwong$pos, env_diar_kwong$diar7d)
table(env_diar_kwong$pos, is.na(env_diar_kwong$diar7d))

#Get proportion dropped for days off
prop.table(table(env_diar_kwong$child_date>=env_diar_kwong$env_date)) * 100
prop.table(table(env_diar_kwong$child_date-env_diar_kwong$env_date > 93)) * 100
prop.table(table(env_diar_kwong$child_date-env_diar_kwong$env_date > 93 & env_diar_kwong$child_date>=env_diar_kwong$env_date)) * 100

summary(as.numeric(env_diar_kwong$child_date-env_diar_kwong$env_date))

#weekly difference
table(floor(as.numeric(env_diar_kwong$child_date-env_diar_kwong$env_date)/7))


env_diar_kwong <- env_diar_kwong %>% 
  mutate(diar7d = ifelse(child_date<env_date, NA, diar7d),
         diar7d = ifelse(child_date-env_date > 93, NA, diar7d),
         diar7d = ifelse(is.na(child_date)|is.na(env_date), NA, diar7d))
table(env_diar_kwong$pos, env_diar_kwong$diar7d)
table(env_diar_kwong$pos, is.na(env_diar_kwong$diar7d))



WBB_main_anthro <- WBB_main_health %>% filter(!is.na(laz)|!is.na(whz)|!is.na(waz)) %>% 
  subset(., select=c(block, clusterid, dataid, hhid, child_date_anthro, agedays,sex, laz, whz, waz)) %>%
  rename(haz=laz)


dim(env_diar_kwong)
dim(WBB_main_anthro)
ch_env_kwong<- left_join(env_diar_kwong, WBB_main_anthro, by=c("block","dataid",  "hhid", "clusterid","sex","agedays"))
colnames(ch_env_kwong)
dim(ch_env_kwong)
table(ch_env_kwong$pos, is.na(ch_env_kwong$haz))

ch_env_kwong <- ch_env_kwong %>% 
  mutate(haz = ifelse(child_date_anthro<env_date, NA, haz),
         haz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, haz),
         whz = ifelse(child_date_anthro<env_date, NA, whz),
         whz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, whz),
         waz = ifelse(child_date_anthro<env_date, NA, waz),
         waz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, waz))

table(ch_env_kwong$pos, is.na(ch_env_kwong$haz))


# In other words, for Erica and Boehm, I would rely on the R01 and World Bank diarrhea datasets, respectively, rather than the main trial. 
# As for the anthro data, the main trial midline should fall after Boehm and the main trial endline should fall after Erica within the time 
# window we allow (environmental sampling anytime before anthro during child's lifetime).
# For the Erica+R01 data, the key will be to offset them by one round (match one round of env data to the following round of diarrhea data 
# to allow ~3 months in between). I suggest doing this merge separately from your combined mega merge.
# Likewise, the World Bank dataset has its own diarrhea -- just make sure you use the prospective (and not concurrent) diarrhea variable 
# when you match it to the Boehm env data for a given HH. That will automatically ensure that the env data precedes the diarrhea data by 4-10 days (as the study was designed to revisit HHs to collect diarrhea data 4-10 days after environmental sampling).

# -make table of numbers before and after dropping for timing and share

env_wbb <- bind_rows(ch_env_fuhr, ch_env_boehm, ch_env_kwong)
head(env_wbb)

env_wbb %>% group_by(study) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_haz=sum(!is.na(haz)))


env_wbb %>% group_by(study, sample, target) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_pos_diar=sum(diar7d==1, na.rm=T), N_pos_env_diar=sum(pos==1 & diar7d==1, na.rm=T), N_haz=sum(!is.na(haz)))


saveRDS(env_wbb, file=paste0(dropboxDir,"Data/WBB_env_CH_data.rds"))
