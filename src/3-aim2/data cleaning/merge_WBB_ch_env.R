

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

env_wbb <- env %>% filter(trial == "WBB", !is.na(pos)) %>% droplevels(.)

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
diar_fuhr <- wbb %>% filter(round %in% c(4,5),  !is.na(diar7d)) %>% 
  mutate(merge_round=as.numeric(round)-1, study="Fuhrmeister 2020") %>%
  select(study, block, clusterid, dataid, hhid, 
         merge_round, child_date, agedays, sex,             
         childid, diar7d,momage, hfiacat)

#Get endline anthropometry
# Erica's R3 and R4 sampling definitely preceded the main trial endline anthro measurements so there should not be missings there either.
anthro_fuhr <- wbb %>% filter(round == "endline", !is.na(haz)|!is.na(waz)|!is.na(whz)) %>% 
  mutate(study="Fuhrmeister 2020") %>%
  select(study,block, clusterid, dataid, hhid, child_date_anthro, agedays, sex,             
         childid, haz, waz, whz, momage, hfiacat) %>%
  rename(agedays_anthro=agedays)

#Tabulate numbers before merge
fuhr_res <- data.frame(
  study = "fuhrmeister",
  env_samples_before_merge = nrow(env_fuhr %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env_fuhr %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(diar_fuhr %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, merge_round, child_date, agedays,    sex, childid, diar7d, merge_round)),
  haz_samples_before_merge = nrow(anthro_fuhr %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, agedays_anthro,    sex, childid, haz))
)
table(env_fuhr$round)
table(diar_fuhr$round)
table(diar_fuhr$merge_round)
table(is.na(diar_fuhr$diar7d))
table(env_fuhr$sample, env_fuhr$round)



#Merge CH samples together
dim(anthro_fuhr)
anthro_fuhr <- bind_rows(anthro_fuhr %>% mutate(merge_round=3),
                          anthro_fuhr %>% mutate(merge_round=4))
dim(anthro_fuhr)
dim(diar_fuhr)
ch_fuhr <- full_join(diar_fuhr, anthro_fuhr, by = c("study","dataid","clusterid", "childid", "hhid","block","merge_round","sex","momage","hfiacat")) %>% filter(!(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) %>%
  distinct(study, dataid, clusterid, childid, merge_round, .keep_all = T)
dim(ch_fuhr)
table(1*!is.na(ch_fuhr$diar7d), !is.na(ch_fuhr$haz))


# dim(env_fuhr)
# dim(diar_fuhr)
# diar_env_fuhr <- full_join(env_fuhr, diar_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round","momage","hfiacat")) %>% filter(!is.na(pos)) %>% arrange(sampleid, dataid,clusterid, hhid,childid, merge_round)
# #diar_env_fuhr <- left_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round"))
# colnames(diar_env_fuhr)
# 
# #Find env. samples without diarrhea 
# temp <- anti_join(env_fuhr, diar_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round","momage","hfiacat")) %>% filter(!is.na(pos)) %>% 
#   distinct(study,dataid,clusterid, hhid,merge_round)
# temp2 <- anti_join(env_fuhr, diar_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round")) %>% filter(!is.na(pos)) %>% 
#   distinct(study,dataid,clusterid, hhid,merge_round)
# wbb %>% filter(dataid=="10802" & round %in% c(4,5))
# temp2[1,]



dim(env_fuhr)
dim(ch_fuhr)
#ch_env_fuhr <- full_join(env_fuhr, ch_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round","sex","momage","hfiacat", "childid","block")) %>%  filter(!is.na(pos) & !(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) 
ch_env_fuhr <- full_join(env_fuhr, ch_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round","momage","hfiacat")) %>%  filter(!is.na(pos) & !(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) %>% distinct(.)
#diar_env_fuhr <- left_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round"))
dim(ch_env_fuhr)
colnames(ch_env_fuhr)

head(ch_env_fuhr)

fuhr_res$env_samples_after_merge <- nrow(ch_env_fuhr %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
fuhr_res$env_HH_after_merge <- nrow(ch_env_fuhr %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
fuhr_res$diar_samples_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, merge_round, child_date, agedays,    sex, childid, diar7d, merge_round))
fuhr_res$haz_samples_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, merge_round, child_date, agedays,    sex, childid, haz, merge_round))
fuhr_res$samples_with_diar_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
fuhr_res$samples_with_haz_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
fuhr_res$samples_with_ch_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))




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



# Merge with endline anthropometry
# The timing of the Boehm sampling was on average 4 months after the initiation of interventions so it preceded the main trial
# midline anthro measurements and should be perfectly matched to those as well.
anthro_boehm <- wbb %>% filter(round == "midline", !is.na(haz)|!is.na(waz)|!is.na(whz)) %>% 
  select(block, clusterid, dataid, hhid, child_date_anthro, agedays, sex,             
         childid, haz, waz, whz, momage, hfiacat) %>%
  rename(agedays_anthro=agedays) %>%
  mutate(study="Boehm 2016", round="World Bank")

#Check diarrhea merging
ch_boehm <- left_join(diar_boehm, anthro_boehm, by = c("study","dataid","clusterid", "childid", "round")) %>% filter(!(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz)))
dim(ch_boehm)

#env <- env_boehm %>% filter(target=="Any pathogen", sample=="any sample type")
env <- env_boehm %>% distinct(study,dataid,clusterid, round)
dim(diar_boehm)
dim(env)
diar_merge_boehm <- left_join(env, diar_boehm, by = c("study","dataid","clusterid", "round"))
dim(diar_merge_boehm)
table(is.na(diar_merge_boehm$diar7d))
prop.table(table(is.na(diar_merge_boehm$diar7d)))*100
diar_merge_boehm$dataid[is.na(diar_merge_boehm$diar7d)]
diar_merge_boehm[is.na(diar_merge_boehm$diar7d),]

env_failed <- anti_join(env, diar_boehm, by = c("study","dataid","clusterid", "round"))
diar_failed <- anti_join(diar_boehm, env, by = c("study","dataid","clusterid", "round"))
env_boehm[env_boehm$dataid==301,]


table(env_boehm$round)
table(diar_boehm$round)
table(is.na(diar_boehm$diar7d))
table(env_boehm$sample, env_boehm$round)

boehm_res <- data.frame(
  study = "boehm",
  env_samples_before_merge = nrow(env_boehm %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env_boehm %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(diar_boehm %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, round, intday, intmo, agedays, childid, diar7d)),
  haz_samples_before_merge = nrow(anthro_boehm %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, agedays_anthro,    sex, childid, haz))
)


dim(env_boehm)
dim(diar_boehm)


ch_env_boehm <- full_join(env_boehm, ch_boehm, by = c("study","dataid","clusterid", "hhid","round","momage","hfiacat")) %>% filter(!is.na(pos)) %>% arrange(sampleid, dataid,clusterid, hhid,childid)
table(is.na(ch_env_boehm$haz))
dim(ch_env_boehm)
colnames(ch_env_boehm)

head(ch_env_boehm)

boehm_res$env_samples_after_merge <- nrow(ch_env_boehm %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
boehm_res$env_HH_after_merge <- nrow(ch_env_boehm %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
boehm_res$diar_samples_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, childid, diar7d))
boehm_res$haz_samples_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, childid, haz))
boehm_res$samples_with_diar_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
boehm_res$samples_with_haz_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
boehm_res$samples_with_ch_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))

boehm_res$env_samples_before_merge
boehm_res$samples_with_diar_after_merge
boehm_res$samples_with_diar_after_merge/boehm_res$env_samples_before_merge  * 100


#3) Kwong to health outcomes- collected at endline
env_kwong <- env_wbb %>% filter(study=="Kwong 2021")
head(env_kwong)

#get endline WBB health data
WBB_main_health <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS"))
WBB_main_health <- WBB_main_health %>% filter(round=="endline")
head(WBB_main_health)
WBB_main_diar <- WBB_main_health %>% filter(!is.na(diar7d)) %>% subset(., select=c(block, clusterid, dataid, hhid, childid , child_date, agedays,sex, diar7d))


WBB_main_anthro <- WBB_main_health %>% filter(!is.na(laz)|!is.na(whz)|!is.na(waz)) %>%
  subset(., select=c(block, clusterid, dataid, hhid, childid , child_date_anthro, agedays,sex, laz, whz, waz)) %>%
  rename(haz=laz, agedays_anthro=agedays)

ch_kwong<- left_join(WBB_main_diar, WBB_main_anthro, by=c("block","dataid",  "hhid", "clusterid","childid","sex"))


#Tabulate N's before merge
kwong_res <- data.frame(
  study = "kwong",
  env_samples_before_merge = nrow(env_kwong %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env_kwong %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(WBB_main_diar %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, agedays, childid, diar7d)),
  haz_samples_before_merge = nrow(WBB_main_anthro %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date_anthro, agedays_anthro,    sex, haz))
)


colnames(env_kwong)
dim(env_kwong)
dim(ch_kwong)
ch_env_kwong<- left_join(env_kwong, ch_kwong, by=c("dataid",  "hhid", "clusterid")) %>% filter(!(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz)))
colnames(ch_env_kwong)
dim(ch_env_kwong)
table(ch_env_kwong$pos, is.na(ch_env_kwong$haz))


table(ch_env_kwong$pos, is.na(ch_env_kwong$haz))

kwong_res$env_samples_after_merge <- nrow(ch_env_kwong %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
kwong_res$env_HH_after_merge <- nrow(ch_env_kwong %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
kwong_res$diar_samples_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, diar7d))
kwong_res$haz_samples_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, haz))
kwong_res$samples_with_diar_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
kwong_res$samples_with_haz_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
kwong_res$samples_with_ch_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))


date_diff <- ch_env_kwong %>% mutate(date_diff = child_date-env_date) %>% select(study, sampleid, target, dataid, round, hhid, date_diff, diar7d, haz) %>% distinct()


ch_env_kwong <- ch_env_kwong %>%
  mutate(diar7d = ifelse(child_date<env_date, NA, diar7d),
         diar7d = ifelse(child_date-env_date > 124, NA, diar7d),
         diar7d = ifelse(is.na(child_date)|is.na(env_date), NA, diar7d))

ch_env_kwong <- ch_env_kwong %>% 
  mutate(haz = ifelse(child_date_anthro<env_date, NA, haz),
         haz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, haz),
         whz = ifelse(child_date_anthro<env_date, NA, whz),
         whz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, whz),
         waz = ifelse(child_date_anthro<env_date, NA, waz),
         waz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, waz))

kwong_res$diar_samples_date_dropped <- kwong_res$samples_with_diar_after_merge - nrow(ch_env_kwong %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid, agedays,    sex, diar7d))
kwong_res$haz_samples_date_dropped <- kwong_res$samples_with_haz_after_merge - nrow(ch_env_kwong %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid, agedays,    sex, haz))



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

#Save N's
WBB_Ns <- bind_rows(fuhr_res, boehm_res, kwong_res)
WBB_Ns$percent_diar_samples_dropped <- 100 - WBB_Ns$diar_samples_after_merge/WBB_Ns$diar_samples_before_merge * 100
WBB_Ns$percent_haz_samples_dropped <- 100 - WBB_Ns$haz_samples_after_merge/WBB_Ns$haz_samples_before_merge * 100

WBB_Ns$percent_env_samples_with_diar <- WBB_Ns$samples_with_diar_after_merge/WBB_Ns$env_samples_before_merge  * 100
WBB_Ns$percent_env_samples_with_haz <-  WBB_Ns$samples_with_haz_after_merge/WBB_Ns$env_samples_before_merge  * 100
WBB_Ns$percent_env_samples_with_ch <-  WBB_Ns$samples_with_ch_after_merge/WBB_Ns$env_samples_before_merge  * 100

WBB_Ns$diar_samples_date_dropped[is.na(WBB_Ns$diar_samples_date_dropped )] <- 0
WBB_Ns$haz_samples_date_dropped[is.na(WBB_Ns$haz_samples_date_dropped )] <- 0
 
WBB_Ns <- WBB_Ns %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)


saveRDS(WBB_Ns, file=paste0(here(),"/results/WBB_merge_Ns.rds"))
saveRDS(date_diff, file=paste0(here(),"/results/WBB_date_diff.rds"))

#Note!!! 
#Need to get the Kwong numbers before dropping based on dates
#And then get the numbers after dropping.
