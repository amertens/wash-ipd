
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
                    study=="Odagiri 2016" ~ "Odisha"),
          merge_round=as.numeric(round)+1) 

env <- env %>% filter(trial == "Gram Vikas") %>% droplevels(.)


#Note that the GV lab data had diarrhea/anthro merged in, but 
ch <- readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds"))
dim(ch)
ch <- ch %>% 
  mutate(study="Reese 2017",
         trial="Gram Vikas",
         diar7d=as.numeric(dia7),
         haz=as.numeric(haz),
         whz=as.numeric(whz),
         #momedu=wom.edu4,
         childid=hh_mid,
         child_date =env_date,
         merge_round=round
         ) %>%
  subset(., select = c(clusterid, merge_round,      hhid,     childid,     hh_st,          
                       haz, whz, sex, age, study, trial,     
                       dataid, diar7d, child_date)) %>%
  filter(!(is.na(diar7d) & is.na(haz) & is.na(whz))) %>%
  distinct() %>% mutate(ch_data=1)

dim(ch)
dim(ch %>% distinct(clusterid,  merge_round,      hhid,
                    haz, whz, sex, age,     
                    dataid, diar7d, child_date))
dim(ch %>% distinct(clusterid,      hhid,
                    haz, whz, sex, age,     
                    dataid, diar7d))
dim(ch %>% distinct(clusterid,  merge_round, hhid, dataid))
dim(ch %>% distinct(clusterid, hhid, dataid))
dim(ch %>% distinct(clusterid, hhid))
dim(ch %>% distinct(clusterid, hhid, haz, whz, sex, age,  diar7d))
dim(ch %>% distinct(clusterid, hhid, haz, sex))

table(ch$merge_round)

ch %>% group_by(dataid, hhid, merge_round) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))
ch %>% group_by(merge_round, hhid, childid, child_date) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))
ch %>% group_by(merge_round, hhid, childid) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))
ch %>% group_by( hhid, childid) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))
ch %>% group_by(dataid) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))
ch %>% group_by(hhid) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))

#How to uniquely id children across rounds
#Make sure I have all the rounds I need

#We collected anthropometric measurements for children under five during round 3 (February-June 2016), according to WHO standard methods
table(is.na(ch$haz), ch$merge_round)
table(ch$diar7d, ch$merge_round)
#Field workers collected stool samples in round 2 (October 2015-January 2016) from all household members in a randomly selected subset of 500 households to assess prevalence of common soil-transmitted helminths.

#------------------------------------------
# Note: CH and env samples collected in same visit
# Rounds are 4 months apart, so use contemporaneous diarrhea
# And next round's anthro
#------------------------------------------

# ch <- ch %>% rename(diar7d_contemporaneous=diar7d,
#                     haz_contemporaneous=haz,
#                     whz_contemporaneous=whz) %>%
#             arrange(childid, dataid, hhid, round) %>%
#             group_by(childid, dataid, hhid) %>%
#             mutate(diar7d=lead(diar7d_contemporaneous),
#                    haz=lead(haz_contemporaneous),
#                    whz=lead(whz_contemporaneous),
#                    child_date=lead(env_date)) %>%
#             filter(!is.na(child_date)) %>%
#             mutate(date_diff=as.numeric(child_date-env_date))
# 
# summary(ch$date_diff)
# summary(ch$date_diff[!is.na(ch$diar7d)])
# summary(ch$date_diff[!is.na(ch$haz)])
# table(ch$date_diff[!is.na(ch$diar7d)] <93 & ch$date_diff[!is.na(ch$diar7d)] >0)
# #table(ch1$date_diff[!is.na(ch1$diar7d)] <93 & ch1$date_diff[!is.na(ch1$diar7d)] >0)

#Need to merge contemporaneous diarhea measures and leading anthro measures



head(ch)

table(ch$round)
env$round <- as.numeric(env$round)

gv_res <- data.frame(
  study = "reese",
  env_samples_before_merge = nrow(env %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% ungroup() %>% filter(!is.na(diar7d)) %>% distinct(dataid, hhid, child_date, age,    sex, childid,     hh_st, diar7d)),
  haz_samples_before_merge = nrow(ch %>% ungroup() %>% filter(!is.na(haz)) %>% distinct(dataid, hhid, child_date, age,    sex, childid,     hh_st, haz))
)

#Note: merging on round is fine, as samples and health outcomes collected on the same day
#Added note: and I used the lead outcome time
dim(env)
dim(ch)
d <- full_join(env, ch, by = c("trial","study","dataid","clusterid","hhid","merge_round"))
dim(d)
d <- d %>% filter(!is.na(sample), !is.na(ch_data))
dim(d)

table(d$round)
table(d$merge_round)

colnames(d)


table(d$pos, d$diar7d)
summary(as.numeric(d$child_date- d$env_date))



gv_res$env_samples_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
gv_res$env_HH_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
gv_res$diar_samples_after_merge <- nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(child_date, dataid, hhid, age,    sex, diar7d))
gv_res$haz_samples_after_merge <- nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(child_date, dataid, hhid, age,    sex, haz))
gv_res$samples_with_diar_after_merge <- nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid, hhid,))
gv_res$samples_with_haz_after_merge <- nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid, hhid))


#Get proportion dropped for days off
prop.table(table(d$child_date>=d$env_date))
prop.table(table(d$child_date-d$env_date > 93))

date_diff <- d %>% mutate(date_diff = child_date-env_date) %>% select(study, sampleid, target, dataid, round, hhid, date_diff, diar7d, haz) %>% distinct()


d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
    diar7d = ifelse(child_date-env_date > 93, NA, diar7d))
table(d$pos, d$diar7d)
table(d$pos, !is.na(d$haz))




gv_res$diar_samples_date_dropped <- gv_res$samples_with_diar_after_merge - nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(childid, sampleid, dataid, hhid))
gv_res$haz_samples_date_dropped <- gv_res$samples_with_haz_after_merge - nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(childid, sampleid, dataid, hhid))



gv_res$percent_diar_samples_dropped <- 100 - gv_res$diar_samples_after_merge/gv_res$diar_samples_before_merge * 100
gv_res$percent_haz_samples_dropped <- 100 - gv_res$haz_samples_after_merge/gv_res$haz_samples_before_merge * 100
gv_res$percent_env_samples_with_diar <- gv_res$samples_with_diar_after_merge/gv_res$env_samples_before_merge  * 100
gv_res$percent_env_samples_with_haz <-  gv_res$samples_with_haz_after_merge/gv_res$env_samples_before_merge  * 100
gv_res$diar_samples_date_dropped[is.na(gv_res$diar_samples_date_dropped )] <- 0
gv_res$haz_samples_date_dropped[is.na(gv_res$haz_samples_date_dropped )] <- 0

gv_res <- gv_res %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)




saveRDS(d, file=paste0(dropboxDir,"Data/gv_env_CH_data.rds"))
saveRDS(gv_res, file=paste0(here(),"/results/gv_merge_Ns.rds"))
saveRDS(date_diff, file=paste0(here(),"/results/gv_date_diff.rds"))
