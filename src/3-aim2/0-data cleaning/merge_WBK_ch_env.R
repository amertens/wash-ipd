
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

env_wbk <- env %>% filter(trial == "WBK") %>% droplevels(.)



wbk_diar <- read.csv(paste0(dropboxDir, "Data/WBK/washb-kenya-diar.csv"))
wbk_anthro <- read_dta(paste0(dropboxDir, "Data/WBK/washb-kenya-anthro.dta"))
#wbk_sth <- read_dta(paste0(dropboxDir, "Data/WBK/parasites_kenya_public_ca20171215.dta"))

#load clean dataset from the diarhhea-pathogen analysis
wbk_sth <- readRDS(paste0(dropboxDir, "Data/WBK/clean_wbk_sth_pathogens.rds"))
colnames(wbk_sth)

wbk_sth <- wbk_sth %>%
  subset(., select = c(childid, aged, sex,  asca_intensity,
                       tric_intensity,    hook_intensity,ascaris_yn,        trichuris_yn,     
                       hook_yn,           sth_yn,            giardia_yn,        sth_coinf,        
                       sth_giar_coinf,    qpcr_Ascaris,      qpcr_Trichuris,    qpcr_Necator,     
                       qpcr_Ancylostoma,  qpcr_Strongyloides)) %>% 
            filter(!is.na(sth_yn) | !is.na(giardia_yn) | !is.na(qpcr_Ascaris) | !is.na(qpcr_Trichuris) | !is.na(qpcr_Necator) | !is.na(qpcr_Ancylostoma) | !is.na(qpcr_Strongyloides))



#Need to get date of WBK sth collection
#convert wbk_sth to real IDs
colnames(wbk_sth)
wbk_sth <- wbk_sth %>%
  mutate(childid=childid/10 - 3252) %>%
  rename(ch_asca_intensity=asca_intensity, 
         ch_tric_intensity=tric_intensity, 
         ch_hook_intensity=hook_intensity,
         ch_ascaris=ascaris_yn, 
         ch_trichuris=trichuris_yn, 
         ch_hook=hook_yn, 
         ch_sth=sth_yn, 
         ch_giardia=giardia_yn, 
         ch_sth_coinf=sth_coinf, 
         ch_sth_giar_coinf=sth_giar_coinf,
         aged_pathogen = aged)
  



colnames(wbk_diar)
wbk_diar <- wbk_diar %>% 
  mutate(child_date=dmy(DOB)+aged, compoundid=floor(hhid/10)) %>% filter(!is.na(child_date)) %>%
  subset(., select =c("childid","child_date","hhid","compoundid","clusterid","studyyear","aged","sex","tr","diar7d")) 

colnames(wbk_anthro)
wbk_anthro <- wbk_anthro %>% 
  mutate(child_date=ymd(DOB)+aged) %>% filter(!is.na(child_date)) %>%
  subset(., select =c("childid","compoundid","child_date","studyyear","aged","laz","waz","whz"))

#Merge WBK datasets
dim(wbk_diar)
dim(wbk_anthro)



wbk_anthro <- wbk_anthro %>% rename(child_date_anthro=child_date, age_anthro = aged)
wbk <- full_join(wbk_diar, wbk_anthro, by=c("childid","compoundid","studyyear"))
# subset to endline
wbk <- wbk %>% filter(studyyear==2, !is.na(diar7d)|!is.na(laz)|!is.na(waz)|!is.na(whz)) %>% 
  subset(., select = -c(studyyear)) %>% mutate(ch_data=1)
head(wbk)

dim(wbk_sth)
dim(wbk)
wbk <- full_join(wbk, wbk_sth, by=c("childid","sex"))
dim(wbk)

#Need to get date of WBK sth collection
wbk <- wbk %>% group_by(childid) %>% 
  mutate(dob = child_date-aged,
         #dob = ifelse(is.na(dob), dmy(child_date_anthro - age_anthro), dob),
         child_date_pathogen = dob + aged_pathogen)
wbk$dob
class(wbk$dob)
class(wbk$child_date)
head(wbk)
table(is.na(wbk$child_date_pathogen), is.na(wbk$aged_pathogen))

ch <- wbk %>% mutate(trial="WBK") %>%
  rename(dataid=compoundid,
         age=aged,
         haz=laz) %>%
  subset(., select = -c(tr))
table(is.na(wbk$child_date))




wbk_res <- data.frame(
  study = "steinbaum",
  env_samples_before_merge = nrow(env_wbk %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env_wbk %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, haz))
)




dim(env_wbk)
dim(ch)
d <- full_join(env_wbk, ch, by = c("trial","hhid","dataid","clusterid")) %>% filter(!is.na(sample), !is.na(ch_data))
dim(d)
colnames(d)

wbk_res$env_samples_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
wbk_res$env_HH_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
wbk_res$diar_samples_after_merge <- nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, diar7d))
wbk_res$haz_samples_after_merge <- nrow(d %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, haz))
wbk_res$samples_with_diar_after_merge <- nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid))
wbk_res$samples_with_haz_after_merge <- nrow(d %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid))
wbk_res$samples_with_ch_after_merge <- nrow(d %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid))



#--------------------------------------
# NOTE! need to drop dates that are off.
#--------------------------------------

date_diff <- d %>% mutate(date_diff = child_date-env_date) %>% select(study, sampleid, target, dataid, round, hhid, date_diff, diar7d, haz) %>% distinct()

d <- d %>%
  mutate(diar7d = ifelse(child_date<env_date, NA, diar7d),
         diar7d = ifelse(child_date-env_date > 124, NA, diar7d),
         diar7d = ifelse(is.na(child_date)|is.na(env_date), NA, diar7d),
         pathogen_date_flag = ifelse((as.numeric(child_date_pathogen - env_date) <= 124 & (as.numeric(child_date_pathogen - env_date) > 0)) ,1,0))
summary(as.numeric(d$child_date_pathogen - d$env_date))
table(as.numeric(d$child_date_pathogen - d$env_date)>0)
table(as.numeric(d$child_date_pathogen - d$env_date)<=124)
d$child_date_pathogen - d$env_date <= 124
table(is.na(d$child_date_pathogen))
table(is.na(d$pathogen_date_flag))


d <- d %>%
  mutate(haz = ifelse(child_date_anthro<env_date, NA, haz),
         haz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, haz),
         whz = ifelse(child_date_anthro<env_date, NA, whz),
         whz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, whz),
         waz = ifelse(child_date_anthro<env_date, NA, waz),
         waz = ifelse(is.na(child_date_anthro)|is.na(env_date), NA, waz))

wbk_res$diar_samples_date_dropped <- wbk_res$samples_with_diar_after_merge - nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid, age,    sex, diar7d))
wbk_res$haz_samples_date_dropped <- wbk_res$samples_with_haz_after_merge - nrow(d %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid, age,    sex, haz))



wbk_res$percent_diar_samples_dropped <- 100 - wbk_res$diar_samples_after_merge/wbk_res$diar_samples_before_merge * 100
wbk_res$percent_haz_samples_dropped <- 100 - wbk_res$haz_samples_after_merge/wbk_res$haz_samples_before_merge * 100
wbk_res$percent_env_samples_with_diar <- wbk_res$samples_with_diar_after_merge/wbk_res$env_samples_before_merge  * 100
wbk_res$percent_env_samples_with_haz <-  wbk_res$samples_with_haz_after_merge/wbk_res$env_samples_before_merge  * 100
wbk_res$percent_env_samples_with_ch <-  wbk_res$samples_with_ch_after_merge/wbk_res$env_samples_before_merge  * 100

wbk_res$diar_samples_date_dropped[is.na(wbk_res$diar_samples_date_dropped )] <- 0
wbk_res$haz_samples_date_dropped[is.na(wbk_res$haz_samples_date_dropped )] <- 0

wbk_res <- wbk_res %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)


saveRDS(d, file=paste0(dropboxDir,"Data/WBK_env_CH_data.rds"))
saveRDS(wbk_res, file=paste0(here(),"/results/WBK_merge_Ns.rds"))
saveRDS(date_diff, file=paste0(here(),"/results/WBK_date_diff.rds"))

