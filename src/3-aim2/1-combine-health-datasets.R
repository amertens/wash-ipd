


rm(list=ls())
source(here::here("0-config.R"))

wbb <- readRDS(paste0(dropboxDir,"Data/WBB_env_CH_data.rds"))
wbk <- readRDS(paste0(dropboxDir,"Data/WBK_env_CH_data.rds"))
mapsan <- readRDS(paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))
gv <- readRDS(paste0(dropboxDir,"Data/gv_env_CH_data.rds"))
odisha <- readRDS(paste0(dropboxDir,"Data/odisha_env_CH_data.rds"))



#d <- bind_rows(wbb, wbk, mapsan, gv, odisha)
d <- data.table::rbindlist(list( wbb, wbk, mapsan, gv, odisha), fill=T)
#d <- d %>% filter(!is.na(env_date) & !is.na(child_date))
table(d$study)

#-----------------------------------------------------------
# Calculate stunting/wasting/underweight
#-----------------------------------------------------------

d <- d %>% mutate(
  stunt = 1*(haz < -2),
  wast = 1*(whz < -2),
  underwt = 1*(waz < -2),
)


#-----------------------------------------------------------
# Check covariates
#-----------------------------------------------------------
table(d$study, d$hfiacat)
table(d$study, is.na(d$hfiacat))
table(d$study, d$hhwealth)
table(d$study, is.na(d$hhwealth))
table(d$study, d$momedu)
table(d$study, is.na(d$momedu)) 
table(d$study, is.na(d$momage)) 

table(d$study, d$sex)
table(d$study, is.na(d$sex))
d$age[is.na(d$age)] <- d$agedays[is.na(d$age)]
table(d$study, is.na(d$age))


#-----------------------------------------------------------
# Save
#-----------------------------------------------------------
saveRDS(d, file=paste0(dropboxDir,"Data/merged_env_CH_data.rds"))




# d %>% group_by(study, sample, target) %>%
#   summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_pos_diar=sum(diar7d==1, na.rm=T), N_pos_env_diar=sum(pos==1 & diar7d==1, na.rm=T), N_haz=sum(!is.na(haz)))

d %>% group_by(study) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_pos_diar=sum(diar7d==1, na.rm=T), N_haz=sum(!is.na(haz)), N_waz=sum(!is.na(waz)))

tab <- d %>% group_by(study, sample, target) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_pos_diar=sum(diar7d==1, na.rm=T), N_pos_env_diar=sum(pos==1 & diar7d==1, na.rm=T), N_haz=sum(!is.na(haz)))
tab %>% filter(sample=="any sample type", target=="Any pathogen")
tab %>% filter(sample=="any sample type", target=="Any MST")

#To do:

#specific issues to check
#-diarrhea in holcomb seems reall low. See how many in primary trial at same sampling time
#

#To check: get number of diarhhea/anthro samples by study
#check Kwong env dates. Can I get actual dates?
#Check that there is a health measure for every env. in fuhrmeister
#Check I am using the closest anthro measure per child in all studies
#make a variable for just diarrhea named diarhhea_all right before setting too-far away diarrhea as NA
#Make table for Ayse
#Check covariate usage, especially the right child age for diarrhea and Z-scores
