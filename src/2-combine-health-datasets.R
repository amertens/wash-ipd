
rm(list=ls())
source(here::here("0-config.R"))
# devtools::install_github("moodymudskipper/safejoin")
library(safejoin)
options(scipen=999)



#-----------------------------------------------------------
# Clean WASH Benefits Bangladesh
#-----------------------------------------------------------
wbb <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS")) %>% 
  rename(haz=laz) %>%
  mutate(trial="WBB",
         child_date =dmy(child_date),
         child_date_anthro =dmy(child_date_anthro)
         )
head(wbb)

#-----------------------------------------------------------
# Merge in WASH Benefits Kenya
#-----------------------------------------------------------
wbk_diar <- read.csv(paste0(dropboxDir, "Data/WBK/washb-kenya-diar.csv"))
wbk_anthro <- read_dta(paste0(dropboxDir, "Data/WBK/washb-kenya-anthro.dta"))
wbk_sth <- read_dta(paste0(dropboxDir, "Data/WBK/parasites_kenya_public_ca20171215.dta"))
# wbk_enrol <- read.csv(paste0(dropboxDir, "Data/WBK/washb-kenya-enrol.csv")) %>% subset(., select=c(
#   "childid", "clusterid", "block" ,     "tr",    "hhid",
#   "Ncomp", "cow", "goat", "chicken",
#   "dog","mother_age","mother_edu","water_time","roof","walls",
#   "floor","elec","radio","tv","mobilephone","clock",
#   "bicycle","motorcycle","stove","cooker","car","u18",
#   "HHS")) 


#Need to get date of WBK sth collection
#convert wbk_sth to real IDs
wbk_sth <- wbk_sth %>%
  mutate(hhid=hhidr2/10 - 3252,
         childid=childidr2/10 - 3252,
         studyyear=2)

colnames(wbk_diar)
wbk_diar <- wbk_diar %>% 
  mutate(child_date=dmy(DOB)+aged) %>%
  subset(., select =c("childid","child_date","hhid","clusterid","studyyear","aged","sex","tr","diar7d")) 
wbk_sth <- wbk_sth %>% subset(., select =c("childid","studyyear","hhid","deworm6m","soilw",
                                           "wearing_shoes","sameday_defecation", "asca_epg","asca_intensity","asca_intensity_cat",
                                           "tric_epg","tric_intensity","tric_intensity_cat", "hook_epg","hook_intensity",
                                           "hook_intensity_cat", "ascaris_yn","trichuris_yn","hook_yn","sth_yn",
                                           "giardia_yn","sth_coinf","sth_giar_coinf")) 
  

colnames(wbk_anthro)
wbk_anthro <- wbk_anthro %>% 
  mutate(child_date_anthro=dmy(DOB)+aged) %>%
  subset(., select =c("childid","child_date_anthro","studyyear","laz","waz","whz"))

#Merge WBK datasets
dim(wbk_diar)
dim(wbk_anthro)
wbk <- full_join(wbk_diar, wbk_anthro, by=c("childid", "studyyear"))
dim(wbk)
table(!is.na(wbk$diar7d), !is.na(wbk$laz))

dim(wbk_sth)
dim(wbk)
wbk <- full_join(wbk, wbk_sth, by=c("childid","hhid", "studyyear"))
dim(wbk)


wbk <- wbk %>% mutate(trial="WBK") %>%
  rename(dataid=hhid,
         haz=laz)



#-----------------------------------------------------------
# Clean Mapsan
#-----------------------------------------------------------
mapsan <- readRDS(paste0(dropboxDir, "Data/MapSan/mapsan_child_cleaned.rds")) %>% mutate(trial="MapSan", dataid=clusterid)
mapsan <- mapsan %>% subset(., select=c(trial, child_date,childid , dataid,clusterid, diar7d, haz,whz,waz))
head(mapsan)
table(mapsan$diar7d)



#-----------------------------------------------------------
# clean Odisha
#-----------------------------------------------------------

odisha <- read_dta(paste0(dropboxDir,"Data/Odisha/diarrhoea and weight data Odisha san trial.dta")) %>% mutate(trial="Odisha")
head(odisha)
colnames(odisha)

odisha <- odisha %>% 
  rename(childid=indid,
         clusterid=villid,
         age=currage,
         diar7d=hh106) %>% 
  subset(., select =c(childid, clusterid, hhid,age,diar7d, waz,
                      visitdate1)) %>%
  mutate(trial="Odisha",
         dataid=clusterid,
         child_date=ymd(visitdate1),
         diar7d=case_when(
           diar7d==2 ~ 0, 
           diar7d==1 ~ 1, 
           diar7d==99 | diar7d==92 ~ NA_real_
         )) %>%
  filter(!is.na(diar7d) | !is.na(waz))
head(odisha)

#Why are there so many missing ages? Are these adults?
table(is.na(odisha$age))

#-----------------------------------------------------------
# clean GV
#-----------------------------------------------------------

# load(paste0(dropboxDir,"Data/Gram Vikas/gramvikas.rda"))
# colnames(gramvikas)
# head(gramvikas)
# 
# #Diarrhea
# table(gramvikas$hh.dia7)
#Z-scores

#identifiers
#hhm.id


#Note that the GV lab data had diarrhea/anthro merged in, but 

gv <- readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds")) %>% 
  mutate(study="Reese et al. 2017",
         trial="Gram Vikas",
         dataid=as.numeric(factor(hh_mid))*10+round,
         #dia7=ifelse(dia7==99,NA,dia7),
                                  #diar7d=ifelse(dia7==2,0,1),
                                  diar7d=as.numeric(dia7),
                                  haz=as.numeric(haz),
                                  whz=as.numeric(whz),
                                  child_date =env_date) %>%
  subset(., select = -c(dia7))
 gv %>% group_by(dataid, round) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))

head(gv)
#-----------------------------------------------------------
# bind child health data together
#-----------------------------------------------------------

df <- data.table::rbindlist(list(wbb, wbk, mapsan, odisha, gv), fill=T)

# table(df$trial, !is.na(df$childid))
# table(df$trial, !is.na(df$child_date))
# table(df$trial, !is.na(df$diar7d))
# table(df$trial, !is.na(df$haz)|!is.na(df$waz)|!is.na(df$whz))
# head(df)

class(df$haz)
class(df$whz)
class(df$waz)


#TEMP 
#Temporarily subset to primary health outcome
df <- df %>% subset(., select = c(trial, clusterid, dataid, childid, child_date, diar7d, haz, whz, waz, svy))

saveRDS(df, file=paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds"))
