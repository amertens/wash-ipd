
rm(list=ls())
source(here::here("0-config.R"))
# devtools::install_github("moodymudskipper/safejoin")
library(safejoin)
options(scipen=999)





#-----------------------------------------------------------
# Merge in WASH Benefits Kenya
#-----------------------------------------------------------
wbk_diar <- read.csv(paste0(dropboxDir, "Data/WBK/washb-kenya-diar.csv"))
wbk_anthro <- read_dta(paste0(dropboxDir, "Data/WBK/washb-kenya-anthro.dta"))
wbk_sth <- read_dta(paste0(dropboxDir, "Data/WBK/parasites_kenya_public_ca20171215.dta"))



#Need to get date of WBK sth collection
#convert wbk_sth to real IDs
wbk_sth <- wbk_sth %>%
  mutate(hhid=hhidr2/10 - 3252,
         childid=childidr2/10 - 3252,
         studyyear=2)

colnames(wbk_diar)
wbk_diar <- wbk_diar %>% 
  mutate(child_date=dmy(DOB)+aged, compoundid=floor(hhid/10)) %>% filter(!is.na(child_date)) %>%
  subset(., select =c("childid","child_date","hhid","compoundid","clusterid","studyyear","aged","sex","tr","diar7d")) 
# wbk_sth <- wbk_sth %>% subset(., select =c("childid","studyyear","hhid","compoundid","deworm6m","soilw",
#                                            "wearing_shoes","sameday_defecation", "asca_epg","asca_intensity","asca_intensity_cat",
#                                            "tric_epg","tric_intensity","tric_intensity_cat", "hook_epg","hook_intensity",
#                                            "hook_intensity_cat", "ascaris_yn","trichuris_yn","hook_yn","sth_yn",
#                                            "giardia_yn","sth_coinf","sth_giar_coinf")) 
  

colnames(wbk_anthro)
wbk_anthro <- wbk_anthro %>% 
  mutate(child_date=ymd(DOB)+aged) %>% filter(!is.na(child_date)) %>%
  subset(., select =c("childid","compoundid","child_date","studyyear","laz","waz","whz"))

#Merge WBK datasets
dim(wbk_diar)
dim(wbk_anthro)
# wbk <- full_join(wbk_diar, wbk_anthro, by=c("childid", "compoundid", "studyyear"))
# dim(wbk)
# table(!is.na(wbk$diar7d), !is.na(wbk$laz))

#wbk <- bind_rows(wbk_diar, wbk_anthro)
wbk_anthro <- wbk_anthro%>% rename(child_date_anthro=child_date)
wbk <- full_join(wbk_diar, wbk_anthro, by=c("childid","compoundid","studyyear"))
# subset to endline
wbk <- wbk %>% filter(studyyear==2, !is.na(diar7d)|!is.na(laz)|!is.na(waz)|!is.na(whz)) %>% subset(., select = -c(studyyear)) 
head(wbk)
# dim(wbk_sth)
# dim(wbk)
# wbk <- full_join(wbk, wbk_sth, by=c("childid","hhid", "studyyear"))
# dim(wbk)


wbk <- wbk %>% mutate(trial="WBK") %>%
  rename(dataid=compoundid,
         haz=laz)
table(is.na(wbk$child_date))


#-----------------------------------------------------------
# Clean Mapsan
#-----------------------------------------------------------
mapsan <- readRDS(paste0(dropboxDir, "Data/MapSan/mapsan_child_cleaned.rds")) %>% mutate(trial="MapSan", dataid=clusterid)
mapsan <- mapsan %>% subset(., select=c(trial, child_date,childid, female, age_months, dataid,clusterid, diar7d, haz,whz,waz)) %>%
  rename(sex=female)
head(mapsan)
table(mapsan$diar7d)



#-----------------------------------------------------------
# clean Odisha
#-----------------------------------------------------------

odisha <- read_dta(paste0(dropboxDir,"Data/Odisha/diarrhoea and weight data Odisha san trial.dta")) %>% mutate(trial="Odisha") %>%
  filter(!is.na(currage)) #only include children with date of birth/current age measured, adults don't have dob
head(odisha)
colnames(odisha)



odisha <- odisha %>% 
  rename(childid=indid,
         clusterid=villid,
         age=currage,
         sex=hh104,
         hhwealth=assetf1,
         walls=housestruc,
         Nhh=hhpop,
         child_date=visitdate1,
         landacre=land, 
         diar7d=hh106) %>% 
  subset(., select =c(childid, clusterid, hhid,age,sex,diar7d, waz,
                      child_date, hhwealth, Nhh, walls, landacre)) %>%
  mutate(trial="Odisha",
         dataid=clusterid,
         child_date=ymd(child_date),
         diar7d=case_when(
           diar7d==2 ~ 0, 
           diar7d==1 ~ 1, 
           diar7d==99 | diar7d==92 ~ NA_real_
         )) %>%
  filter(!is.na(diar7d) | !is.na(waz))
head(odisha)


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
                       momedu, haz, whz, sex, age, elec, dadagri, landown, 
                       tr, hhwealth, Nhh, study, trial,     
                       dataid, diar7d, child_date)) %>%
  distinct()

 gv %>% group_by(dataid, round) %>% summarize(N=n()) %>% ungroup() %>% summarise(mean(N))

head(gv)



#Sex, age
# 1.	Child birth order/parity -aim 2 only
# 4.	Household food security -aim 2 only
# 6.	Parental age -aim 2 only
# 8.	Parental employment  - check all
  # a.	Indicator for works in agriculture 
# 9.	Land ownership 


#-----------------------------------------------------------
# bind non-WBB child health data together
#-----------------------------------------------------------

df <- data.table::rbindlist(list( wbk, mapsan, odisha, gv), fill=T)

 table(df$trial, !is.na(df$hhid))
# table(df$trial, !is.na(df$childid))
# table(df$trial, !is.na(df$child_date))
# table(df$trial, !is.na(df$diar7d))
# table(df$trial, !is.na(df$haz)|!is.na(df$waz)|!is.na(df$whz))
# head(df)


#-----------------------------------------------------------
# Clean covariates
#-----------------------------------------------------------

#temp
df <- df %>% filter(!is.na(diar7d)|!is.na(haz)|!is.na(waz))

#Date of observation
table(df$trial, !is.na(df$child_date))

#Fix missing child date in WBK

#Child age 
class(df$age)

table(df$trial, !is.na(df$age))
table(df$trial, !is.na(df$aged))
table(df$trial, !is.na(df$age_months))

df$age[is.na(df$age)] <- df$age[is.na(df$age)]
df$age[is.na(df$age)] <- df$aged[is.na(df$age)]
df$age[is.na(df$age)] <- df$age_months[is.na(df$age)]

table(df$trial, !is.na(df$age))

#child sex
table(df$trial, !is.na(df$sex))

#Temporarily subset to primary health outcome
#df <- df %>% subset(., select = c(trial, clusterid, dataid, hhid, childid, sex,age,child_date, diar7d, haz, whz, waz, svy))
df <- df %>% subset(., select = c(trial, clusterid, dataid, hhid, childid, sex,age,child_date, diar7d, haz, whz, waz))

saveRDS(df, file=paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds"))
