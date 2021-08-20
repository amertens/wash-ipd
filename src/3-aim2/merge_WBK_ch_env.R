
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
wbk <- wbk %>% filter(studyyear==2, !is.na(diar7d)|!is.na(laz)|!is.na(waz)|!is.na(whz)) %>% 
  subset(., select = -c(studyyear)) %>% mutate(ch_data=1)
head(wbk)
# dim(wbk_sth)
# dim(wbk)
# wbk <- full_join(wbk, wbk_sth, by=c("childid","hhid", "studyyear"))
# dim(wbk)


wbk <- wbk %>% mutate(trial="WBK") %>%
  rename(dataid=compoundid,
         agedays=aged,
         haz=laz)
table(is.na(wbk$child_date))

ch <- wbk %>% subset(., select = c(trial, clusterid, dataid, hhid, childid, sex,agedays,child_date, diar7d, haz, whz, waz,ch_data))



d <- full_join(env_wbk, ch, by = c("trial","dataid","clusterid"))

d <- d %>% filter(!is.na(sample), !is.na(ch_data))





saveRDS(d, file=paste0(dropboxDir,"Data/WBK_env_CH_data.rds"))
