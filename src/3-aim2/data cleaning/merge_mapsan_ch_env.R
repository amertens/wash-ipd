
rm(list=ls())
source(here::here("0-config.R"))


#-----------------------------------------------------------
# Merge mapsan env and child health
#-----------------------------------------------------------

env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study %in% c("Holcomb 2020","Capone 2021") ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 

env <- env %>% filter(trial == "MapSan") %>% droplevels(.)
table(env$study, env$sample)

ch <- readRDS(paste0(dropboxDir, "Data/MapSan/mapsan_child_cleaned.rds")) %>% mutate(trial="MapSan", dataid=clusterid)
ch <- ch %>% subset(., select=c(trial, round,child_date,childid, female, age_months, dataid, hhid,clusterid, diar7d, haz,whz,waz)) %>%
  rename(sex=female, age=age_months) %>% mutate(ch_data=1)
head(ch)
table(ch$diar7d)

table(env$study, env$round)
table(env$study, env$sample, env$round)



holcomb_res <- data.frame(
  study = "holcomb",
  env_samples_before_merge = nrow(env %>% filter(study=="Holcomb 2020") %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% filter(study=="Holcomb 2020")%>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(round!="el",!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(round!="el",!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, haz))
)

capone_res <- data.frame(
  study = "capone",
  env_samples_before_merge = nrow(env %>% filter(study=="Capone 2021") %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% filter(study=="Capone 2021")%>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, haz))
)


#Merge diarrhea and growth separately. Growth 1-year later
# So merge holcomb BL to ML, and ML to EL
# Merge capone BL to ML, and ML to EL
#Merge concurrent diarrhea
ch_bl <- ch %>% filter(round=="bl")
ch_ml <- ch %>% filter(round=="ml")
ch_el <- ch %>% filter(round=="el")
cp_bl <- env %>% filter(study=="Capone 2021", round=="bl") 
cp_ml <- env %>% filter(study=="Capone 2021", round=="ml") 
cp_el <- env %>% filter(study=="Capone 2021", round=="el") 
hol_bl <- env %>% filter(study=="Holcomb 2020", round=="bl") 
hol_ml <- env %>% filter(study=="Holcomb 2020", round=="ml") 


#Function to split data by hh versus compound samples
unique(env$sample)

# env = cp_bl
# ch = ch_ml %>% subset(., select = -c(diar7d))

merge_ch <- function(env, ch){
  ch$ch_data <- 1
  env_compound <- env %>% filter(sample %in% c("LS","FlyLat","FlyKitch","SW","any sample type")) %>% subset(., select = -c(hhid))
  env_hh <- env %>% filter(sample %in% c("S","MH", "CH", "W" ))
  
  d_compound <- full_join(env_compound, ch, by = c("trial","dataid","clusterid"))
  d_hh <- full_join(env_hh, ch, by = c("trial","dataid","clusterid", "hhid"))
  #table(d_hh$study, d_hh$sample)
  
  d<-bind_rows(d_compound,d_hh)
  d <- d %>% filter(!is.na(sample), !is.na(ch_data))
  return(d)
}

# cp_bl <- cp_bl %>% filter(sample=="any sample type", target=="Any pathogen")
# dim(cp_bl)
# dim(ch_ml)
cp_anthro_ml <- merge_ch(cp_bl, ch_ml %>% subset(., select = -c(diar7d)))
cp_anthro_ml <- merge_ch(cp_ml, ch_el %>% subset(., select = -c(diar7d)))
cp_anthro_el <- merge_ch(cp_el, ch_el %>% subset(., select = -c(diar7d)))


cp_diar_bl <- merge_ch()




#d <- full_join(env, ch, by = c("trial","dataid","clusterid"))
env_compound <- env %>% filter(sample %in% c("LS","FlyLat","FlyKitch","SW","any sample type")) %>% subset(., select = -c(hhid))
env_hh <- env %>% filter(sample %in% c("S","MH", "CH",  "W" ))

d_compound <- full_join(env_compound, ch, by = c("trial","dataid","clusterid","round"))
d_hh <- full_join(env_hh, ch, by = c("trial","dataid","clusterid", "hhid","round"))
table(d_hh$study, d_hh$sample)


d<-bind_rows(d_compound,d_hh)
d <- d %>% filter(!is.na(sample), !is.na(ch_data))

table(d$sample)


#merge the next round's diarhhea and anthro and then see which is closer but still after env. sampling 
ch2 <- ch %>% filter(round!="bl") %>% mutate(round=case_when(round == "ml" ~ "bl", round == "el" ~ "ml"))
d_compound2 <- full_join(env_compound, ch2, by = c("trial","dataid","clusterid","round"))  %>% subset(., select = -c(hhid))
d_hh2 <- full_join(env_hh, ch2, by = c("trial","dataid","clusterid", "hhid","round"))
d2<-bind_rows(d_compound2, d_hh2)
d2 <- d2 %>% filter(!is.na(sample), !is.na(ch_data))

d$merge_type="concurrent"
d2$merge_type="next round"

table(d$study, d$sample)
table(d2$study, d2$sample)
dim(d)
dim(d2)

d<-bind_rows(d, d2)

#Subset to closest observation where CH is after env. sample collection
d <- d %>% group_by(sample, sampleid, target, dataid, clusterid, round, childid) %>% mutate(samp_diff=as.numeric(child_date-env_date), samp_diff2=ifelse(samp_diff<1, -100000, samp_diff), N=n()) %>% 
  filter(samp_diff2-1==min(samp_diff2-1)) %>% ungroup()
table(d$merge_type)
summary(d$samp_diff)
summary(d$samp_diff2)

table(d$sample)

mapsan_res$env_samples_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
mapsan_res$env_HH_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
mapsan_res$diar_samples_after_merge <- nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, diar7d))
mapsan_res$haz_samples_after_merge <- nrow(d %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, haz))
mapsan_res$samples_with_diar_after_merge <- nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
mapsan_res$samples_with_haz_after_merge <- nrow(d %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
mapsan_res$samples_with_ch_after_merge <- nrow(d %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))



summary(d$samp_diff[d$study=="Holcomb 2020"])
summary(d$samp_diff[d$study=="Capone et al. 2021"])

d <- d  %>%
  filter(!is.na(samp_diff), samp_diff>=0) %>%
  filter(samp_diff==min(samp_diff))




#Note that I am merging on round, because env sampling occurs at the same time and dates are coarsened in a way
table(d$pos, d$diar7d)
summary(as.numeric(d$child_date- d$env_date))

date_diff <- d %>% mutate(date_diff = child_date-env_date) %>% select(study, sampleid, target, dataid, round, hhid, date_diff, diar7d, haz) %>% distinct()


d <- d %>% 
  filter(child_date +14 >=env_date) %>% #add 14 because child data had been coarsened to the month
  mutate(
    diar7d = ifelse(child_date-env_date > 124, NA, diar7d))
table(d$pos, d$diar7d)
table(d$pos, !is.na(d$haz))


df <- env %>% filter(sample=="any sample type", target=="Any pathogen")
table(df$pos)
df <- d %>% filter(sample=="any sample type", target=="Any pathogen")
table(df$pos)
df <- env %>% filter(sample=="any sample type", target=="Any MST")
table(df$pos)
df <- d %>% filter(sample=="any sample type", target=="Any MST")
table(df$pos)

#Check how far apart rounds are


mapsan_res$diar_samples_date_dropped <- mapsan_res$samples_with_diar_after_merge - nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid, hhid, age,    sex, diar7d))
mapsan_res$haz_samples_date_dropped <- mapsan_res$samples_with_haz_after_merge - nrow(d %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid, hhid, age,    sex, haz))



mapsan_res$percent_diar_samples_dropped <- 100 - mapsan_res$diar_samples_after_merge/mapsan_res$diar_samples_before_merge * 100
mapsan_res$percent_haz_samples_dropped <- 100 - mapsan_res$haz_samples_after_merge/mapsan_res$haz_samples_before_merge * 100
mapsan_res$percent_env_samples_with_diar <- mapsan_res$samples_with_diar_after_merge/mapsan_res$env_samples_before_merge  * 100
mapsan_res$percent_env_samples_with_haz <-  mapsan_res$samples_with_haz_after_merge/mapsan_res$env_samples_before_merge  * 100
mapsan_res$percent_env_samples_with_ch <-  mapsan_res$samples_with_ch_after_merge/mapsan_res$env_samples_before_merge  * 100

mapsan_res$diar_samples_date_dropped[is.na(mapsan_res$diar_samples_date_dropped )] <- 0
mapsan_res$haz_samples_date_dropped[is.na(mapsan_res$haz_samples_date_dropped )] <- 0

mapsan_res <- mapsan_res %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)




saveRDS(d, file=paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))
saveRDS(mapsan_res, file=paste0(here(),"/results/mapsan_merge_Ns.rds"))
saveRDS(date_diff, file=paste0(here(),"/results/mapsan_date_diff.rds"))
