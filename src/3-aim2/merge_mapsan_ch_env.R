
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

env <- env %>% filter(trial == "MapSan") %>% droplevels(.)

ch <- readRDS(paste0(dropboxDir, "Data/MapSan/mapsan_child_cleaned.rds")) %>% mutate(trial="MapSan", dataid=clusterid)
ch <- ch %>% subset(., select=c(trial, round,child_date,childid, female, age_months, dataid, hhid,clusterid, diar7d, haz,whz,waz)) %>%
  rename(sex=female, age=age_months) %>% mutate(ch_data=1)
head(ch)
table(ch$diar7d)

table(env$round)

head(env)
head(ch)
dim(env)
dim(ch)


#Split data by hh versus compound samples
unique(env$sample)

#d <- full_join(env, ch, by = c("trial","dataid","clusterid"))
env_compound <- env %>% filter(sample %in% c("S","LS","FlyLat","FlyKitch","SW","any sample type")) %>% subset(., select = -c(hhid))
env_hh <- env %>% filter(sample %in% c("MH", "CH", "W" ))

d_compound <- full_join(env_compound, ch, by = c("trial","dataid","clusterid","round"))
d_hh <- full_join(env_hh, ch, by = c("trial","dataid","clusterid", "hhid","round"))
d<-bind_rows(d_compound,d_hh)
d <- d %>% filter(!is.na(sample), !is.na(ch_data))

#merge the next round's diarhhea and anthro and then see which is closer but still after env. sampling 
ch2 <- ch %>% filter(round!="bl") %>% mutate(round=case_when(round == "ml" ~ "bl", round == "el" ~ "ml"))
d_compound2 <- full_join(env_compound, ch2, by = c("trial","dataid","clusterid","round"))  %>% subset(., select = -c(hhid))
d_hh2 <- full_join(env_hh, ch2, by = c("trial","dataid","clusterid", "hhid","round"))
d2<-bind_rows(d_compound2, d_hh2)
d2 <- d2 %>% filter(!is.na(sample), !is.na(ch_data))

dim(d)
dim(d2)

d<-bind_rows(d, d2)
d <- d %>% group_by(sample, sampleid, target) %>% mutate(samp_diff=as.numeric(child_date-env_date)) %>%
  filter(!is.na(samp_diff), samp_diff>=0) %>%
  filter(samp_diff==min(samp_diff))



#Note that I am merging on round, because env sampling occurs at the same time and dates are coarsened in a way
table(d$pos, d$diar7d)
summary(as.numeric(d$child_date- d$env_date))

d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
    diar7d = ifelse(child_date-env_date > 93, NA, diar7d))
table(d$pos, d$diar7d)
table(d$pos, !is.na(d$haz))





saveRDS(d, file=paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))
