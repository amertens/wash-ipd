
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

env <- env %>% filter(trial == "Odisha") %>% 
  subset(., select = -c(hhid)) %>%
  droplevels(.)

#drop missing covariates and get them from child dataset
env <- env %>% subset(., select = c(study,trial, sampleid, dataid, clusterid, tr,    sample, env_date,   target,   pos, abund, qual,  round))
head(env)


#-----------------------------------------------------------
# clean Odisha
#-----------------------------------------------------------

ch <- read_dta(paste0(dropboxDir,"Data/Odisha/diarrhoea and weight data Odisha san trial.dta")) %>% mutate(trial="Odisha") %>%
  filter(!is.na(currage)) #only include children with date of birth/current age measured, adults don't have dob
head(ch)
colnames(ch)


ch <- ch %>% 
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
                      child_date, hhwealth, walls, Nhh, landacre)) %>%
  mutate(trial="Odisha",
         dataid=clusterid,
         age_anthro=age,
         child_date=ymd(child_date),
         hhwealth=factor(ntile(hhwealth,4), levels=c("1","2","3","4")),
         diar7d=case_when(
           diar7d==2 ~ 0, 
           diar7d==1 ~ 1, 
           diar7d==99 | diar7d==92 ~ NA_real_
         )) %>%
  filter(!is.na(diar7d) | !is.na(waz))  %>% mutate(ch_data=1)


odisha_res <- data.frame(
  study = "odagiri",
  env_samples_before_merge = nrow(env %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,   clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% do(drop_agg(.)) %>% distinct(dataid,  clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(!is.na(waz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, waz))
)


#Note: merging on round is fine, as samples and health outcomes collected on the same day
dim(env)
dim(ch)
d <- full_join(env, ch, by = c("trial","dataid","clusterid"))
d <- d %>% filter(!is.na(sample), !is.na(ch_data))
dim(d)


odisha_res$env_samples_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))
odisha_res$env_HH_after_merge <- nrow(d %>% do(drop_agg(.)) %>% distinct(dataid, clusterid))
odisha_res$diar_samples_after_merge <- nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, diar7d))
odisha_res$haz_samples_after_merge <- nrow(d %>% filter(!is.na(waz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, waz))
odisha_res$samples_with_diar_after_merge <- nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))
odisha_res$samples_with_haz_after_merge <- nrow(d %>% filter(!is.na(waz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))
odisha_res$samples_with_ch_after_merge <- nrow(d %>% filter(!is.na(waz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))


table(d$pos, d$diar7d)
summary(as.numeric(d$child_date- d$env_date))

date_diff <- d %>% mutate(date_diff = child_date-env_date) %>% select(study, sampleid, target, dataid, round, hhid, date_diff, diar7d, waz) %>% distinct()

d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
    diar7d = ifelse(child_date-env_date > 124, NA, diar7d))
table(d$pos, d$diar7d)
table(d$pos, !is.na(d$waz))

odisha_res$diar_samples_date_dropped <- odisha_res$samples_with_diar_after_merge - nrow(d %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))
odisha_res$haz_samples_date_dropped <- odisha_res$samples_with_haz_after_merge - nrow(d %>% filter(!is.na(waz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))



odisha_res$percent_diar_samples_dropped <- 100 - odisha_res$diar_samples_after_merge/odisha_res$diar_samples_before_merge * 100
odisha_res$percent_haz_samples_dropped <- 100 - odisha_res$haz_samples_after_merge/odisha_res$haz_samples_before_merge * 100
odisha_res$percent_env_samples_with_diar <- odisha_res$samples_with_diar_after_merge/odisha_res$env_samples_before_merge  * 100
odisha_res$percent_env_samples_with_haz <-  odisha_res$samples_with_haz_after_merge/odisha_res$env_samples_before_merge  * 100
odisha_res$percent_env_samples_with_ch <-  odisha_res$samples_with_ch_after_merge/odisha_res$env_samples_before_merge  * 100

odisha_res$diar_samples_date_dropped[is.na(odisha_res$diar_samples_date_dropped )] <- 0
odisha_res$haz_samples_date_dropped[is.na(odisha_res$haz_samples_date_dropped )] <- 0

odisha_res <- odisha_res %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)


summary(d$hhwealth)


saveRDS(d, file=paste0(dropboxDir,"Data/odisha_env_CH_data.rds"))
saveRDS(odisha_res, file=paste0(here(),"/results/odisha_merge_Ns.rds"))
saveRDS(date_diff, file=paste0(here(),"/results/odisha_date_diff.rds"))
