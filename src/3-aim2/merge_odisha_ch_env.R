
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
                      child_date)) %>%
  mutate(trial="Odisha",
         dataid=clusterid,
         child_date=ymd(child_date),
         diar7d=case_when(
           diar7d==2 ~ 0, 
           diar7d==1 ~ 1, 
           diar7d==99 | diar7d==92 ~ NA_real_
         )) %>%
  filter(!is.na(diar7d) | !is.na(waz))  %>% mutate(ch_data=1)
head(odisha)


head(env)
head(ch)
dim(env)
dim(ch)

#Note: merging on round is fine, as samples and health outcomes collected on the same day
dim(env)
dim(ch)
d <- full_join(env, ch, by = c("trial","dataid","clusterid"))
dim(d)
colnames(d)

d <- d %>% filter(!is.na(sample), !is.na(ch_data))
dim(d)




table(d$pos, d$diar7d)
summary(as.numeric(d$child_date- d$env_date))

d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
    diar7d = ifelse(child_date-env_date > 93, NA, diar7d))
table(d$pos, d$diar7d)
table(d$pos, !is.na(d$waz))





saveRDS(d, file=paste0(dropboxDir,"Data/odisha_env_CH_data.rds"))
