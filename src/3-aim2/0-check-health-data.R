
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data.rds")) %>% filter(!is.na(diar7d))
ch <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds")) %>% filter(!is.na(diar7d))
env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))



#Why 7 cases of diarrhea for any sample type, but 10 for water? Check into merging
d <- d %>% filter(trial=="WBB")
ch <- ch %>% filter(trial=="WBB")
env1 <- env %>% filter(study=="Fuhrmeister 2020", sample=="any sample type", target== "Any pathogen")
env2 <- env %>% filter(study=="Fuhrmeister 2020", sample=="W", target== "Any pathogen")

table(ch$diar7d)
table(env1$pos)
table(env2$pos)
sum(!is.na(env1$pos))
sum(!is.na(env2$pos)) #why are there more water samples than "any sample type"?

env %>% filter(study=="Fuhrmeister 2020", target== "Any pathogen", sample!="any sample type") %>%
  group_by(study, clusterid, dataid, tr, target) %>% slice(1) %>% ungroup() %>%
  summarize(N=n())
env %>% filter(study=="Fuhrmeister 2020", target== "Any pathogen", sample!="any sample type") %>%
  group_by(study,  target, sample)  %>%
  summarize(N=n())
env %>% filter(study=="Fuhrmeister 2020", target== "Any pathogen", sample!="any sample type") %>%
  group_by(study,  target, sample, dataid)  %>% 
  summarize(N=n()) %>% ungroup() %>% summarize(min(N), mean(N), max(N))


df <- env %>% filter(study=="Fuhrmeister 2020") %>%
  group_by(study, clusterid, dataid, tr, target) %>%
  mutate(pos=max(pos, na.rm = TRUE), sample="any sample type") %>% 
  slice(1)

# the merge issue is because CH is at the trial level, and the "any sample type" is at the study level
#d <- full_join(env, ch, by = c("trial","dataid","clusterid")) %>% filter(!is.na(pos), !is.na(diar7d))

dim(env1)
dim(env2)
dim(ch)
d1 <- full_join(env1, ch, by = c("trial","dataid","clusterid")) %>% filter(!is.na(pos), !is.na(diar7d))
d2 <- full_join(env2, ch, by = c("trial","dataid","clusterid")) %>% filter(!is.na(pos), !is.na(diar7d))
dim(d1)
dim(d2)

table(env1$pos)
temp1 <- env1 %>% group_by(study, clusterid, dataid) %>% slice(1)
table(temp1$pos)


table(env2$pos)
temp2 <- env2 %>% group_by(study, clusterid, dataid, sampleid) %>% slice(1)
table(temp2$pos)

temp2 <- env2 %>% distinct()




#to check:
#-if there is a reasonable number of kids per sample
#the right rounds in each dataset... all env. samples before health outcomes
tab1 <- d %>% filter(!is.na(diar7d)) %>% group_by(trial, dataid, sample, target) %>%
  summarize(N=n()) %>% 
  group_by(trial, sample, target) %>%
  summarize(min(N), mean(N), max(N))
tab1

#Notes: for mapsan and odisha, dataid is not hhid. Do I need to also make a hhid to merge on?

tab <- d %>% filter(!is.na(diar7d)) %>% distinct(trial, dataid, sample, target)
tab

# d <- d %>% filter(dataid==10301, sample=="MH", trial=="WBB", target=="Pathogenic E. coli")
# head(d)

#Am I correctly merging on round? Get sample dates and drop based on time distance


#Check why so few successfuly diarrhea estimates
d <- d %>% filter(target=="Any pathogen", !is.na(diar7d))

table(d$pos, d$diar7d, d$sample, d$trial)

















#-----------------------------------------------------------
# Check merging
#-----------------------------------------------------------

#env data
ch <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds")) %>% mutate(ch_data=1)
env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study=="Holcomb 2020" ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 
table(env$trial)  
#Drop baseline measure from mapsan and food prep samples
env <- env %>% filter(round != "0m", sample!="FP") %>% droplevels(.)

#Check just wash benefits
ch <- ch %>% filter(trial=="WBB", !is.na(diar7d))
env <- env %>% filter(trial=="WBB", target=="Any pathogen", sample=="any sample type")

dim(ch)
dim(env)

d <- full_join(env, ch, by = c("trial","dataid","clusterid"))
d <- d %>% filter(!is.na(sample), !is.na(ch_data))
dim(d)

table(d$pos, d$diar7d)

d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
    diar7d = ifelse(child_date-env_date > 93, NA, diar7d))

table(d$pos, d$diar7d)

#Why are so many dropped?
summary(d$child_date)
summary(d$env_date)
summary(as.numeric(d$child_date-d$env_date))



