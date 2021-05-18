

source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds")) %>% filter(!is.na(diar7d))

#to check:
#-if there is a reasonable number of kids per sample
#the right rounds in each dataset... all env. samples before health outcomes
tab1 <- d %>% filter(!is.na(diar7d)) %>% group_by(trial, dataid, sample, target) %>%
  summarize(N=n()) %>% 
  group_by(trial, sample) %>%
  summarize(mean(N), max(N))
tab1

#Notes: for mapsan and odisha, dataid is not hhid. Do I need to also make a hhid to merge on?

tab <- d %>% filter(!is.na(diar7d)) %>% distinct(trial, dataid, sample, target)
tab

d <- d %>% filter(dataid==10301, sample=="MH", trial=="WBB", target=="Pathogenic E. coli")
head(d)

#Am I correctly merging on round? Get sample dates and drop based on time distance