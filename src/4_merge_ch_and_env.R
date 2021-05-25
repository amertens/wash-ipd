


rm(list=ls())
source(here::here("0-config.R"))

ch <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds"))

head(ch)
table(ch$trial, ch$svy)

#Should I transform CH to longform?



#-----------------------------------------------------------
# Merge in child health and environmental data
#-----------------------------------------------------------

#env data
env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister et al. 2020", "Kwong et al. 2021", "Boehm et al. 2016") ~ "WBB",
                    study=="Steinbaum et al. 2019" ~ "WBK",
                    study=="Holcomb et al. 2020" ~ "MapSan",
                    study=="Reese et al. 2017" ~ "Gram Vikas",
                    study=="Odagiri et al. 2016" ~ "Odisha")) 
table(env$trial)  
#Drop baseline measure from mapsan
env <- env %>% filter(round != "0m") %>% droplevels(.)

table(env$trial, is.na(env$dataid))  
table(env$trial, is.na(env$env_date))  




#XXX TEMP XXXX
#Subset to one sample per HH to check merge
#How much is issues in the round selected causing merge errors?
# head(env)
# env <- env %>% group_by(trial, dataid) %>% slice(1)
# ch <- ch %>% group_by(trial, dataid, childid) %>% filter(!is.na(childid)) %>% slice(1)

# d <- eat(d, env, .by = c("trial","clusterid"), .conflict = "patch")

# env <- env %>% filter(trial=="Odisha")
# ch <- ch %>% filter(trial=="Odisha")
# head(env)
# head(ch)

dim(env)
dim(ch)
d <- full_join(env, ch, by = c("trial","dataid","clusterid")) %>% filter(!is.na(pos), !is.na(diar7d))
dim(d)
table(d$sample,d$diar7d)
table(ch$trial,ch$diar7d)
table(d$trial,d$diar7d)

table(d$pos,d$diar7d)


#Notes to check! 
#Why isn't odisha env. and diarrhea merging?
# Check the mapsan diarrhea merging... mapsan is failing to estimate any results
# d <- d %>% filter(trial=="MapSan")
# table(d$pos,d$diar7d)
# df <- d %>% filter(target=="Any pathogen", sample=="any sample type")
# table(df$pos,df$diar7d)
# df <- d %>% filter(target=="Any MST", sample=="any sample type")
# table(df$pos,df$diar7d)


#-----------------------------------------------------------
# Drop obs of diarrhea < 3 months from sampling time
# And any anthro obs after a sample merge
#-----------------------------------------------------------
table(1*!is.na(d$env_date), !is.na(d$child_date))
table(1*!is.na(d$env_date), !is.na(d$child_date), d$trial)
d <- d %>% filter(!is.na(env_date) & !is.na(child_date))

df <- d %>% filter(trial=="Gram Vikas")
table(df$child_date-df$env_date)

d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
  diar7d = ifelse(child_date-env_date > 93, NA, diar7d))

table(d$trial,d$diar7d)

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
table(d$study, is.na(d$age))


#-----------------------------------------------------------
# Save
#-----------------------------------------------------------
saveRDS(d, file=paste0(dropboxDir,"Data/merged_env_CH_data.rds"))




