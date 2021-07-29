


rm(list=ls())
source(here::here("0-config.R"))

ch <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds")) %>% mutate(ch_data=1)

head(ch)
table(ch$trial, ch$round)


#-----------------------------------------------------------
# Merge in child health and environmental data
#-----------------------------------------------------------

#env data
env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
table(env$study)  
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study=="Holcomb 2020" ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 
table(env$trial)  
table(env$trial, env$study)  
table(env$study, is.na(env$round))
#Drop baseline measure from mapsan and food prep samples
env <- env %>% filter(round != "0m", sample!="FP") %>% droplevels(.)
#Drop WBB (merged seperately)
env_wbb <- env %>% filter(trial == "WBB") %>% droplevels(.)
env <- env %>% filter(trial != "WBB") %>% droplevels(.)

table(env_wbb$study)
table(env$trial, is.na(env$dataid))  
table(env$trial, is.na(env$env_date))  




# #XXX TEMP XXXX
# #Subset to one sample per HH to check merge
# #How much is issues in the round selected causing merge errors?
# head(env)
# env <- env %>% group_by(trial, dataid) %>% slice(1)
# ch <- ch %>% group_by(trial, dataid, childid) %>% filter(!is.na(childid), !is.na(diar7d)) %>% slice(1)
# d <- eat(ch, env, .by = c("trial","clusterid","dataid"), .conflict = "patch")
# d2 <- left_join(ch, env, by = c("trial","clusterid","dataid"))
# head(d2)
# table(is.na(d2$diar7d))
# table(as.numeric(d2$child_date-d2$env_date)> -91)
# 
# env <- env %>% filter(trial=="WBB")
# ch <- ch %>% filter(trial=="WBB")
# head(env)
# head(ch)
# 
# dim(env)
# dim(ch)
# 
# haz <- ch %>% filter(trial=="WBB", !is.na(haz))
# mst <- env %>% filter(study=="Fuhrmeister 2020", target=="Any MST")
# d <- full_join(mst, haz, by = c("trial","dataid","clusterid"))# %>% filter(!is.na(pos), !is.na(diar7d))
# 
# d <- d %>%
#   filter(child_date>=(env_date))
# 
# table(d$pos,!is.na(d$haz))
# table(d$pos,!is.na(d$haz), d$sample)
# 
# summary(d$env_date)
# summary(d$child_date)
# table(as.numeric(d$child_date-d$env_date))
# table(as.numeric(d$child_date-d$env_date)> -91)
# #XXX End TEMP XXXX


#Split data by hh versus compound samples
unique(env$sample)
                       
#d <- full_join(env, ch, by = c("trial","dataid","clusterid"))
env_compound <- env %>% filter(sample %in% c("S","LS","FlyLat","FlyKitch","SW","any sample type"))
env_hh <- env %>% filter(sample %in% c("MH", "CH", "W" ))

d_compound <- full_join(env_compound, ch, by = c("trial","dataid","clusterid"))
d_hh <- full_join(env_hh, ch, by = c("trial","dataid","clusterid", "hhid"))

d<-bind_rows(d_compound,d_hh)
d <- d %>% filter(!is.na(sample), !is.na(ch_data))

table(is.na(d$sample))
table(d$trial, is.na(d$sample))

dim(d)
table(d$sample,d$diar7d)
table(ch$trial,ch$diar7d)
table(d$trial,d$diar7d)
d2 <- d %>% filter(target=="Any pathogen", sample=="any sample type")
table(d2$trial,d2$diar7d)
d3 <- d %>% filter(target=="Any pathogen", sample=="S")
table(d3$trial,d3$diar7d)

table(d$pos,d$diar7d)
table(d$pos,d$diar7d, d$study)

table(d$pos,!is.na(d$haz), d$study)


#Check rows that don't merge
df1 <- anti_join(env, ch, by = c("trial","dataid","clusterid")) %>% droplevels()
table(df1$study)
df2 <- anti_join(ch, env, by = c("trial","dataid","clusterid")) %>% droplevels()
table(df2$trial)



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

prop.table(table(d$trial[!is.na(d$haz)], d$child_date[!is.na(d$haz)]-d$env_date[!is.na(d$haz)]>=0),1)*100  #Where is WBK anthro?
prop.table(table(d$trial[!is.na(d$diar7d)], d$child_date[!is.na(d$diar7d)]-d$env_date[!is.na(d$diar7d)] < 93),1)*100

#prop.table(table(d$study[!is.na(d$haz)], d$child_date[!is.na(d$haz)]-d$env_date[!is.na(d$haz)]>=0),1)*100
prop.table(table(d$study[!is.na(d$diar7d)], d$child_date[!is.na(d$diar7d)]-d$env_date[!is.na(d$diar7d)] < 93),1)*100

table(d$trial,d$diar7d)

d <- d %>% 
  filter(child_date>=env_date) %>%
  mutate(
  diar7d = ifelse(child_date-env_date > 93, NA, diar7d))

table(d$trial,d$diar7d)
table(d$trial,!is.na(d$haz))

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




#-----------------------------------------------------------
# Merge WBB
#-----------------------------------------------------------
wbb <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS")) %>% 
  rename(haz=laz) %>%
  mutate(trial="WBB",
         aged=agedays
  )
table(wbb$study)
table(env_wbb$study)

head(wbb)
head(env_wbb)

table(wbb$round)
table(is.na(wbb$round))

table(env_wbb$round)
table(is.na(env_wbb$round))

#Merge WBB based on sampling round

#1) Fuhrmeister to health outcomes

# Specifically, Erica's samples were collected during Round 3 and Round 4 of the R01. 
#Therefore, if you merge them with the R01 health data with a one-round offset (i.e., match the R3 environmental data to the R4 
#health data, the R4 environmental data to the R5 health data), 
#then each environmental datapoint should have a subsequent diarrhea datapoint within the next 3ish months.
env_fuhr <- env_wbb %>% filter(study=="Fuhrmeister 2020") %>% mutate(merge_round=as.numeric(round))
diar_fuhr <- wbb %>% filter(round %in% c(4,5), !is.na(diar7d)) %>% mutate(merge_round=as.numeric(round)-1) %>%
  select(block, clusterid, dataid, hhid, round,
         merge_round, child_date, agedays, sex,             
         childid, diar7d,momage, hfiacat)
table(env_fuhr$round)
table(diar_fuhr$round)
table(diar_fuhr$merge_round)
table(is.na(diar_fuhr$diar7d))
table(env_fuhr$sample, env_fuhr$round)


dim(env_fuhr)
dim(diar_fuhr)
diar_env_fuhr <- full_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round")) %>% filter(!is.na(pos) | !is.na(diar7d)) %>% arrange(sampleid, dataid,clusterid, hhid,childid, merge_round)
#diar_env_fuhr <- left_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round"))
dim(diar_env_fuhr)
table(diar_env_fuhr$pos, diar_env_fuhr$diar7d)


#Get endline anthropometry
anthro_fuhr <- wbb %>% filter(round == "endline", !is.na(haz)|!is.na(waz)|!is.na(whz)) %>% 
  select(block, clusterid, dataid, hhid, child_date, agedays, sex,             
         childid, haz, waz, whz, momage, hfiacat)

dim(diar_env_fuhr)
dim(anthro_fuhr)
ch_env_fuhr <- full_join(diar_env_fuhr, anthro_fuhr, by = c("dataid","clusterid", "hhid")) %>% filter(!is.na(pos) | (!is.na(diar7d) & !is.na(haz) & !is.na(waz) & !is.na(whz))) 
#diar_env_fuhr <- left_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round"))
dim(ch_env_fuhr)

head(ch_env_fuhr)


#2) Boehm to health outcomes
# Erica's R3 and R4 sampling definitely preceded the main trial endline anthro measurements so there should not be missings there either.
# The Boehm sampling was done as part of the World Bank study which collected its own diarrhea data one week after the environmental samples 
# (and Amy shared that dataset with you) so those should be perfectly matched.
# The timing of the Boehm sampling was on average 4 months after the initiation of interventions so it preceded the main trial
# midline anthro measurements and should be perfectly matched to those as well.
env_boehm <- env_wbb %>% filter(study=="Boehm 2016")

#3) Kwong to health outcomes
env_kwong <- env_wbb %>% filter(study=="Kwong 2021")





# In other words, for Erica and Boehm, I would rely on the R01 and World Bank diarrhea datasets, respectively, rather than the main trial. 
# As for the anthro data, the main trial midline should fall after Boehm and the main trial endline should fall after Erica within the time 
# window we allow (environmental sampling anytime before anthro during child's lifetime).
# For the Erica+R01 data, the key will be to offset them by one round (match one round of env data to the following round of diarrhea data 
# to allow ~3 months in between). I suggest doing this merge separately from your combined mega merge.
# Likewise, the World Bank dataset has its own diarrhea -- just make sure you use the prospective (and not concurrent) diarrhea variable 
# when you match it to the Boehm env data for a given HH. That will automatically ensure that the env data precedes the diarrhea data by 4-10 days (as the study was designed to revisit HHs to collect diarrhea data 4-10 days after environmental sampling).

# -should be a 1:1 merge for WBB R01/WB env. sampling and health outcomes
# -make table of numbers before and after dropping for timing and share


