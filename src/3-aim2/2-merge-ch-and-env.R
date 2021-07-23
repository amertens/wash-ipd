


rm(list=ls())
source(here::here("0-config.R"))

ch <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds")) %>% mutate(ch_data=1)

head(ch)
table(ch$trial, ch$svy)


#-----------------------------------------------------------
# Merge in child health and environmental data
#-----------------------------------------------------------

#env data
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




