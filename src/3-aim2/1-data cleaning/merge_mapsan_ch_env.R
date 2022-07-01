
rm(list=ls())
source(here::here("0-config.R"))


#-----------------------------------------------------------
# Merge mapsan env and child health
#-----------------------------------------------------------

env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study %in% c("Holcomb 2021","Capone 2021","Capone 2022 in prep") ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 


env <- env %>% filter(trial == "MapSan") %>% droplevels(.) %>%
  subset(., select = -c(momage, momedu, hhwealth, hhwealth_cont,
                        Nhh, nrooms, walls, floor, elec))

d <- env %>% filter(pos==0 & abund>0) %>% droplevels()
summary(d$abund)


colnames(env)
table(env$study, env$sample)

       
head(env)
table(env$Nhh)


ch <- readRDS(paste0(dropboxDir, "Data/MapSan/mapsan_child_cleaned.rds")) %>% mutate(trial="MapSan", dataid=clusterid)
colnames(ch)
ch$ch_surveydat
ch[ch$childid=="20950201",]


#rename variables to standardize
ch <- ch %>%
  rename(
    hhwealth=povNormal,
    Nhh=Hhsize,
    nrooms=hhrooms,
    walls=hh_walls,
    floor=hhCement,
    elec=compElec )%>%
  mutate(
    dataid=clusterid,
    diar7d_all=diar7d,
    momedu=case_when(ch_careEDUorig==0 ~"No education",
                     ch_careEDUorig==1 ~"Incomplete Primary",
                     ch_careEDUorig==2 ~"Primary",
                     ch_careEDUorig==3 ~"Secondary",
                     ch_careEDUorig==4 ~"Secondary",
                     ch_careEDUorig==5 ~"More than secondary",
                     ch_careEDUorig==6 ~"More than secondary",
                     ch_careEDUorig==7 ~"More than secondary",
                     is.na(ch_careEDUorig) ~"Missing"),
    tr=ifelse(studyArm_binary==1,"Sanitation","Control"),
    tr = factor(tr, levels = c("Control", "Sanitation"))
  )

ch <- ch %>% subset(., select=c(trial, round,child_date,child_date_pathogen,childid, female, age_months, dataid, hhid,clusterid, diar7d, diar7d_all, haz,whz,waz,
                                Kkasc,
                                KKtrc,
                                KKhkw,
                                gpp_aden, gpp_noro, gpp_rota, gpp_cdif,
                                gpp_camp, gpp_o157, gpp_etec, gpp_salm,
                                gpp_stec, gpp_shig, gpp_chol, gpp_yers,
                                gpp_cryp, gpp_enta, gpp_giar,
                                hhwealth, momedu, Nhh, nrooms, walls, floor, elec)) %>%
  rename(sex=female, age=age_months) %>% mutate(ch_data=1, agedays=age*30.4167, agedays_anthro=agedays)
head(ch)
table(ch$diar7d)


ch %>% group_by(sex) %>% summarise(mean(haz, na.rm=T), n())

saveRDS(ch, file = paste0(dropboxDir, "Data/WBK/clean-mapsan-diar.RDS"))




hol_res <- data.frame(
  study = "holcomb",
  env_samples_before_merge = nrow(env %>% filter(study=="Holcomb 2021") %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% filter(study=="Holcomb 2021")%>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(round!="el",!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(round!="el",!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, haz))
)

cp_res <- data.frame(
  study = "capone",
  env_samples_before_merge = nrow(env %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") ) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") )%>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, haz))
)

table(env$study, env$round)

env_samples_before_merge = env %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") ) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)
table(env_samples_before_merge$round)

#Merge diarrhea and growth separately. Growth 1-year later
# So merge holcomb BL to ML, and ML to EL
# Merge capone BL to ML, and ML to EL
#Merge concurrent diarrhea
ch_bl <- ch %>% filter(round=="bl")
ch_ml <- ch %>% filter(round=="ml")
ch_el <- ch %>% filter(round=="el")
cp_bl <- env %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") , round=="bl") 
cp_ml <- env %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") , round=="ml") 
cp_el <- env %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") , round=="el") 
hol_bl <- env %>% filter(study=="Holcomb 2021", round=="bl") 
hol_ml <- env %>% filter(study=="Holcomb 2021", round=="ml") 

summary(ch_bl$child_date)
summary(cp_bl$env_date)

summary(ch_ml$child_date)
summary(cp_ml$env_date)

summary(ch_bl$child_date)
summary(hol_bl$env_date)
summary(ch_ml$child_date[!is.na(ch_ml$haz)])
summary(hol_ml$env_date)

#Function to split data by hh versus compound samples
unique(env$sample)

env = hol_bl 
ch = ch_ml %>% subset(., select = -c(diar7d, round))


merge_ch <- function(env, ch){
  ch$ch_data <- 1
  env_compound <- env %>% filter(sample %in% c("LS","Fly","SW","any sample type")) %>% subset(., select = -c(hhid))
  env_hh <- env %>% filter(sample %in% c("S","MH", "CH", "W" ))

  d_compound <- left_join(env_compound, ch, by = c("trial","dataid","clusterid"))
  d_hh <- NULL
  if(nrow(env_hh)>0){
    d_hh <- left_join(env_hh, ch, by = c("trial","dataid","clusterid", "hhid"))
  }

  d<-bind_rows(d_compound,d_hh)
  return(d)
}

summary(cp_el$env_date)
summary(ch_el$child_date)


# cp_anthro_bl <- merge_ch(cp_bl, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
# cp_anthro_ml <- merge_ch(cp_ml, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
# cp_anthro_el <- merge_ch(cp_el, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="el") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))

cp_anthro_bl <- merge_ch(cp_bl, ch_bl %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
cp_anthro_ml <- merge_ch(cp_ml, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
cp_anthro_el <- merge_ch(cp_el, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="el") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))

cp_anthro_bl_ml <- merge_ch(cp_bl, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
cp_anthro_ml_el <- merge_ch(cp_ml, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))


cp_diar_bl <- merge_ch(cp_bl, ch_bl %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="bl") %>% filter(!is.na(diar7d),!is.na(pos))
cp_diar_ml <- merge_ch(cp_ml, ch_ml %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="ml") %>% filter(!is.na(diar7d),!is.na(pos))
cp_diar_el <- merge_ch(cp_el, ch_el %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="el") %>% filter(!is.na(diar7d),!is.na(pos))

# hol_anthro_bl <- merge_ch(hol_bl, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl") #%>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
# hol_anthro_ml <- merge_ch(hol_ml, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
hol_anthro_bl <- merge_ch(hol_bl, ch_bl %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl", anthro_round="bl") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
hol_anthro_ml <- merge_ch(hol_ml, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml", anthro_round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
hol_anthro_bl_ml <- merge_ch(hol_bl, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl", anthro_round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
hol_anthro_ml_el <- merge_ch(hol_ml, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml", anthro_round="el") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))

summary(as.numeric(hol_anthro_bl$child_date-hol_anthro_bl$env_date))
summary(as.numeric(hol_anthro_ml$child_date-hol_anthro_ml$env_date))
summary(as.numeric(hol_anthro_bl_ml$child_date-hol_anthro_bl_ml$env_date))
summary(as.numeric(hol_anthro_ml_el$child_date-hol_anthro_ml_el$env_date))

hol_diar_bl <- merge_ch(hol_bl, ch_bl %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="bl") %>% filter(!is.na(diar7d),!is.na(pos))
hol_diar_ml <- merge_ch(hol_ml, ch_ml %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="ml") %>% filter(!is.na(diar7d),!is.na(pos))


cp_df <- bind_rows(cp_anthro_bl, cp_anthro_ml, cp_anthro_el, cp_diar_bl, cp_diar_ml, cp_diar_el,cp_anthro_bl_ml,cp_anthro_ml_el)
hol_df <- bind_rows(hol_anthro_bl, hol_anthro_ml, hol_anthro_bl_ml, hol_anthro_ml_el, hol_diar_bl, hol_diar_ml)

#holcomb -same round matches exactly- offset env. data one day earlier
#hol_df <- hol_df %>% mutate(env_date = env_date-1)

# cp_df <- bind_rows(cp_anthro_bl, cp_anthro_ml, cp_anthro_el, cp_diar_bl, cp_diar_ml, cp_diar_el)
# hol_df <- bind_rows(hol_anthro_bl, hol_anthro_ml, hol_diar_bl, hol_diar_ml)

# cp_df <- bind_rows(cp_anthro_el, cp_diar_bl, cp_diar_ml, cp_diar_el, cp_anthro_bl_ml, cp_anthro_ml_el)
#hol_df <- bind_rows(hol_anthro_bl_ml, hol_anthro_ml_el, hol_diar_bl, hol_diar_ml)
d <- bind_rows(cp_df, hol_df)



cp_res$env_samples_after_merge <- nrow(cp_df %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
cp_res$env_HH_after_merge <- nrow(cp_df %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
cp_res$diar_samples_after_merge <- nrow(cp_df %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, diar7d))
cp_res$haz_samples_after_merge <- nrow(cp_df %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, haz))
cp_res$samples_with_diar_after_merge <- nrow(cp_df %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))
cp_res$samples_with_haz_after_merge <- nrow(cp_df %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))
cp_res$samples_with_ch_after_merge <- nrow(cp_df %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, clusterid,sample, round))


hol_res$env_samples_after_merge <- nrow(hol_df %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
hol_res$env_HH_after_merge <- nrow(hol_df %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
hol_res$diar_samples_after_merge <- nrow(hol_df %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, diar7d))
hol_res$haz_samples_after_merge <- nrow(hol_df %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, age,    sex, haz))
hol_res$samples_with_diar_after_merge <- nrow(hol_df %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
hol_res$samples_with_haz_after_merge <- nrow(hol_df %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
hol_res$samples_with_ch_after_merge <- nrow(hol_df %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))

summary(d$env_date)

#Fix 2025 date
d$env_date[d$env_date=="2025-06-25" ] <- "2015-06-25"
  
# summary(as.numeric(d$child_date- d$env_date))
# date_diff <- d %>% filter(!is.na(haz), target=="Any pathogen", sample=="any sample type") %>% 
#   mutate(date_diff = child_date-env_date) %>% select(study, sampleid, target, dataid, round, hhid, date_diff, pos, diar7d, haz) %>% distinct()
# table(date_diff$date_diff)
# 
# date_diff2 <- date_diff %>% filter(date_diff > -16, date_diff <500)
# table(date_diff2$date_diff)
# ggplot(date_diff2, aes(x=date_diff, y=haz)) + geom_point() + facet_wrap(~pos) + geom_smooth()
# 
# date_diff[date_diff$date_diff<0,]
# temp <-d[d$sampleid=="2095_bl",]
# temp%>% filter(!is.na(haz), target=="Any pathogen", sample=="any sample type", study=="Capone 2022 in prep") %>% 
#   mutate(date_diff = child_date-env_date)




d <- d %>%
  filter(child_date + 14 > env_date) %>% #add time because child data had been coarsened to the month to be avoid excluding too much data
  mutate(
    diar7d_full=diar7d,
    date_diff = as.numeric(child_date-env_date),
    haz_full=haz,
    #haz = ifelse(child_date-env_date > 365, NA, haz),
    diar7d = ifelse(child_date-env_date > 124, NA, diar7d))

table(d$study, d$date_diff)

# df <- d %>% filter(study=="Holcomb 2021", sample=="SW", target=="Any MST")
# table(df$round)
# 
# df <- d %>% filter(study=="Holcomb 2021", sample=="SW", target=="Any MST")
# summary(df$date_diff)
# table(df$round, df$date_diff, df$anthro_round)
# 
# df %>% filter(sample=="SW", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
# df %>% filter(sample=="SW", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos, date_diff>0) %>%
#   summarize(N=n(),mn=mean(haz), date_diff=mean(date_diff)) %>% group_by( study, sample, target, date_diff>0) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
# 
# head(df)
# temp <- d %>% filter(study=="Holcomb 2021") %>% group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   summarize(N=n())
# table(temp$N)
# 
# temp[temp$N==3,]
# 
# temp <- d %>% filter(sampleid=="308100ls12m", target=="Any MST", !is.na(haz), sample=="LS", childid=="30810101")
# temp
# 
# d2<-d %>% mutate(date_diff = child_date-env_date, date_diff2=ifelse(date_diff<= 0, 9999,date_diff)) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   mutate(N=n()) %>% filter(N==2) %>% filter(sample=="any sample type", target=="Any pathogen", !is.na(haz)) %>%
#   select(study, date_diff, round, pos, haz) %>% arrange(childid, round) %>% mutate(meas=ifelse(date_diff==min(date_diff),"first","second"))
# d2 %>% group_by( study, sample, target,pos, meas) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target, meas) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))


# d3 <- df %>% mutate(date_diff = child_date-env_date, date_diff2=ifelse(date_diff<= 0, 9999,date_diff)) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>% 
#   mutate(N=n()) %>% filter(N==1) %>% filter(sample=="any sample type", target=="Any pathogen", !is.na(haz)) %>%
#   select(study, date_diff, round, pos, haz) %>% arrange(childid, round) 
# head(d3)
# 

# table(d$study)
# df2 <- d %>% group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   #filter(date_diff> -14 & date_diff<=365) %>%
#   filter(date_diff> -14 & date_diff<=365) %>%
#   mutate(N=n(), meas=ifelse(date_diff==max(date_diff),"second","first")) %>% filter(meas=="second")
# table(df2$N)
# table(df2$study)


# 
# df2 %>% filter(sample=="any sample type", target=="Any pathogen", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
# df2 %>% filter(sample=="any sample type", target=="Any pathogen", sex==0, !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))

# df2 <- d %>% #filter(date_diff> -14 & date_diff<=500) %>%
#   mutate(date_diff2=ifelse(date_diff> 31*12, -9999,as.numeric(date_diff))) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   mutate(N=n()) %>% mutate(meas=ifelse(date_diff2==max(date_diff2),"second","first")) %>%
#   filter(meas=="second")

# temp <- d %>% filter(sample=="any sample type", target=="Any pathogen", sex==0, !is.na(haz)) %>% select(date_diff, pos, haz) %>% arrange(date_diff)
# ggplot(temp, aes(x=date_diff, y=haz, group=pos, color=pos)) + geom_point() + geom_smooth() +facet_wrap(~pos)
# 
# temp<- d %>% filter(sample=="any sample type", target=="Any pathogen", sex==0, !is.na(haz)) %>% mutate(week=floor(date_diff/7)) %>%
#   group_by( week, pos) %>% summarize(N=n(), mn=mean(haz, na.rm=T))
# # ggplot(temp, aes(x=week, y=mn)) + geom_point() + facet_wrap(~pos) + geom_smooth()
# ggplot(temp, aes(x=week, y=mn, group=pos, color=pos)) + geom_point() + geom_smooth() #+theme(legend.position = "bottom")
# 
# ggplot(d %>% filter(sample=="any sample type", target=="Any pathogen", sex==0, !is.na(haz)) , aes(x=date_diff, y=haz, group=pos, color=pos)) + geom_point() + geom_smooth() #+theme(legend.position = "bottom")

# df2 <- d %>% #filter(date_diff> -14, date_diff< 30*100) %>%
#   mutate(date_diff2=ifelse(date_diff<= 0, 9999,as.numeric(date_diff))) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   mutate(N=n()) %>% mutate(meas=ifelse(date_diff2==min(date_diff2),"first","second")) %>%
#   filter(meas=="first")
# 
# dim(d)
# gap <-
#d$date_diff[d$study=="Holcomb 2021"] <- d$date_diff[d$study=="Holcomb 2021"]+1 

#capone: preferentially pick theCH  measurement after the env obs but not too far
df2_capone <- d %>% filter(study!="Holcomb 2021") %>%
  mutate(date_diff2=ifelse(date_diff <= 0, 9999,as.numeric(date_diff)),
         date_diff2=ifelse(date_diff> 18*30, 99999,as.numeric(date_diff))
         ) %>%
  group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
  filter(abs(date_diff2)==max(abs(date_diff2)))
#holcomb: better time matched so preferentially pick the CH measurement proximate but after (ensuring aren't any that are possibly before from date coarsening)

# Ndf <-  d %>% filter(study=="Holcomb 2021") %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   summarize(N=n())
# dim(Ndf)
# table(Ndf$N)
# table(Ndf$N)[1] + table(Ndf$N)[2]/2


# temp <-  d %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   mutate(N=n()) %>% ungroup() %>% filter(N>1) %>% arrange(childid)
# table(temp$N)
# 
# temp$childid
# temp <- temp %>% filter(childid==20840401)
# temp2 <- temp  %>% filter(study=="Holcomb 2021") %>%
#   # mutate(date_diff2=ifelse(date_diff < 0, 99999,as.numeric(date_diff)),
#   #        date_diff2=ifelse(date_diff> 18*30, 9999,as.numeric(date_diff))
#   # ) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   #filter(abs(date_diff2)==min(abs(date_diff2)))
#   filter(date_diff==max(date_diff)) %>%
#   slice(1)
# dim(temp)
# dim(temp2)




df2_holcomb <- d %>% filter(study=="Holcomb 2021")
dim(df2_holcomb)
table(df2_holcomb$date_diff)
df2_holcomb <- df2_holcomb %>%
  mutate(date_diff2=ifelse(date_diff <= 0, 99999,as.numeric(date_diff))) %>%
   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
  arrange(date_diff2) %>%
  slice(1)
# 
# df2_holcomb <- df2_holcomb %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   arrange(date_diff) %>%
#   slice(1)
# 
# df2_holcomb <- df2_holcomb %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   arrange(-date_diff) %>%
#   slice(1)
# 
# # df2_holcomb <- df2_holcomb %>% filter(date_diff>0)
# #   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
# #   arrange(date_diff) %>%
# #   slice(1)
# 
# dim(df2_holcomb)
# 
# summary(df2_holcomb$date_diff)
# summary(df2_holcomb$date_diff2)
# 
# # df2_holcomb %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
# #   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(sparseN=min(N), N=sum(N),effect_of_pos=last(mn)-first(mn))
# # 
# # df2_holcomb %>% filter(sample=="SW", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
# #   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(sparseN=min(N), N=sum(N),effect_of_pos=last(mn)-first(mn))
# 
# df2_holcomb %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) 
# 
# df2_holcomb %>% filter(sample=="SW", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) 
# df2_holcomb %>% filter(sample=="W", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) 
# df2_holcomb %>% filter(sample=="S", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz))
# 
# df2_capone %>% filter(sample=="any sample type", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) 
# df2_holcomb1 <- d %>% filter(study=="Holcomb 2021") %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   filter(date_diff==min(date_diff)) 
# df2_holcomb2 <- d %>% filter(study=="Holcomb 2021") %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   filter(date_diff==max(date_diff)) 
# 
# df2_holcomb1 %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
# df2_holcomb2 %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
# 
# Ndf2 <-  df2_holcomb %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>%
#   group_by(study, sampleid, target, sample, sex,dataid, round, hhid, childid,  is.na(diar7d)) %>%
#   summarize(N=n())
# dim(Ndf2)
# table(Ndf2$N)


dim(df2_holcomb)

df2 <- bind_rows(df2_capone, df2_holcomb)

#table(df2$study, df2$date_diff)
# df3 <- df2 %>% filter(study=="Holcomb 2021", sample=="SW", target=="Any MST")
# summary(df$date_diff)
# table(df$date_diff)
# 
# temp2 <- df2 %>% filter(sampleid=="308100ls12m", target=="Any MST", !is.na(haz), sample=="LS", childid=="30810101")
# temp2$anthro_round
# temp2$date_diff2

df2 %>% filter(sample=="any sample type", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
  summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))


df2 %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
  summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))

df2 %>% filter(sample=="SW", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
  summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
df2 %>% filter(sample=="SW", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos, date_diff>0) %>%
  summarize(N=n(),mn=mean(haz), date_diff=mean(date_diff)) %>% group_by( study, sample, target, date_diff>0) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))



# df2 %>% filter(sample=="any sample type", target=="Any pathogen", sex==0, !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
# 
# df2 %>% filter(sample!="any sample type", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
#   summarize(N=n(),mn=mean(haz)) %>% group_by( study, sample, target) %>%  summarize(N=sum(N),effect_of_pos=last(mn)-first(mn))
# 
try(d<-df2 %>% subset(., select = -c(date_diff, haz_full, `is.na(diar7d)`, N, meas)))
try(d<-df2 %>% subset(., select = -c(date_diff, haz_full, `is.na(diar7d)`)))



table(d$pos, d$diar7d_full)
table(d$pos, d$diar7d)
table(d$pos, d$Kkasc)
table(d$pos, !is.na(d$haz))

#quartile HH wealth
d$hhwealth_cont <- d$hhwealth
d$hhwealth<-NA
#quantile hhwealth by study
i=unique(d$study)[1]
for(i in unique(d$study)){
  df <- d[d$study==i,]
  hhwealth <- factor(NA)
  if(length(unique(df$hhwealth_cont))>4){
    hhwealth=factor(quantcut(df$hhwealth_cont, na.rm=T), labels=c("1","2","3","4"))
  }
  d$hhwealth[d$study==i] <- hhwealth
}
d$hhwealth <- factor(d$hhwealth, labels=c("1","2","3","4"))
d$hhwealth = fct_explicit_na(d$hhwealth, na_level = "Missing")


d <- d %>% 
  mutate(#hhwealth=factor(ntile(hhwealth,4), levels=c("1","2","3","4")),
    Nhh=factor(case_when(
      Nhh<5 ~ "<5",
      Nhh>=5 & Nhh <=8 ~ "5-8",
      Nhh>8 ~ ">8"
    ), levels=c("5-8","<5",">8")),
    Nhh=fct_explicit_na(Nhh, na_level = "Missing"),
    nrooms=as.numeric(nrooms),
    nrooms=factor(case_when(
      nrooms<3 ~ "1-2",
      nrooms>2  ~ ">3",
    ), levels=c("1-2",">3")),
    nrooms=fct_explicit_na(nrooms, na_level = "Missing"),
    landown=fct_explicit_na(landown, na_level = "Missing"),
    dadagri=factor(dadagri),
    dadagri=fct_explicit_na(dadagri, na_level = "Missing"),
    momedu=factor(momedu),
    momedu=fct_explicit_na(momedu, na_level = "Missing"),
    hfiacat=factor(hfiacat),
    hfiacat=fct_explicit_na(hfiacat, na_level = "Missing"),
    walls=factor(walls),
    walls=fct_explicit_na(walls, na_level = "Missing"),
    roof=factor(roof),
    roof=fct_explicit_na(roof, na_level = "Missing"),
    floor=factor(floor),
    floor=fct_explicit_na(floor, na_level = "Missing"),
    elec=factor(elec),
    elec=fct_explicit_na(elec, na_level = "Missing")
  )

# cp_res$diar_samples_date_dropped <- cp_res$samples_with_diar_after_merge - nrow(d %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") ) %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid, clusterid,sample, round))
# cp_res$haz_samples_date_dropped <- cp_res$samples_with_haz_after_merge - nrow(d %>% filter(study %in% c("Capone 2021","Capone 2022 in prep") ) %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid, clusterid,sample, round))
# 
# # hol_res$diar_samples_date_dropped <- hol_res$samples_with_diar_after_merge - nrow(d %>% filter(study=="Holcomb 2021") %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid, hhid, age,    sex, diar7d))
# # hol_res$haz_samples_date_dropped <- hol_res$samples_with_haz_after_merge - nrow(d %>% filter(study=="Holcomb 2021") %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid, hhid, age,    sex, haz))
# 
# hol_res$diar_samples_date_dropped <- hol_res$samples_with_diar_after_merge - nrow(d %>% filter(study=="Holcomb 2021") %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
# hol_res$haz_samples_date_dropped <- hol_res$samples_with_haz_after_merge - nrow(d %>% filter(study=="Holcomb 2021") %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
# 
# 
# 
# cp_res$percent_diar_samples_dropped <- 100 - cp_res$diar_samples_after_merge/cp_res$diar_samples_before_merge * 100
# cp_res$percent_haz_samples_dropped <- 100 - cp_res$haz_samples_after_merge/cp_res$haz_samples_before_merge * 100
# cp_res$percent_env_samples_with_diar <- cp_res$samples_with_diar_after_merge/cp_res$env_samples_before_merge  * 100
# cp_res$percent_env_samples_with_haz <-  cp_res$samples_with_haz_after_merge/cp_res$env_samples_before_merge  * 100
# cp_res$percent_env_samples_with_ch <-  cp_res$samples_with_ch_after_merge/cp_res$env_samples_before_merge  * 100
# 
# 
# hol_res$percent_diar_samples_dropped <- 100 - hol_res$diar_samples_after_merge/hol_res$diar_samples_before_merge * 100
# hol_res$percent_haz_samples_dropped <- 100 - hol_res$haz_samples_after_merge/hol_res$haz_samples_before_merge * 100
# hol_res$percent_env_samples_with_diar <- hol_res$samples_with_diar_after_merge/hol_res$env_samples_before_merge  * 100
# hol_res$percent_env_samples_with_haz <-  hol_res$samples_with_haz_after_merge/hol_res$env_samples_before_merge  * 100
# hol_res$percent_env_samples_with_ch <-  hol_res$samples_with_ch_after_merge/hol_res$env_samples_before_merge  * 100
# 
# cp_res$diar_samples_date_dropped[is.na(cp_res$diar_samples_date_dropped )] <- 0
# cp_res$haz_samples_date_dropped[is.na(cp_res$haz_samples_date_dropped )] <- 0
# 
# cp_res <- cp_res %>% 
#   mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
#          per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)
# 
# hol_res$diar_samples_date_dropped[is.na(hol_res$diar_samples_date_dropped )] <- 0
# hol_res$haz_samples_date_dropped[is.na(hol_res$haz_samples_date_dropped )] <- 0
# 
# hol_res <- hol_res %>% 
#   mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
#          per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)
# 
# mapsan_res <- bind_rows(cp_res, hol_res)


saveRDS(d, file=paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))
#saveRDS(mapsan_res, file=paste0(here(),"/results/mapsan_merge_Ns.rds"))
#saveRDS(date_diff, file=paste0(here(),"/results/mapsan_date_diff.rds"))

d %>% filter(sample=="LS", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
  summarize(N=n(),mn=mean(haz)) 
d %>% filter(sample=="SW", target=="Any MST", !is.na(haz)) %>% group_by( study, sample, target,pos) %>%
  summarize(N=n(),mn=mean(haz)) 