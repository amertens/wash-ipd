
rm(list=ls())
source(here::here("0-config.R"))


#-----------------------------------------------------------
# Merge mapsan env and child health
#-----------------------------------------------------------

env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study %in% c("Holcomb 2020","Capone 2021","Capone 2021 in prep") ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 


env <- env %>% filter(trial == "MapSan") %>% droplevels(.) %>%
  subset(., select = -c(momage, momedu, hhwealth, hhwealth_cont,
                        Nhh, nrooms, walls, floor, elec))

colnames(env)
table(env$study, env$sample)

       
head(env)
table(env$Nhh)


ch <- readRDS(paste0(dropboxDir, "Data/MapSan/mapsan_child_cleaned.rds")) %>% mutate(trial="MapSan", dataid=clusterid)
colnames(ch)

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

ch <- ch %>% subset(., select=c(trial, round,child_date,childid, female, age_months, dataid, hhid,clusterid, diar7d, diar7d_all, haz,whz,waz,
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

saveRDS(ch, file = paste0(dropboxDir, "Data/WBK/clean-mapsan-diar.RDS"))




hol_res <- data.frame(
  study = "holcomb",
  env_samples_before_merge = nrow(env %>% filter(study=="Holcomb 2020") %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% filter(study=="Holcomb 2020")%>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(round!="el",!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(round!="el",!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, haz))
)

cp_res <- data.frame(
  study = "capone",
  env_samples_before_merge = nrow(env %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") ) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") )%>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(ch %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, diar7d)),
  haz_samples_before_merge = nrow(ch %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, child_date, age,    sex, childid, haz))
)

table(env$study, env$round)

env_samples_before_merge = env %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") ) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)
table(env_samples_before_merge$round)

#Merge diarrhea and growth separately. Growth 1-year later
# So merge holcomb BL to ML, and ML to EL
# Merge capone BL to ML, and ML to EL
#Merge concurrent diarrhea
ch_bl <- ch %>% filter(round=="bl")
ch_ml <- ch %>% filter(round=="ml")
ch_el <- ch %>% filter(round=="el")
cp_bl <- env %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") , round=="bl") 
cp_ml <- env %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") , round=="ml") 
cp_el <- env %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") , round=="el") 
hol_bl <- env %>% filter(study=="Holcomb 2020", round=="bl") 
hol_ml <- env %>% filter(study=="Holcomb 2020", round=="ml") 




#Function to split data by hh versus compound samples
unique(env$sample)

env = hol_bl 
ch = ch_ml %>% subset(., select = -c(diar7d, round))


merge_ch <- function(env, ch){
  ch$ch_data <- 1
  #env_compound <- env %>% filter(sample %in% c("LS","FlyLat","FlyKitch","SW","any sample type")) %>% subset(., select = -c(hhid))
  env_compound <- env %>% filter(sample %in% c("LS","Fly","SW","any sample type")) %>% subset(., select = -c(hhid))
  env_hh <- env %>% filter(sample %in% c("S","MH", "CH", "W" ))
  
  d_compound <- left_join(env_compound, ch, by = c("trial","dataid","clusterid"))
  d_hh <- NULL
  if(nrow(env_hh)>0){
    d_hh <- left_join(env_hh, ch, by = c("trial","dataid","clusterid", "hhid"))
  }
  #table(d_hh$study, d_hh$sample)
  
  d<-bind_rows(d_compound,d_hh)
  #d <- d %>% filter(!is.na(sample), !is.na(ch_data))
  return(d)
}


cp_anthro_bl <- merge_ch(cp_bl, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
cp_anthro_ml <- merge_ch(cp_ml, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
cp_anthro_el <- merge_ch(cp_el, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))

cp_diar_bl <- merge_ch(cp_bl, ch_bl %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="bl") %>% filter(!is.na(diar7d),!is.na(pos))
cp_diar_ml <- merge_ch(cp_ml, ch_ml %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="ml") %>% filter(!is.na(diar7d),!is.na(pos))
cp_diar_el <- merge_ch(cp_el, ch_el %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="el") %>% filter(!is.na(diar7d),!is.na(pos))

hol_anthro_bl <- merge_ch(hol_bl, ch_ml %>% subset(., select = -c(diar7d, round))) %>% mutate(round="bl") #%>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))
hol_anthro_ml <- merge_ch(hol_ml, ch_el %>% subset(., select = -c(diar7d, round))) %>% mutate(round="ml") %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz),!is.na(pos))

hol_diar_bl <- merge_ch(hol_bl, ch_bl %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="bl") %>% filter(!is.na(diar7d),!is.na(pos))
hol_diar_ml <- merge_ch(hol_ml, ch_ml %>% subset(., select =  -c(haz, whz, waz, round))) %>% mutate(round="ml") %>% filter(!is.na(diar7d),!is.na(pos))




cp_df <- bind_rows(cp_anthro_bl, cp_anthro_ml, cp_anthro_el, cp_diar_bl, cp_diar_ml, cp_diar_el)
hol_df <- bind_rows(hol_anthro_bl, hol_anthro_ml, hol_diar_bl, hol_diar_ml)
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


summary(as.numeric(d$child_date- d$env_date))
date_diff <- d %>% mutate(date_diff = child_date-env_date) %>% select(study, sampleid, target, dataid, round, hhid, date_diff, diar7d, haz) %>% distinct()

d <- d %>% 
  filter(child_date +14 >=env_date) %>% #add 14 because child data had been coarsened to the month
  mutate(
    diar7d_full=diar7d,
    diar7d = ifelse(child_date-env_date > 124, NA, diar7d))
table(d$pos, d$diar7d)
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

cp_res$diar_samples_date_dropped <- cp_res$samples_with_diar_after_merge - nrow(d %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") ) %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid, clusterid,sample, round))
cp_res$haz_samples_date_dropped <- cp_res$samples_with_haz_after_merge - nrow(d %>% filter(study %in% c("Capone 2021","Capone 2021 in prep") ) %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid, clusterid,sample, round))

# hol_res$diar_samples_date_dropped <- hol_res$samples_with_diar_after_merge - nrow(d %>% filter(study=="Holcomb 2020") %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid, hhid, age,    sex, diar7d))
# hol_res$haz_samples_date_dropped <- hol_res$samples_with_haz_after_merge - nrow(d %>% filter(study=="Holcomb 2020") %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid, hhid, age,    sex, haz))

hol_res$diar_samples_date_dropped <- hol_res$samples_with_diar_after_merge - nrow(d %>% filter(study=="Holcomb 2020") %>% do(drop_agg(.)) %>% filter(!is.na(diar7d)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
hol_res$haz_samples_date_dropped <- hol_res$samples_with_haz_after_merge - nrow(d %>% filter(study=="Holcomb 2020") %>% do(drop_agg(.)) %>% filter(!is.na(haz)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))



cp_res$percent_diar_samples_dropped <- 100 - cp_res$diar_samples_after_merge/cp_res$diar_samples_before_merge * 100
cp_res$percent_haz_samples_dropped <- 100 - cp_res$haz_samples_after_merge/cp_res$haz_samples_before_merge * 100
cp_res$percent_env_samples_with_diar <- cp_res$samples_with_diar_after_merge/cp_res$env_samples_before_merge  * 100
cp_res$percent_env_samples_with_haz <-  cp_res$samples_with_haz_after_merge/cp_res$env_samples_before_merge  * 100
cp_res$percent_env_samples_with_ch <-  cp_res$samples_with_ch_after_merge/cp_res$env_samples_before_merge  * 100


hol_res$percent_diar_samples_dropped <- 100 - hol_res$diar_samples_after_merge/hol_res$diar_samples_before_merge * 100
hol_res$percent_haz_samples_dropped <- 100 - hol_res$haz_samples_after_merge/hol_res$haz_samples_before_merge * 100
hol_res$percent_env_samples_with_diar <- hol_res$samples_with_diar_after_merge/hol_res$env_samples_before_merge  * 100
hol_res$percent_env_samples_with_haz <-  hol_res$samples_with_haz_after_merge/hol_res$env_samples_before_merge  * 100
hol_res$percent_env_samples_with_ch <-  hol_res$samples_with_ch_after_merge/hol_res$env_samples_before_merge  * 100

cp_res$diar_samples_date_dropped[is.na(cp_res$diar_samples_date_dropped )] <- 0
cp_res$haz_samples_date_dropped[is.na(cp_res$haz_samples_date_dropped )] <- 0

cp_res <- cp_res %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)

hol_res$diar_samples_date_dropped[is.na(hol_res$diar_samples_date_dropped )] <- 0
hol_res$haz_samples_date_dropped[is.na(hol_res$haz_samples_date_dropped )] <- 0

hol_res <- hol_res %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)

mapsan_res <- bind_rows(cp_res, hol_res)


saveRDS(d, file=paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))
saveRDS(mapsan_res, file=paste0(here(),"/results/mapsan_merge_Ns.rds"))
saveRDS(date_diff, file=paste0(here(),"/results/mapsan_date_diff.rds"))


summary(d$hhwealth_cont)
table(d$study, (d$hhwealth))
table(d$sample, (d$floor))
table(d$sample, (d$Nhh))

