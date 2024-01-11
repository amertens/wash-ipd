

rm(list=ls())
source(here::here("0-config.R"))

#-----------------------------------------------------------
# Merge WBB
#-----------------------------------------------------------
env <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
env <- env %>% mutate(
  trial = case_when(study %in% c("Fuhrmeister 2020", "Kwong 2021", "Boehm 2016") ~ "WBB",
                    study=="Steinbaum 2019" ~ "WBK",
                    study=="Holcomb 2020" ~ "MapSan",
                    study=="Reese 2017" ~ "Gram Vikas",
                    study=="Odagiri 2016" ~ "Odisha")) 


env_wbb <- env %>% filter(trial == "WBB", !is.na(pos)) %>% droplevels(.)

table(env_wbb$study)

#Subset to HHID covariates
colnames(env_wbb)
cov <- env_wbb %>% distinct(dataid, clusterid, hhid, Nhh,          momage,      
                      momedu,       dadagri,      landown,      landacre,     hfiacat,      
                     watmin,       floor,       hhwealth_cont,roof,         elec,         walls,        nrooms)     
head(cov)

cov$hhwealth=factor(quantcut(cov$hhwealth_cont, na.rm=T), labels=c("1","2","3","4"))
cov$hhwealth <- factor(cov$hhwealth, labels=c("1","2","3","4"))
cov$hhwealth = fct_explicit_na(cov$hhwealth, na_level = "Missing")


env_wbb <- env_wbb %>% subset(., select = -c(Nhh,          momage,      
                                momedu,       dadagri,      landown,      landacre,     hfiacat,      
                                watmin,       floor,       hhwealth_cont,roof,         elec,         walls,        nrooms))

wbb <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS")) %>% 
  rename(haz=laz) %>%
  mutate(trial="WBB",
         aged=agedays
  )

tab <- table(wbb$tr, wbb$diar7d)
(tab[2,2]*tab[1,1])/(tab[2,1]*tab[1,2])  

#load clean dataset from the diarhhea-pathogen analysis
wbb_sth <- readRDS(paste0(dropboxDir, "Data/WBB/clean/clean_wbb_sth_pathogens.rds"))


colnames(wbb_sth)

table(wbb_sth$hhid)

wbb_sth <- wbb_sth %>% subset(., select = c(
  dataid,hhid,childid, block,  svyweek, svyyear, sex,             agedays,         
  clusterid,       logalepg,        loghwepg,        
  logttepg,       posgi,           poseh,           poscr,           
  posprot,         posmult,         ctgi,            cteh,            
  ctcr,            qpcr.positive.Ac,qpcr.positive.Ad,qpcr.positive.Al,
  qpcr.positive.IAC,qpcr.positive.Na,qpcr.positive.Ss,qpcr.positive.Tt,
  qpcr.positive.Hw,qpcr.positive.Sth,qpcr.CTmean.Ac,  qpcr.CTmean.Ad,  
  qpcr.CTmean.Al,  qpcr.CTmean.IAC, qpcr.CTmean.Na,  qpcr.CTmean.Ss,  
  qpcr.CTmean.Tt)) %>% 
  filter(!is.na(logalepg) | !is.na(loghwepg) | !is.na(logttepg) | 
           !is.na(posgi) | !is.na(poseh) | !is.na(poscr) | !is.na(posprot) | 
        !is.na(posmult) | !is.na(qpcr.positive.Ac) | !is.na(qpcr.positive.Ad) |
          !is.na(qpcr.positive.Al) | !is.na(qpcr.positive.IAC) | !is.na(qpcr.positive.Na) |   
          !is.na(qpcr.positive.Ss) | !is.na(qpcr.positive.Tt) | !is.na(qpcr.positive.Sth))  %>%
  rename(
         aged_pathogen = agedays) %>%
  mutate(month=ceiling(svyweek/52*12),
         #child_date_pathogen= dmy(paste0("15-",month, "-", svyyear)) #Fix year- it was 2 years after intervention?
         ) %>%
  subset(., select = -c(month))  %>% 
  subset(., select = -c(posmult,
                                                           loghwepg,
                                                           qpcr.CTmean.Na, #don't have hookworm in soil
                                                           qpcr.CTmean.Ad,
                                                           qpcr.positive.Ad,
                                                           qpcr.positive.Na,
                                                           qpcr.positive.Hw,
                                                           qpcr.positive.Sth,
                                                           qpcr.CTmean.Ss, #no threadworm
                                                           qpcr.positive.Ss,
                                                           qpcr.positive.Ac, #Figure out what Ac and IAC are  
                                                           qpcr.positive.IAC,  
                                                           qpcr.CTmean.Ac,      
                                                           qpcr.CTmean.IAC)) %>%
  rename(
    ch_abund_ascaris=logalepg,
    ch_abund_trichuris=logttepg,
    ch_pos_giardia=posgi,
    ch_pos_entamoeba=poseh,
    ch_pos_crypto=poscr,
    ch_abund_giardia=ctgi,
    ch_abund_entamoeba=cteh,
    ch_abund_crypto=ctcr,
    ch_qpcr_pos_trichuris=qpcr.positive.Tt,
    ch_qpcr_abund_trichuris=qpcr.CTmean.Tt,
    ch_qpcr_pos_ascaris=qpcr.positive.Al,
    ch_qpcr_abund_ascaris=qpcr.CTmean.Al) %>%
  mutate(ch_pos_ascaris = 1*(ch_abund_ascaris>0), 
         ch_pos_trichuris = 1*(ch_abund_trichuris>0))


head(env_wbb)
head(wbb_sth)

#get STH dates:
# library(haven)
# sth_dates <- read_dta("C:/Users/andre/Downloads/WASHB-PSTH-Day1survey_STHCohort.dta") %>% mutate(child_date_sth=ymd(EntryDate), dataid=as.numeric(dataid)) %>% subset(., select= c(dataid, child_date_sth))
# head(sth_dates$child_date_sth)
# summary(sth_dates$child_date_sth)


sth_dates <- read.csv("C:/Users/andre/Dropbox/IPD WASH/Data/WBB/wbb-parasite.csv") %>% 
  mutate(child_date_sth=dmy(date), dataid=as.numeric(dataid)) %>% rename(childid=personid) %>% 
  subset(., select= c(dataid,  childid, child_date_sth))
summary(sth_dates$child_date_sth)

unique(wbb_sth$dataid_r)
unique(wbb_sth$dataid)
unique(sth_dates$dataid)

unique(wbb_sth$childid)
unique(sth_dates$childid)

dim(wbb_sth)
dim(sth_dates)
wbb_sth <- left_join(wbb_sth,sth_dates, by=c("dataid","childid"))
table(is.na(wbb_sth$child_date_sth))
dim(wbb_sth)
summary(wbb_sth$svyyear)
summary(wbb_sth$child_date_sth)


# add the public conversion IDs
IDs <- read.csv(paste0(dropboxDir, "Data/WBB/public-ids.csv"))
wbb_sth <- wbb_sth %>% rename(block_r=block,	clusterid_r=clusterid,	dataid_r=dataid)
wbb_sth <- left_join(wbb_sth, IDs, by = c("block_r",	"clusterid_r",	"dataid_r"))
head(wbb_sth)


#--------------------------------------------------------------
# Clean TAC data to pathogens in the environment
#--------------------------------------------------------------
EE_TAC <- readRDS(paste0(dropboxDir,"Data/WBB/data.UVA.relativeCt_spikeinAdjusted_Caitlin.RDS"))
EE_TAC$ETEC_ST

EE_TAC <- EE_TAC %>% 
  mutate(ch_pos_path_ecoli=1*(ETEC_LT+EAEC+ETEC_ST+ETEC.any +tEPEC+ aEPEC+ EPEC.any+STEC > 0),
         ch_pos_adenovirus=1*(`Adenovirus 40/41` +  `Adenovirus pan`> 0)) %>%
  rename(ch_abund_giardia_EE=Giardia,
         ch_abund_norovirus=Norovirus.any,
         ch_abund_crypto_EE =Cryptosporidium,
         ch_abund_salmonella=Salmonella,
         ch_abund_ascaris_EE=Ascaris,
         ch_abund_trichuris_EE=Trichuris,
         ch_abund_entamoeba_EE=E.histolytica,
         ch_abund_cholera=V.cholerae,
         ch_abund_cdiff=C.difficile,
         ch_abund_shigella=Shig_EIEC,
         ch_abund_rotavirus=Rotavirus,
         ch_abund_campylobacter=Campy.pan) %>%
  mutate(
    ch_pos_giardia_EE=1*(ch_abund_giardia_EE> 0),
    ch_pos_norovirus=1*(ch_abund_norovirus> 0),
    ch_pos_crypto_EE =1*(ch_abund_crypto_EE> 0),
    ch_pos_salmonella=1*(ch_abund_salmonella> 0),
    ch_pos_ascaris_EE=1*(ch_abund_ascaris_EE> 0),
    ch_pos_trichuris_EE=1*(ch_abund_trichuris_EE> 0),
    ch_pos_entamoeba_EE=1*(ch_abund_entamoeba_EE> 0),
    ch_pos_cholera=1*(ch_abund_cholera> 0),
    ch_pos_cdiff=1*(ch_abund_cdiff> 0),
    ch_pos_shigella=1*(ch_abund_shigella> 0),
    ch_pos_rotavirus= 1*(ch_abund_rotavirus> 0),
    ch_pos_campylobacter=1*(ch_abund_campylobacter> 0)
  ) %>%
  subset(., select = -c(ETEC_LT,EAEC,ETEC_ST,ETEC.any,tEPEC, aEPEC, EPEC.any,STEC,Ancyclostoma,Necator,E.bieneusi,
                        Blastocystis,Plesiomonas,Aeromonas,PhHV,MS2,Schistosoma,H.nana,`Adenovirus 40/41`, `Adenovirus pan`,
                        Cyclospora, Isospora, H.pylori, Sapovirus, `Cryptosporidium hominis`, `Cryptosporidium parvum`,
                        E.intestinalis,`pan Entamoeba`, Astrovirus, Strongyloides, M.tuberculosis, `Norovirus GI`, `Norovirus GII`,
                        B.fragilis, `Campylobacter jejuni/coli`))
colnames(EE_TAC)


#get midline stool dates from ee dataset and merge in
EEdates <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-stool.csv")
EEdates <- EEdates %>% subset(., select = c(childid, date2)) %>% 
  rename(child_date_pathogen=date2) %>% mutate(child_date_pathogen=dmy(child_date_pathogen)) %>%
  filter(!is.na(child_date_pathogen))
summary(EEdates$child_date_pathogen)

#merge:
dim(EE_TAC)
dim(EEdates)
EE_TAC<-left_join(EE_TAC, EEdates, by = c("childid")) %>% filter(!is.na(child_date_pathogen))
dim(EE_TAC)
head(EE_TAC)




#Merge WBB based on sampling round

#1) Fuhrmeister to health outcomes

# Specifically, Erica's samples were collected during Round 3 and Round 4 of the R01. 
#Therefore, if you merge them with the R01 health data with a one-round offset (i.e., match the R3 environmental data to the R4 
#health data, the R4 environmental data to the R5 health data), 
#then each environmental datapoint should have a subsequent diarrhea datapoint within the next 3ish months.
env_fuhr <- env_wbb %>% filter(study=="Fuhrmeister 2020") %>% mutate(merge_round=as.numeric(round))
diar_fuhr <- wbb %>% filter(round %in% c(4,5),  !is.na(diar7d)) %>% 
  mutate(merge_round=as.numeric(round)-1, 
         study="Fuhrmeister 2020", 
         diar7d_full=diar7d) %>%
  select(study, block, clusterid, dataid, hhid, 
         merge_round, child_date, agedays, sex,             
         childid, diar7d, diar7d_full, momage, hfiacat)

prop.table(table(diar_fuhr$diar7d))*100


#Get endline anthropometry
# Erica's R3 and R4 sampling definitely preceded the main trial endline anthro measurements so there should not be missings there either.
anthro_fuhr <- wbb %>% filter(round == "endline", !is.na(haz)|!is.na(waz)|!is.na(whz)) %>% 
  mutate(study="Fuhrmeister 2020") %>%
  select(study,block, clusterid, dataid, hhid, child_date_anthro, agedays, sex,             
         childid, haz, waz, whz, momage, hfiacat) %>%
  rename(agedays_anthro=agedays)
anthro_fuhr %>% filter(dataid==10306)

#Tabulate numbers before merge
fuhr_res <- data.frame(
  study = "fuhrmeister",
  env_samples_before_merge = nrow(env_fuhr %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env_fuhr %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(diar_fuhr %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, hhid, merge_round, child_date, agedays,    sex, childid, diar7d, merge_round)),
  haz_samples_before_merge = nrow(anthro_fuhr %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, agedays_anthro,    sex, childid, haz))
)
table(env_fuhr$round)
table(diar_fuhr$round)
table(diar_fuhr$merge_round)
table(is.na(diar_fuhr$diar7d))
table(env_fuhr$sample, env_fuhr$round)



#Merge CH samples together
dim(anthro_fuhr)
anthro_fuhr <- bind_rows(anthro_fuhr %>% mutate(merge_round=3),
                          anthro_fuhr %>% mutate(merge_round=4))
dim(anthro_fuhr)
dim(diar_fuhr)
ch_fuhr <- full_join(diar_fuhr, anthro_fuhr, by = c("study","dataid","clusterid", "childid", "hhid","block","merge_round","sex","momage","hfiacat")) %>% filter(!(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) %>%
  distinct(study, dataid, clusterid, childid, merge_round, .keep_all = T)
dim(ch_fuhr)
colnames(ch_fuhr)
table(1*!is.na(ch_fuhr$diar7d), !is.na(ch_fuhr$haz))




dim(env_fuhr)
dim(ch_fuhr)
#ch_env_fuhr <- full_join(env_fuhr, ch_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round","sex", "childid","block")) %>%  filter(!is.na(pos) & !(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) 
env_fuhr <- env_fuhr %>% subset(., select = -c(hhwealth))
ch_env_fuhr <- full_join(env_fuhr, ch_fuhr, by = c("study","dataid","clusterid", "hhid","merge_round")) %>%  filter(!is.na(pos) & !(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz))) %>% distinct(.)
#diar_env_fuhr <- left_join(env_fuhr, diar_fuhr, by = c("dataid","clusterid", "hhid","merge_round"))
dim(ch_env_fuhr)
colnames(ch_env_fuhr)

head(ch_env_fuhr)

fuhr_res$env_samples_after_merge <- nrow(ch_env_fuhr %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
fuhr_res$env_HH_after_merge <- nrow(ch_env_fuhr %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
fuhr_res$diar_samples_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, merge_round, child_date, agedays,    sex, childid, diar7d, merge_round))
fuhr_res$haz_samples_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, merge_round, child_date, agedays,    sex, childid, haz, merge_round))
fuhr_res$samples_with_diar_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
fuhr_res$samples_with_haz_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
fuhr_res$samples_with_ch_after_merge <- nrow(ch_env_fuhr %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))




#2) Boehm to health outcomes
# The Boehm sampling was done as part of the World Bank study which collected its own diarrhea data one week after the environmental samples 
# (and Amy shared that dataset with you) so those should be perfectly matched.


env_boehm <- env_wbb %>% filter(study=="Boehm 2016") 
table(env_boehm$pos[env_boehm$target=="Any pathogen" & env_boehm$sample=="W"])
table(env_boehm$pos[env_boehm$target=="Any pathogen" & env_boehm$sample=="SW"])

table(env_boehm$pos[env_boehm$target=="Rotavirus"& env_boehm$sample=="W"])

#world bank diarrhea
labs <- haven::read_dta(paste0(dropboxDir,"Data/WBB/fecal_pathways_1_childhealth_micro_submit.dta"))
labs <- makeVlist(labs)

diar_boehm_full <- haven::read_dta(paste0(dropboxDir,"Data/WBB/world bank/child_health_micro_cum_d1_d2_long_30oct2016.dta")) %>% mutate(round="World Bank")
head(diar_boehm_full)
colnames(diar_boehm_full)

table(is.na(diar_boehm_full$d1diar7dprev)) #day 1 diarrhea
table(is.na(diar_boehm_full$loose7dprev)) #diarrhea measure to use
table(diar_boehm_full$d1diar7dprev) #day 1 diarrhea
table(diar_boehm_full$loose7dprev) #diarrhea measure to use
prop.table(table(diar_boehm_full$d1diar7dprev)) #day 1 diarrhea
prop.table(table(diar_boehm_full$loose7dprev)) #diarrhea measure to use
nrow(diar_boehm_full %>% distinct(pid))
nrow(diar_boehm_full %>% distinct(pid, cid))

diar_boehm <- diar_boehm_full %>% rename(diar7d=loose7dprev,
                                    dataid=pid,
                                    childid=cid,
                                    agedays=age) %>%
  filter(!is.na(diar7d), !is.na(agedays), 
         arm %in% c("Control", "Sanitation")) %>%
  select(clusterid, dataid, 
         #hhid,  child_date_anthro, sex,    
         agedays,          
         childid, intday, intmo, diar7d,
         round) %>%
  mutate(childid=gsub("_","",childid),
         childid=gsub("tt","t2",childid),
         childid=gsub("t","t1",childid),
         childid=toupper(childid),
         clusterid=as.numeric(clusterid),
         agedays=agedays*30.4167,
         study="Boehm 2016",
         diar7d_full=diar7d) %>%
  distinct(clusterid, dataid, agedays,childid,intday, intmo,diar7d, diar7d_full, round, study)

dim(diar_boehm)
table(diar_boehm$childid)
table(diar_boehm$diar7d)
prop.table(table(diar_boehm$diar7d))
#From https://pubs.acs.org/doi/pdf/10.1021/acs.est.8b00928
# On the second household visit, 2200 (90%) of these children
# were successfully measured and included in the prospective
# analysis. WHO-defined diarrhea 7-day prevalence was 17.8% at
# the first household visit and 16.9% at the second visit;



# Merge with endline anthropometry
# The timing of the Boehm sampling was on average 4 months after the initiation of interventions so it preceded the main trial
# midline anthro measurements and should be perfectly matched to those as well.
anthro_boehm <- wbb %>% filter(round == "midline", !is.na(haz)|!is.na(waz)|!is.na(whz)) %>% 
  select(block, clusterid, dataid, hhid, child_date_anthro, agedays, sex,             
         childid, haz, waz, whz, momage, hfiacat) %>%
  rename(agedays_anthro=agedays) %>%
  mutate(study="Boehm 2016", round="World Bank")

summary(anthro_boehm$child_date_anthro)
summary(env_boehm$env_date)

#Check diarrhea merging
ch_boehm <- left_join(diar_boehm, anthro_boehm, by = c("study","dataid","clusterid", "childid", "round")) %>% filter(!(is.na(diar7d) & is.na(haz) & is.na(waz) & is.na(whz)))
dim(ch_boehm)
colnames(ch_boehm)

#env <- env_boehm %>% filter(target=="Any pathogen", sample=="any sample type")
env <- env_boehm %>% distinct(study,dataid,clusterid, round)
dim(diar_boehm)
dim(env)
diar_merge_boehm <- left_join(env, diar_boehm, by = c("study","dataid","clusterid", "round")) 
dim(diar_merge_boehm)
colnames(diar_merge_boehm)

unique(env$dataid[!(env$dataid %in% diar_boehm$dataid)])
unique(env$dataid[!(env$dataid %in% diar_boehm_full$pid)])


table(is.na(diar_merge_boehm$diar7d))
table(diar_merge_boehm$diar7d)
prop.table(table((diar_merge_boehm$diar7d)))*100
prop.table(table(is.na(diar_merge_boehm$diar7d)))*100

table((diar_merge_boehm$diar7d))
prop.table(table((diar_merge_boehm$diar7d)))*100

diar_merge_boehm$dataid[is.na(diar_merge_boehm$diar7d)]
diar_merge_boehm[is.na(diar_merge_boehm$diar7d),]

#dataid and clusterid
env_failed <- anti_join(env, diar_boehm, by = c("study","dataid","clusterid", "round"))
diar_failed <- anti_join(diar_boehm, env, by = c("study","dataid","clusterid", "round"))
env_failed <- env_failed %>% distinct(dataid, clusterid)
diar_failed <- diar_failed %>% distinct(dataid, clusterid)
dim(env_failed)
dim(diar_failed)
unique(env_failed$dataid)

#Just dataid
env_failed <- anti_join(env, diar_boehm, by = c("dataid"))
diar_failed <- anti_join(diar_boehm, env, by = c("dataid"))
env_failed <- env_failed %>% distinct(dataid, clusterid)
diar_failed <- diar_failed %>% distinct(dataid, clusterid)
dim(env_failed)
dim(diar_failed)
unique(env_failed$dataid)


table(unique(env_failed$dataid) %in% unique(diar_boehm_full$pid))
table(unique(env_failed$dataid) %in% unique(diar_boehm$dataid))
miss_id <- unique(env_failed$dataid)[unique(env_failed$dataid) %in% unique(diar_boehm_full$pid)]
temp<-diar_boehm_full[diar_boehm_full$pid %in% miss_id,]
#write.csv(temp, file="C:/Users/andre/Downloads/temp.csv")
table(temp$diar7dprev)
table(temp$d1diar7dprev)


boehm_res <- data.frame(
  study = "boehm",
  env_samples_before_merge = nrow(env_boehm %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env_boehm %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid)),
  diar_samples_before_merge = nrow(diar_boehm %>% filter(!is.na(diar7d)) %>% ungroup() %>% distinct(dataid, round, intday, intmo, agedays, childid, diar7d)),
  haz_samples_before_merge = nrow(anthro_boehm %>% filter(!is.na(haz)) %>% ungroup() %>% distinct(dataid, hhid, agedays_anthro,    sex, childid, haz))
)


dim(env_boehm)
dim(diar_boehm)

ch_env_boehm <- full_join(env_boehm, ch_boehm, by = c("study","dataid","clusterid", "hhid","round")) %>% filter(!is.na(pos)) %>% arrange(sampleid, dataid,clusterid, hhid,childid)
table(is.na(ch_env_boehm$haz))
dim(ch_env_boehm)
colnames(ch_env_boehm)

#Get child date of diarrhea from interview month/day and env year
table(ch_env_boehm$env_date)
table(ch_env_boehm$intmo)


temp <- ch_env_boehm %>% filter(!is.na(haz), sample=="W", target=="Rotavirus")
temp <- ch_env_boehm %>% filter(!is.na(haz), sample=="W", target=="Rotavirus")
summary(temp$child_date_anthro)
summary(temp$env_date)

summary(temp$abund)
table(temp$pos, temp$abund)

summary(as.numeric(temp$child_date_anthro-temp$env_date))
table(temp$child_date_anthro-temp$env_date)
temp %>% group_by(pos) %>% summarize(N=n(), mean(haz,na.rm=T))

head(ch_env_boehm)

ch_env_boehm$child_date_anthro


ch_env_boehm$child_date <- dmy(paste0(ch_env_boehm$intday,"-",ch_env_boehm$intmo,"-",year(ch_env_boehm$env_date)))
table(ch_env_boehm$diar7d, is.na(ch_env_boehm$child_date))
table(is.na(ch_env_boehm$diar7d), is.na(ch_env_boehm$child_date))


boehm_res$env_samples_after_merge <- nrow(ch_env_boehm %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
boehm_res$env_HH_after_merge <- nrow(ch_env_boehm %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
boehm_res$diar_samples_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, childid, diar7d))
boehm_res$haz_samples_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, childid, haz))
boehm_res$samples_with_diar_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
boehm_res$samples_with_haz_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
boehm_res$samples_with_ch_after_merge <- nrow(ch_env_boehm %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))

boehm_res$env_samples_before_merge
boehm_res$samples_with_diar_after_merge
boehm_res$samples_with_diar_after_merge/boehm_res$env_samples_before_merge  * 100



#---------------------------------------------------------------------------------------
#3) Kwong to health outcomes- collected at endline or R01/EE if closer
#---------------------------------------------------------------------------------------

env_kwong <- env_wbb %>% filter(study=="Kwong 2021")
head(env_kwong)

#Tabulate N's before merge
kwong_res <- data.frame(
  study = "kwong",
  env_samples_before_merge = nrow(env_kwong %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round)),
  env_HH_before_merge = nrow(env_kwong %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
)


diar_r01_kwong <- wbb %>% filter(round %in% c(1:8),  !is.na(diar7d))
summary(diar_r01_kwong$child_date)
diar_r01_kwong <- diar_r01_kwong %>% subset(., select=c(childid, dataid, agedays, sex, hfiacat , diar7d, child_date))


dim(env_kwong)
diar_env_kwong <- left_join(env_kwong, diar_r01_kwong, by=c("dataid"))
dim(diar_env_kwong)
head(diar_env_kwong)

diar_env_kwong %>% filter(!is.na(diar7d), !is.na(pos)) %>%
  distinct( dataid, hhid,childid ) %>%
  summarize(N=n())

#cp_df <- cp_df %>% filter(child_date +14 >=env_date) %>% mutate( date_diff=child_date-env_date, date_diff2=ifelse(date_diff<0,date_diff+999999,date_diff))   %>% arrange(date_diff)
diar_env_kwong <- diar_env_kwong %>%   mutate( date_diff=child_date-env_date)   %>%  filter(date_diff  >=0 & date_diff <= 124) %>% arrange(sampleid,sample,target,childid,date_diff)

diar_env_kwong %>% group_by(sampleid,sample,target,childid) %>% summarise(N=n()) %>% ungroup() %>% summarise(mean(N))
diar_env_kwong<-diar_env_kwong %>% group_by(sampleid,sample,target,childid)  %>% arrange(date_diff) %>% slice(1) %>% mutate(N=n())
diar_env_kwong %>% group_by(sampleid,sample,target,childid) %>% summarise(N=n()) %>% ungroup() %>% summarise(mean(N))
dim(diar_env_kwong)




#get endline WBB health data
WBB_main_health <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS"))
WBB_main_health <- WBB_main_health %>% filter(round=="endline")
head(WBB_main_health)


WBB_main_anthro <- WBB_main_health %>% filter(!is.na(laz)|!is.na(whz)|!is.na(waz)) %>%
  subset(., select=c(block, clusterid, dataid, hhid, childid , child_date_anthro, agedays,sex, laz, whz, waz)) %>%
  rename(haz_main=laz, whz_main=whz, waz_main=waz, agedays_anthro=agedays)


EE <- read.csv("C:/Users/andre/Downloads/BD-EE-anthro.csv") %>% filter(date3!="") %>%
  rename(haz=laz3,  waz=waz3,  whz=whz3, child_date_anthro=date3,age_anthro= aged3) %>% mutate(child_date_anthro=as.Date(child_date_anthro, origin=csv_origin)) %>% filter(!is.na(haz)|!is.na(waz)|!is.na(whz)) %>%
  subset(., select = c(dataid, childNo,haz, waz, whz, child_date_anthro, age_anthro))
summary(EE$child_date_anthro)
summary(WBB_main_anthro$child_date_anthro)
summary(env_kwong$env_date)

unique(env_kwong$dataid)
unique(EE$dataid)

summary(EE$haz)
table(is.na(EE$haz))
dim(EE)
WBB_main_anthro <- WBB_main_anthro %>% rename(child_date_anthro_main=child_date_anthro)
anthro_env_kwong <- left_join(env_kwong, WBB_main_anthro, by=c("dataid",  "hhid", "clusterid")) %>% mutate(diff_main=as.numeric(child_date_anthro_main-env_date))
table(anthro_env_kwong$diff_main)
anthro_env_kwong <- left_join(anthro_env_kwong, EE, by=c("dataid")) %>% mutate(diff_EE=as.numeric(child_date_anthro-env_date))
dim(anthro_env_kwong)
table(anthro_env_kwong$diff_EE)
table(anthro_env_kwong$diff_EE< anthro_env_kwong$diff_main)


anthro_env_kwong <- anthro_env_kwong  %>% mutate(flag = ifelse(diff_main>=diff_EE &!is.na(diff_main),TRUE,FALSE),
                                                 diff_EE=ifelse(flag,diff_main,diff_EE),
                                                 child_date_anthro=ifelse(flag,child_date_anthro_main,child_date_anthro),
                                                 agedays_anthro=ifelse(flag,agedays_anthro,age_anthro),
                                                 child_date_anthro=as.Date(child_date_anthro, origin = stata_origin ),
                                                 haz=ifelse(is.na(haz)|flag, haz_main, haz),
                                                 whz=ifelse(is.na(whz)|flag, whz_main, whz),
                                                 waz=ifelse(is.na(waz)|flag, waz_main, waz),
                                                 diff_EE=ifelse(is.na(diff_EE), diff_main, diff_EE)) %>% distinct()
 summary(anthro_env_kwong$child_date_anthro)


table(is.na(anthro_env_kwong$haz))
anthro_env_kwong$haz[anthro_env_kwong$diff_EE< 1 ] <- NA
table(is.na(anthro_env_kwong$haz))

anthro_env_kwong$whz[anthro_env_kwong$diff_EE< 1 ] <- NA
anthro_env_kwong$waz[anthro_env_kwong$diff_EE< 1 ] <- NA



dim(diar_env_kwong)
dim(anthro_env_kwong)
ch_env_kwong <- full_join(diar_env_kwong,  anthro_env_kwong, by = c("study","trial","sampleid","childid","tr","sample","target","round",
                                                                    "abund","qual","hhwealth","season","animals","aggregate_Y","dataid",  
                                                                    "month","sex","wet","hhid", "clusterid","pos","env_date"))

ch_env_kwong<-bind_rows(diar_env_kwong, anthro_env_kwong)
dim(ch_env_kwong)
colnames(ch_env_kwong)


#impute missing sexes
EE[EE$dataid=="13807",]
ch_env_kwong$sex[ch_env_kwong$dataid=="13807"] <- "male"
ch_env_kwong$sex[ch_env_kwong$dataid=="42606"] <- "male"



table(ch_env_kwong$pos, is.na(ch_env_kwong$haz))
table(ch_env_kwong$pos, is.na(ch_env_kwong$haz))
ch_env_kwong %>% group_by(pos) %>% summarise(mean(haz,na.rm=T))
ch_env_kwong<- ch_env_kwong %>% ungroup()
kwong_res$env_samples_after_merge <- nrow(ch_env_kwong %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
kwong_res$env_HH_after_merge <- nrow(ch_env_kwong %>% do(drop_agg(.)) %>% distinct(dataid,  hhid, clusterid))
kwong_res$diar_samples_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, diar7d))
kwong_res$haz_samples_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(dataid, hhid, agedays,    sex, haz))
kwong_res$samples_with_diar_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
kwong_res$samples_with_haz_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))
kwong_res$samples_with_ch_after_merge <- nrow(ch_env_kwong %>% filter(!is.na(haz)|!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid,  hhid, clusterid,sample, round))




ch_env_kwong2 <- ch_env_kwong %>% filter(!is.na(diar7d))
table(ch_env_kwong2$child_date==ch_env_kwong2$env_date)

ch_env_kwong <- ch_env_kwong %>%
  mutate(diar7d_full=diar7d,
         diar7d = ifelse(child_date<=env_date, NA, diar7d),
         diar7d = ifelse(child_date-env_date > 124, NA, diar7d),
         diar7d = ifelse(is.na(child_date)|is.na(env_date), NA, diar7d))
table(ch_env_kwong$diar7d)
table(ch_env_kwong$diar7d_full)

ch_env_kwong %>% group_by(pos) %>% 
  filter(child_date>env_date) %>%
  filter(!is.na(haz), target=="Any pathogen", sample=="any sample type") %>% summarise(N=n(), mean(haz,na.rm=T))
ch_env_kwong %>% group_by(pos) %>% filter(!is.na(diar7d), target=="Any pathogen", sample=="any sample type") %>% summarise(N=n(), mean(diar7d,na.rm=T))


kwong_res$diar_samples_date_dropped <- kwong_res$samples_with_diar_after_merge - nrow(ch_env_kwong %>% filter(!is.na(diar7d)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid, agedays,    sex, diar7d))
kwong_res$haz_samples_date_dropped <- kwong_res$samples_with_haz_after_merge - nrow(ch_env_kwong %>% filter(!is.na(haz)) %>% do(drop_agg(.)) %>% distinct(sampleid, dataid, hhid, agedays,    sex, haz))



# In other words, for Erica and Boehm, I would rely on the R01 and World Bank diarrhea datasets, respectively, rather than the main trial. 
# As for the anthro data, the main trial midline should fall after Boehm and the main trial endline should fall after Erica within the time 
# window we allow (environmental sampling anytime before anthro during child's lifetime).
# For the Erica+R01 data, the key will be to offset them by one round (match one round of env data to the following round of diarrhea data 
# to allow ~3 months in between). I suggest doing this merge separately from your combined mega merge.
# Likewise, the World Bank dataset has its own diarrhea -- just make sure you use the prospective (and not concurrent) diarrhea variable 
# when you match it to the Boehm env data for a given HH. That will automatically ensure that the env data precedes the diarrhea data by 4-10 days (as the study was designed to revisit HHs to collect diarrhea data 4-10 days after environmental sampling).

# -make table of numbers before and after dropping for timing and share

env_wbb <- bind_rows(ch_env_fuhr, ch_env_boehm, ch_env_kwong)
head(env_wbb)

df1 <- env_wbb %>% filter(study=="Kwong 2021") %>% filter(!is.na(diar7d)) %>% distinct(dataid, hhid,childid)
df2 <- env_wbb %>% filter(study=="Kwong 2021") %>% filter(!is.na(haz)) %>% distinct(dataid, hhid,childid)


#merge ch pathogens
env_wbb <- full_join(env_wbb, wbb_sth, by=c("dataid","hhid","childid","sex", "clusterid"))
dim(env_wbb) #10444    


summary(wbb_sth$child_date_pathogen)
summary(env_wbb$child_date_pathogen)

colnames(env_wbb)
table(env_wbb$qpcr.positive.Sth)


dim(wbb_sth)
dim(env_wbb)
#EE_TAC$childid <- as.character(EE_TAC$childid)
EE_TAC$dataid <- floor(EE_TAC$childid/10) 
EE_TAC$childid <- "T1"
env_wbb$dataid
env_wbb <- full_join(env_wbb, EE_TAC, by=c("dataid","childid")) %>% filter(!is.na(pos))
dim(env_wbb)  
table(is.na(env_wbb$ch_pos_campylobacter))
table((env_wbb$ch_pos_campylobacter))

table(is.na(EE_TAC$child_date_pathogen), EE_TAC$ch_pos_path_ecoli)
table(is.na(env_wbb$child_date_pathogen), env_wbb$ch_pos_path_ecoli)
table(is.na(env_wbb$child_date_pathogen), env_wbb$ch_pos_path_ecoli, env_wbb$study)


#merge in covariates
env_wbb <- env_wbb %>% subset(., select = -c(hhwealth,momage,hfiacat))
env_wbb <- left_join(env_wbb, cov, by=c("dataid", "hhid", "clusterid"))
colnames(env_wbb)

env_wbb %>% group_by(study) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_haz=sum(!is.na(haz)))

env_wbb %>% group_by(study, sample, target) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_pos_diar=sum(diar7d==1, na.rm=T), N_pos_env_diar=sum(pos==1 & diar7d==1, na.rm=T), N_haz=sum(!is.na(haz)))



saveRDS(env_wbb, file=paste0(dropboxDir,"Data/WBB_env_CH_data.rds"))

env_wbb$ch_pos_cholera

df1 <- env_wbb %>% filter(study=="Kwong 2021") %>% filter(!is.na(diar7d)) %>% distinct(dataid, hhid,childid)
df2 <- env_wbb %>% filter(study=="Kwong 2021") %>% filter(!is.na(haz)) %>% distinct(dataid, hhid,childid)
df <- env_wbb %>% filter(study=="Kwong 2021") %>% filter(!is.na(haz),!is.na(pos)) 
df %>% select(child_date_anthro,env_date)





#Save N's
WBB_Ns <- bind_rows(fuhr_res, boehm_res, kwong_res)
WBB_Ns$percent_diar_samples_dropped <- 100 - WBB_Ns$diar_samples_after_merge/WBB_Ns$diar_samples_before_merge * 100
WBB_Ns$percent_haz_samples_dropped <- 100 - WBB_Ns$haz_samples_after_merge/WBB_Ns$haz_samples_before_merge * 100

WBB_Ns$percent_env_samples_with_diar <- WBB_Ns$samples_with_diar_after_merge/WBB_Ns$env_samples_before_merge  * 100
WBB_Ns$percent_env_samples_with_haz <-  WBB_Ns$samples_with_haz_after_merge/WBB_Ns$env_samples_before_merge  * 100
WBB_Ns$percent_env_samples_with_ch <-  WBB_Ns$samples_with_ch_after_merge/WBB_Ns$env_samples_before_merge  * 100

WBB_Ns$diar_samples_date_dropped[is.na(WBB_Ns$diar_samples_date_dropped )] <- 0
WBB_Ns$haz_samples_date_dropped[is.na(WBB_Ns$haz_samples_date_dropped )] <- 0
 
WBB_Ns <- WBB_Ns %>% 
  mutate(per_diar_samples_date_dropped= diar_samples_date_dropped/samples_with_diar_after_merge *100,
         per_haz_samples_date_dropped= haz_samples_date_dropped/samples_with_haz_after_merge *100)


saveRDS(WBB_Ns, file=paste0(here(),"/results/WBB_merge_Ns.rds"))
#saveRDS(date_diff, file=paste0(here(),"/results/WBB_date_diff.rds"))

colnames(env_wbb)

table(env_wbb$diar7d_full)