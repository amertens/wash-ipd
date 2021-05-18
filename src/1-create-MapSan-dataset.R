

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))

#Load env. data
env <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_child_envr_data_forIPD.csv"))

#rename variables. Original study clustered on compound
env <- env %>% rename(sampleid=ï..samp_id, clusterid=comp, env_date=samp_date)


table(env$status)
table(env$detect)
table(env$status, env$detect)

#Drop cultured and qPCR E.Coli
table(env$target)
env <- env %>% filter(target!="cEC" & target!="EC23S")

#mark seasons
#https://weatherspark.com/y/97168/Average-Weather-in-Maputo-Mozambique-Year-Round
#The wetter season lasts 5.5 months, from October 23 to April 9
table(env$env_date)
#all observations in dry season
env <- env %>% mutate(
  season = "dry"
  )
 

#Subset to aim 1 variables and save dataset
env <- env %>% select(env_date, sampleid, clusterid, hh, survey, season, type, type_def, samp_level, 
                      target, effort, status, detect, logquant, censored, censquant)

#clean variables for merge
env <- env %>% mutate(
  survey=case_when(survey=="0m"~0,
                   survey=="12m"~1),
  env_date=paste0(env_date,"-15"),
  env_date=ymd(env_date)
) %>% filter(!is.na(env_date))
head(env)

head(env)

#harmonize coding of sample types
env <- env %>% mutate(
    sample = case_when(
      type=="S" ~ "S",
      type=="ds" ~ "S",
      type=="fp" ~ "FP",
      type=="hw" ~ "W",
      type=="ls" ~ "LS",
      type=="wp" ~ "SW"
    )      
  )



#----------------------------------------
# merge in env. pathogen data
#----------------------------------------


#load soil pathogen data
soil <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_soil_pathogens.csv"))
#load dates
soil_dates <- read.csv(paste0(dropboxDir,"Data/MapSan/soil_samples_dates_to share.csv")) %>%
  rename(compound=Compound, phase=Phase, compound_phase=Soil.Sample, env_date=Date)
soil_dates$phase[soil_dates$phase=="bl"] <- "BL"

dim(soil)
soil <- left_join(soil,soil_dates, by=c("compound", "phase", "compound_phase"))
dim(soil)
table(is.na(soil$env_date))

colnames(env)
colnames(soil)
head(soil)
soil <- soil %>% 
  mutate(animal_in_compound = ifelse(dog_observed=="Yes" | chicken_duck_observed=="Yes" | cat_observed=="Yes", 1, 0)) %>%
  subset(., select=c(compound_phase, phase, compound, env_date, adenovirus_40_41:campylobacter_jejuni_coli, trial_arm, animal_in_compound)) %>%
  gather(adenovirus_40_41:campylobacter_jejuni_coli, key = target, value = detect ) %>%
  mutate(sample="S", dataid=compound, logquant=NA, survey=2, env_date=ymd(mdy(env_date))) %>%
  rename(sampleid=compound_phase, clusterid=compound, round=phase )
#Notes: keep in the intervention variable to check merging accuracy
soil$env_date[1:10]


unique(soil$target)
#Create pathogenic E.coli Drop non-included targets
ecoli_measures <- c("enteroaggregative_Ecoli","enteropathogenic_Ecoli","enterotoxigenic_Ecoli","shiga_toxin_producing_Ecoli")
soil_pathogenic <- soil %>% group_by(env_date, clusterid, dataid, round, trial_arm, animal_in_compound, sample) %>%
  summarise(detect = 1*(sum(detect==1 & target %in% ecoli_measures)>0)) %>%
  mutate(target="pathogenic_ecoli")


table(soil$target, soil$detect)
table(soil_pathogenic$detect)

soil <- bind_rows(soil, soil_pathogenic) %>% 
  filter(!(target %in% ecoli_measures))
table(soil$target, soil$detect)

head(env)
head(soil)
unique(soil$target)

env <- bind_rows(env, soil)
table(is.na(env$env_date))

env[is.na(env$env_date),]

table(env$target)
env <- env %>% filter(target!="")
table(env$round)

env$round[env$round=="24M"] <- "el"

#make numeric sample ID
#env <- env %>% mutate(sampleid=as.numeric(factor(sampleid)))


#----------------------------------------
# merge in fly pathogen data
#----------------------------------------

flypos <- read.csv(paste0(dropboxDir,"Data/MapSan/MapSan_flies_presence_absence_for AM.csv"))
flyabund <- read.csv(paste0(dropboxDir,"Data/MapSan/MapSan_flies_quantitative_for AM.csv"))
fly_dates <- read.csv(paste0(dropboxDir,"Data/MapSan/fly_samples_dates_to share.csv")) %>%
  rename(compound=Compound, round=Phase,  compound_phase=Fly.Sample, env_date=Date) %>% distinct() %>%
  mutate(env_date=mdy(env_date))
fly_dates$round[fly_dates$round=="12M"] <- "ml"

head(flypos)
flypos <- flypos %>% 
  rename(sample=trap_location, round=Phase , sampleid=Sample_name, compound_phase=shortname, compound=Compound, tr=treated) %>% 
  subset(., select=c(sampleid,compound_phase, pan_adenovirus:cryptosporidium_parvum_or_hominis, compound, round,sample,tr)) %>%
  gather(pan_adenovirus:cryptosporidium_parvum_or_hominis, key = target, value = pos )
table(flypos$pos)
table(flypos$target, flypos$pos)
table(flypos$target, flypos$pos, flypos$sample)


#Create pathogenic E.coli Drop non-included targets
ecoli_measures <- c("EAEC", "EPEC", "ETEC", "STEC")
flypos_pathogenic <- flypos %>% group_by(sampleid, compound, compound_phase, round,  sample, tr) %>%
  summarise(pos = 1*(sum(pos==1 & target %in% ecoli_measures)>0)) %>%
  mutate(target="pathogenic_ecoli")
table(flypos_pathogenic$pos)
head(flypos_pathogenic)

flypos <- bind_rows(flypos, flypos_pathogenic) %>% 
  filter(!(target %in% ecoli_measures))
table(flypos$target, flypos$pos)


dim(flypos)
flypos <- left_join(flypos,fly_dates, by=c("compound", "round", "compound_phase"))
dim(flypos)


#Abundance
head(flyabund)
flyabund <- flyabund %>% 
  rename(sample=Location, round=Phase , sampleid=Sample_name, compound_phase=shortname, compound=Compound, tr=treated) %>% 
  subset(., select=c(sampleid,compound_phase, pan_adenovirus:cryptosporidium_parvum_or_hominis, compound, round,sample,tr)) %>%
  gather(pan_adenovirus:cryptosporidium_parvum_or_hominis, key = target, value = abund ) %>%
  filter(!(target %in% c("EAEC", "EPEC", "ETEC", "STEC")))
summary(flyabund$abund)

colnames(flypos)
colnames(flyabund)


dim(flypos)
dim(flyabund)
fly <- left_join(flypos,flyabund, by=c("sampleid", "compound_phase", "compound", "round", "sample", "tr", "target"))
dim(fly)
table(is.na(fly$abund))
table(fly$target, is.na(fly$abund))

#add fly data to env dataset

colnames(env)
colnames(fly)
fly <- fly %>% subset(., select= -c(compound_phase)) %>%
  rename(clusterid=compound) %>%
  mutate(tr=case_when(tr==1~"I",tr==0 ~ "C"),
         sample=case_when(sample=="kitchen" ~ "FlyKitch",
                          sample=="latrine" ~ "FlyLat"))
env <- env %>% rename(tr=trial_arm, pos=detect, abund=censquant) 
head(env)
head(flypos)

table(env$tr)
table(fly$tr)

#drop targets with no positives
table(fly$target, fly$pos)
fly <- fly %>% group_by(target) %>% mutate(Npos=sum(pos)) %>% filter(Npos>0)
table(fly$target, fly$pos)

#drop mitochondrial DNA
fly <- fly %>% filter(target!="human_mtDNA")

#bind into main data
env <- bind_rows(env, fly)

table(env$round)
env$round[env$round=="BL"] <- "bl"
env$round[env$round=="EL"] <- "el"

env <- env %>%
  mutate(dataid=clusterid)
table(is.na(env$dataid))


#----------------------------------------
#make health dataset
#----------------------------------------

#Load child data
child <- read.csv(paste0(dropboxDir,"Data/MapSan/triPhase_database_20200824 1412_IPD.csv"))
colnames(child)
head(child)

#rename variables
child <- child %>% rename(childid=ï..totchildID, clusterid=compID)
  
#Replace missingness code (99999999) with NA
child <- child %>% mutate_all(~na_if(., 99999999))

#Create child sample date
child <- child %>% mutate(child_date= ym(ch_surveydate_coarsed))


child <- child %>% select(childid, actualPhase, clusterid, child_date,
                   studyArm_binary, studyArm_ternary, age_months, age_sampMonths,
                   carerEDU, breast,
                   diarrhea,
                   Kkhas,
                   Kkany,
                   Kkasc,
                   KKtrc,
                   KKhkw,
                   Hhsize,
                   hhCement,
                   drophole,
                   ventpipe,
                   pedestal,
                   drinkWat,
                   hhrooms,
                   compLat,
                   watPoint,
                   latWall,
                   compElec,
                   compAnyAnimal,
                   gpp_aden, gpp_noro, gpp_rota, gpp_cdif, 
                   gpp_camp, gpp_o157, gpp_etec, gpp_salm, 
                   gpp_stec, gpp_shig, gpp_chol, gpp_yers,
                   gpp_cryp, gpp_enta, gpp_giar,
                   gpp_prim,
                   gpp_prim_noSal,
                   gpp_numInf_noSal,
                   gpp_coInf_noSal,
                   gpp_vir,
                   gpp_bact,
                   gpp_par,
                   gpp_Num_inf,
                   gpp_anyInf,
                   hl_den,
                   hl_terc,
                   rainfall_survey,
                   rainfall_sample,
                   persons_room,
                   personsPerRoomCat,
                   persons_latrine,
                   households_latrine,
                   householdLatCat,
                   persons_waterpoint,
                   CompPop,
                   sampDate_coarsed,
                   female,
                   totHHid,
                   ch_careEDUorig,
                   ch_surveydate_coarsed,
                   ch_exclbreast,
                   hh_walls,
                   hh_drinkwatorig,
                   povNormal,
                   numHH,
                   anthro_hfa_2,
                   anthro_wfa_2,
                   anthro_wfh_2
)

head(child)

#Rename variables
child <- child %>% 
  rename(survey=actualPhase,
         diar7d=diarrhea,
         haz=anthro_hfa_2,
         waz=anthro_wfa_2,
         whz=anthro_wfh_2)
         
#clean variables for merge
child <- child %>% 
  mutate(hh=totHHid %% 100,
         round = case_when(
           survey==0 ~"bl",
           survey==1 ~"ml",
           survey==2 ~"el"
           ))


#Check treatment assignments
table(child$studyArm_ternary)
table(soil$trial_arm)

table(child$clusterid, child$studyArm_binary)


#merge datasets
dim(child)
dim(env)

#Update: get treatment from clusterid
#first merge treatment arm by compound ID so source water/latrine soil is merged
child_tr <- child %>% subset(., select = c("clusterid", "studyArm_binary")) %>% distinct(.)
child[child$clusterid==2082,]
soil[soil$clusterid==2082, ]
dim(child_tr)
dim(env)
env2 <- left_join(env, child_tr, by = c("clusterid"))
table(is.na(env2$studyArm_binary))
dim(env2)


#then merge full data
table(child$clusterid)
table(env2$clusterid)

table(child$studyArm_binary)
table(env2$studyArm_binary)
table(is.na(env2$studyArm_binary))
table(env2$tr, env2$studyArm_binary)

child$clusterid[1:10]
env2$clusterid[1:10]
child$hh[1:10]
env2$hh[1:10]
env2 <- env2 %>% subset(., select = -c(hh, tr))


dim(child)+dim(env2)
d <- full_join(child, env2, by = c("clusterid", "round", "studyArm_binary")) %>% filter(!is.na(pos))
dim(d)

colnames(d)
table(d$round)

#rename variables to standardize
d <- d %>%
  rename(
         hhwealth=povNormal,
         Nhh=Hhsize,
         nrooms=hhrooms,
         momedu=carerEDU,
         walls=hh_walls,
         floor=hhCement,
         elec=compElec,
         abund_only_detect=logquant
         )%>%
  #mutate(tr=ifelse(floor(clusterid/1000)==2,"Sanitation","Control"),
  mutate(
    dataid=clusterid,
    tr=ifelse(studyArm_binary==1,"Sanitation","Control"),
    tr = factor(tr, levels = c("Control", "Sanitation")),
  abund = abund^10) #untransform abundance estimates

table(is.na(d$env_date)) 
table(is.na(d$child_date)) 


saveRDS(d, file=paste0(dropboxDir,"Data/MapSan/mapsan_cleaned.rds"))


#Split out just env data and covariates
colnames(d)
env_clean <- d %>% subset(., select = c(env_date,sampleid, clusterid, tr,
  dataid,  hh, round, sample, samp_level, target,
  effort, pos, abund_only_detect, abund, censored,
  Nhh, hhwealth, nrooms, walls, floor, elec, season, compAnyAnimal, studyArm_binary
)) 


#Subset to hh-level observations
dim(env)
dim(d)
env_clean <- env_clean %>% 
             distinct(env_date, sampleid, dataid, clusterid, tr, sample, target, .keep_all=TRUE) %>%
             filter(!is.na(sample), sample!="") 
            
dim(env_clean)

table(env_clean$target, env_clean$pos, env_clean$sample)


#Save environmental data
saveRDS(env_clean, file=paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds"))

table(is.na(env_clean$env_date)) 

df <- env_clean %>% group_by(sampleid, sample) %>% summarise(N=length(unique(tr))) 
  table(df$N)