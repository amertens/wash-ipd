

# importing packages and setting directories
source(here::here("0-config.R"))

#Load env. data
env <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_child_envr_data_forIPD.csv"))

#rename variables. Original study clustered on compound
env <- env %>% rename(sampleid=ï..samp_id, clusterid=comp)


table(env$status)
table(env$detect)
table(env$status, env$detect)

#Drop cultured and qPCR E.Coli
table(env$target)
env <- env %>% filter(target!="cEC" & target!="EC23S")

#mark seasons
#https://weatherspark.com/y/97168/Average-Weather-in-Maputo-Mozambique-Year-Round
#The wetter season lasts 5.5 months, from October 23 to April 9
table(env$samp_date)
#all observations in dry season
env <- env %>% mutate(
  season = "dry"
  )
 

#Subset to aim 1 variables and save dataset
env <- env %>% select(sampleid, clusterid, hh, survey, season, sample, sample_def, samp_level, 
                      target, effort, status, detect, logquant, censored, censquant)

#clean variables for merge
env <- env %>% mutate(
  survey=case_when(survey=="0m"~0,
                   survey=="12m"~1),
  dataid=clusterid
)

head(env)


#Get censoring values
# d <- env %>% filter(censored!="none") %>% 
#   mutate(grp=paste0(sample, "-", target, "-", censored)) %>%
#   select(grp,censquant) %>%
#   droplevels(.)
# table(d$sample, d$censquant, d$target)
# 
# d %>% group_by(sample, target, censored) %>% do(res=data.frame(summary(censquant))) 
# 
# library(psych)
# 
# describeBy(d, group="grp")


#----------------------------------------
# merge in env. pathogen data
#----------------------------------------


#load soil pathogen data
soil <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_soil_pathogens.csv"))
colnames(env)
colnames(soil)
head(soil)
soil <- soil %>% filter(phase=="24M") %>%
  mutate(animal_in_compound = ifelse(dog_observed=="Yes" | chicken_duck_observed=="Yes" | cat_observed=="Yes", 1, 0)) %>%
  subset(., select=c(compound, adenovirus_40_41:campylobacter_jejuni_coli, trial_arm, animal_in_compound)) %>%
  gather(adenovirus_40_41:campylobacter_jejuni_coli, key = target, value = detect ) %>%
  mutate(sample="S", dataid=compound, logquant=NA, survey=2) %>%
  rename(clusterid=compound )
#Notes: keep in the intervention variable to check merging accuracy
    

#Create pathogenic E.coli Drop non-included targets
ecoli_measures <- c("enteroaggregative_Ecoli","enteropathogenic_Ecoli","enterotoxigenic_Ecoli","shiga_toxin_producing_Ecoli")
soil_pathogenic <- soil %>% group_by(clusterid, dataid, trial_arm, animal_in_compound, sample) %>%
  summarise(detect = 1*(sum(detect==1 & target %in% ecoli_measures))>0) %>%
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

table(env$target)
env <- env %>% filter(target!="")

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




child <- child %>% select(childid, actualPhase, clusterid,
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


#Rename variables
child <- child %>% 
  rename(survey=actualPhase)

#clean variables for merge
child <- child %>% 
  mutate(hh=totHHid %% 100)


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
env2 <- left_join(env, child_tr, by = c("clusterid"))
table(is.na(env2$studyArm_binary))
dim(env2)

# #Figure out why treatment arms don't line up in soil data
# df <- left_join(soil, child_tr, by = c("clusterid"))
# table(is.na(df$studyArm_binary))
# table(df$studyArm_binary, df$trial_arm)
# 
# df<-df %>% distinct(clusterid, studyArm_binary, trial_arm)
# table(df$studyArm_binary, df$trial_arm)

#then merge full data
d <- full_join(child, env2, by = c("clusterid", "survey", "hh", "studyArm_binary"))
dim(d)

table(d$studyArm_binary)
table(is.na(d$studyArm_binary))
table(d$sample, is.na(d$studyArm_binary))
 
# d <- left_join(child, env, by = c("compID", "survey", "hh"))
# dim(d)



#rename variables to standardize
d <- d %>%
  rename(
         round=survey,
         pos=detect,
         hhwealth=povNormal,
         Nhh=Hhsize,
         nrooms=hhrooms,
         momedu=carerEDU,
         walls=hh_walls,
         floor=hhCement,
         elec=compElec,
         abund_only_detect=logquant,
         abund=censquant
         )%>%
  #mutate(tr=ifelse(floor(clusterid/1000)==2,"Sanitation","Control"),
  mutate(tr=ifelse(studyArm_binary==1,"Sanitation","Control"),
  round=case_when(round==0~"0m",
                  round==1~"12m",
                  round==2~"24m"),
  tr = factor(tr, levels = c("Control", "Sanitation"))
  )
table(d$tr, d$studyArm_ternary)
table(d$tr, d$trial_arm)

saveRDS(d, file=paste0(dropboxDir,"Data/MapSan/mapsan_cleaned.rds"))


#Split out just env data and covariates
colnames(d)
env_clean <- d %>% subset(., select = c(sampleid, clusterid, tr,
  dataid,  hh, round, sample, sample_def, samp_level, target,
  effort, pos, abund_only_detect, abund, censored,
  Nhh, hhwealth, nrooms, walls, floor, elec, season, compAnyAnimal, studyArm_binary
)) 


#Subset to hh-level observations
dim(env)
dim(d)
env_clean <- env_clean %>% 
             distinct(sampleid, dataid, clusterid, tr, sample, target, .keep_all=TRUE) %>%
             filter(!is.na(sample), sample!="") 
dim(env_clean)

table(env_clean$target, env_clean$pos, env_clean$sample)


prop.table(table(env_clean$tr, env_clean$studyArm_binary))*100

#Save environmental data
saveRDS(env_clean, file=paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds"))

