

# importing packages and setting directories
source(here::here("0-config.R"))

#Load env. data
env <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_child_envr_data_forIPD.csv"))

#rename variables
env <- env %>% rename(sampleid=ï..samp_id, compID=comp)

#Subset to aim 1 variables and save dataset
env <- env %>% select(sampleid, compID, hh, survey, type, type_def, samp_level, 
                      target, effort, status, detect, logquant, logerror, censored, censquant, censerror)

#clean variables for merge
env <- env %>% mutate(
  survey=case_when(survey=="0m"~0,
                   survey=="12m"~1)
)

head(env)




#Load child data
child <- read.csv(paste0(dropboxDir,"Data/MapSan/triPhase_database_20200824 1412_IPD.csv"))
colnames(child)
head(child)

#rename variables
child <- child %>% rename(childid=ï..totchildID)
  
#Replace missingness code (99999999) with NA
child <- child %>% mutate_all(~na_if(., 99999999))




child <- child %>% select(childid, actualPhase, compID,
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





#merge datasets
dim(child)
dim(env)

#first merge treatment arm by compound ID so source water/latrine soil is merged
child_tr <- child %>% subset(., select = c("compID", "studyArm_binary"))
env <- left_join(env, child_tr, by = c("compID"))
table(is.na(env$studyArm_binary))


#then merge full data
d <- full_join(child, env, by = c("compID", "survey", "hh", "studyArm_binary"))
dim(d)

table(is.na(d$studyArm_binary))
table(d$type, is.na(d$studyArm_binary))
 
# d <- left_join(child, env, by = c("compID", "survey", "hh"))
# dim(d)



#rename variables to standardize
d <- d %>%
  rename(tr=studyArm_binary,
         round=survey,
         pos=detect,
         hhwealth=povNormal,
         Nhh=Hhsize,
         nrooms=hhrooms,
         momedu=carerEDU,
         walls=hh_walls,
         floor=hhCement,
         elec=compElec
         )


saveRDS(d, file=paste0(dropboxDir,"Data/MapSan/mapsan_cleaned.rds"))


#Split out just env data and covariates
colnames(d)
env_clean <- d %>% subset(., select = c(sampleid, compID, tr,
  sampleid, compID, hh, round, type, type_def, samp_level, target,
  effort, pos, logquant,
  numHH,
  hhwealth, nrooms, momedu, walls, floor, elec
)) %>%
  mutate(
    round="env round",
    block=1,
    watmin=NA,
    dadagri=NA,landacre=NA, hfiacat=NA,
    momage=NA, momheight=NA
  ) %>%
rename(
 Nhh=numHH,
 dataid=compID
)




#Subset to hh-level observations
dim(env)
dim(d)
env_clean <- env_clean %>% 
             distinct(.) %>%
             filter(!is.na(type), type!="") 
dim(env_clean)

#Save environmental data
saveRDS(env_clean, file=paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds"))

