

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



#Load child data
child <- read.csv(paste0(dropboxDir,"Data/MapSan/triPhase_database_20200824 1412_IPD.csv"))
colnames(child)
head(child)

#rename variables
child <- child %>% rename(childid=ï..totchildID)
  
#Replace missingness code (99999999) with NA
child <- child %>% mutate_all(~na_if(., 99999999))

#Subset to aim 1 variables and save dataset
#Adjustment covariates:
# 1.	Child birth order/parity 
# 2.	Asset-based wealth index 
# 3.	Number of individuals and children in household
# 4.	Household food security 
# 5.	Household electrification and construction, including wall/roof material 
# 6.	Parental age 
# 7.	Parental education 
# 8.	Parental employment 
# a.	Indicator for works in agriculture 
# 9.	Land ownership 


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

d <- full_join(child, env, by = c("compID", "survey", "hh"))

dim(d)



#rename variables to standardize
d <- d %>%
  rename(tr=studyArm_binary)


saveRDS(d, file=paste0(dropboxDir,"Data/MapSan/mapsan_cleaned.rds"))



