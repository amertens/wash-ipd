

# importing packages and setting directories
source(here::here("0-config.R"))

#Load env. data
env <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_child_envr_data_forIPD.csv"))

#rename variables. Original study clustered on compound
env <- env %>% rename(sampleid=ï..samp_id, clusterid=comp)


table(env$status)
table(env$detect)
table(env$status, env$detect)

#Drop cultured E.Coli
table(env$target)
env <- env %>% filter(target!="cEC")

#Subset to aim 1 variables and save dataset
env <- env %>% select(sampleid, clusterid, hh, survey, type, type_def, samp_level, 
                      target, effort, status, detect, logquant, censored, censquant)

#clean variables for merge
env <- env %>% mutate(
  survey=case_when(survey=="0m"~0,
                   survey=="12m"~1),
  dataid=clusterid
)

head(env)







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





#merge datasets
dim(child)
dim(env)

#first merge treatment arm by compound ID so source water/latrine soil is merged
child_tr <- child %>% subset(., select = c("clusterid", "studyArm_binary")) %>% distinct(.)

dim(child_tr)
env2 <- left_join(env, child_tr, by = c("clusterid"))
table(is.na(env2$studyArm_binary))
dim(env2)

#then merge full data
d <- full_join(child, env2, by = c("clusterid", "survey", "hh", "studyArm_binary"))
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
         elec=compElec,
         abund_only_detect=logquant,
         abund=censquant
         )


saveRDS(d, file=paste0(dropboxDir,"Data/MapSan/mapsan_cleaned.rds"))


#Split out just env data and covariates
colnames(d)
env_clean <- d %>% subset(., select = c(sampleid, clusterid, tr,
  dataid,  hh, round, type, type_def, samp_level, target,
  effort, pos, abund_only_detect, abund, censored,
  Nhh, hhwealth, nrooms, walls, floor, elec
)) 


env_clean <- env_clean %>% subset(., select = c(sampleid, dataid, clusterid, tr, type, target, pos, abund_only_detect, censored, abund, round, Nhh, 
                                                #momage, momheight, momedu, 
                                                #dadagri, landacre, hfiacat,watmin,  
                                                floor, hhwealth)) %>%
  mutate(tr=case_when(
    tr=="0" ~ "Control",
    tr=="1" ~ "Sanitation"
  ),
  round=case_when(round==0~"0m",
                   round==1~"12m"),
  tr = factor(tr, levels = c("Control", "Sanitation"))
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

