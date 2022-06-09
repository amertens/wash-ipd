

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))

#Load env. data
env <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_child_envr_data_forIPD.csv"))

head(env)
#compID is the clustering ID
table(env$hh)

#rename variables. Original study clustered on compound
env <- env %>% rename(sampleid=ï..samp_id, clusterid=comp, env_date=samp_date) %>%
  mutate(hh=ifelse(is.na(hh),52,hh),
    hhid=clusterid*100+hh)
summary(env$env_date)

#Drop cultured and qPCR E.Coli
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
env <- env %>% select(env_date, sampleid, clusterid, hhid, hh, survey, season, type, type_def, samp_level, 
                      target, effort, status, detect, logquant, censored, censquant)

#clean variables for merge
env <- env %>% mutate(
  round=case_when(survey=="0m"~"bl",
                   survey=="12m"~"ml"),
  env_date=paste0(env_date,"-15"),
  env_date=ymd(env_date),
  censquant = censquant^10 #untransform abundance estimates
) %>% 
  filter(!is.na(env_date)) %>% subset(., select = -c(survey))


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
  )%>%
  mutate(study="Holcomb 2021")



#----------------------------------------
# merge in env. pathogen data
#----------------------------------------


#load soil pathogen data
soil <- read.csv(paste0(dropboxDir,"Data/MapSan/mapsan_soil_pathogens.csv"))
#load dates
soil_dates <- read.csv(paste0(dropboxDir,"Data/MapSan/soil_samples_dates_to share.csv")) %>%
  rename(compound=Compound, phase=Phase, compound_phase=Soil.Sample, env_date=Date)
soil$phase[soil$phase=="BL"] <- "bl"

dim(soil)
soil <- left_join(soil,soil_dates, by=c("compound", "phase", "compound_phase"))
dim(soil)
table(is.na(soil$env_date))

colnames(env)
colnames(soil)
head(soil)
soil <- soil %>% 
  mutate(animals_observed_latrine = ifelse(dog_observed=="Yes" | chicken_duck_observed=="Yes" | cat_observed=="Yes", 1, 0)) %>%
  subset(., select=c(compound_phase, phase, compound, env_date, adenovirus_40_41:campylobacter_jejuni_coli, trial_arm, animals_observed_latrine)) %>%
  gather(adenovirus_40_41:campylobacter_jejuni_coli, key = target, value = detect ) %>%
  mutate(sample="LS", dataid=compound, logquant=NA, env_date=ymd(mdy(env_date))) %>%
  rename(sampleid=compound_phase, clusterid=compound, round=phase )
#Notes: keep in the intervention variable to check merging accuracy
soil$env_date[1:10]
soil$round[soil$round=="24M"] <- "el"


unique(soil$target)
#Create pathogenic E.coli Drop non-included targets and mark zoonotic/vs non-zoonotic
ecoli_measures <- c("enteroaggregative_Ecoli","enteropathogenic_Ecoli","enterotoxigenic_Ecoli","shiga_toxin_producing_Ecoli")
ecoli_measures_zoo <- c("enteropathogenic_Ecoli","shiga_toxin_producing_Ecoli")
ecoli_measures_not_zoo <- c("enteroaggregative_Ecoli","enterotoxigenic_Ecoli")
soil_pathogenic <- soil %>% group_by(sampleid, env_date, clusterid, dataid, round, trial_arm, animals_observed_latrine, sample) %>%
  summarise(detect = 1*(sum(detect==1 & target %in% ecoli_measures)>0)) %>%
  mutate(target="pathogenic_ecoli")
soil_zoo <- soil %>% group_by(sampleid, env_date, clusterid, dataid, round, trial_arm, animals_observed_latrine, sample) %>%
  summarise(detect = 1*(sum(detect==1 & target %in% ecoli_measures_zoo)>0)) %>%
  mutate(target="EC_zoo")
soil_measures_not_zoo <- soil %>% group_by(sampleid, env_date, clusterid, dataid, round, trial_arm, animals_observed_latrine, sample) %>%
  summarise(detect = 1*(sum(detect==1 & target %in% ecoli_measures_not_zoo)>0)) %>%
  mutate(target="EC_not_zoo")


table(soil$target, soil$detect)
table(soil_pathogenic$detect)

soil <- bind_rows(soil, soil_pathogenic, soil_zoo, soil_measures_not_zoo) %>% 
  filter(!(target %in% ecoli_measures))%>%
  mutate(study="Capone 2021")
table(soil$target, soil$detect)

#add soil to env dataset
env <- bind_rows(env, soil)

table(env$round)
table(is.na(env$env_date))
table(env$target)
table(env$round)
table(is.na(env$sampleid))
table(is.na(env$clusterid))

#----------------------------------------
# merge in fly pathogen data
#----------------------------------------

# For the presence/absence dataset I used a Cq cutoff of 35. For the quantitative dataset I set all non-detects to the theoretical LOD (250 gc/fly) and for all detects I used a standard curve to determine the gene copy density.
# 
# These flies are from MapSan pre-intervention (baseline) and post-intervention (referred to as midline / 12-month). Due to a limited number of compounds where we caught flies at the 12-month phase, I tried to analyze 2 flies per compound, and to differentiate samples you will see a "_2" in the first column.
# 
# I've also included the fly mass (in grams), species (house or bottle), and where we caught the fly (latrine entrance or food prep area) that can be included as potential confounders. I prioritized analyzing house flies over bottle flies, and flies caught in the food prep area over the latrine entrance. However, we didn't always catch houseflies and sometimes we only caught flies from the latrine entrance.

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
table(flypos$tr, flypos$pos, flypos$round,flypos$sample,flypos$target)


dim(flypos)
flypos <- left_join(flypos,fly_dates, by=c("compound", "round", "compound_phase"))
dim(flypos)


#Abundance
head(flyabund)
flyabund <- flyabund %>% 
  rename(sample=Location, round=Phase , sampleid=Sample_name, compound_phase=shortname, compound=Compound, tr=treated) %>% 
  subset(., select=c(sampleid,compound_phase, pan_adenovirus:cryptosporidium_parvum_or_hominis, compound, round,sample,tr)) %>%
  gather(pan_adenovirus:cryptosporidium_parvum_or_hominis, key = target, value = abund ) %>%
  filter(!(target %in% c("EAEC", "EPEC", "ETEC", "STEC"))) %>%
  mutate(censored=ifelse(abund==250,"left","none"))
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
         FlyLoc=case_when(sample=="kitchen" ~ "FlyKitch",
                          sample=="latrine" ~ "FlyLat"),
         sample="Fly")
env <- env %>% rename(tr=trial_arm, pos=detect, abund=censquant) %>%
  mutate()
head(env)
head(flypos)

table(env$tr)
table(fly$tr)

#drop targets with no positives
table(fly$target, fly$pos)
fly <- fly %>% group_by(target) %>% mutate(Npos=sum(pos)) %>% filter(Npos>0)
table(fly$target, fly$pos)

#drop mitochondrial DNA
fly <- fly %>% filter(target!="human_mtDNA") %>%
  mutate(study="Capone 2022 in prep")

#bind into main data
env <- bind_rows(env, fly)

table(env$round)
env <- env %>%
  mutate(dataid=clusterid)

#mark censored obs
table(env$censored)
table(is.na(env$censored),is.na(env$abund))
env$qual <- ifelse(env$censored=="none","ROQ","BLOQ")
env$qual <- ifelse(is.na(env$abund),NA,env$qual)
table(is.na(env$abund),is.na(env$qual))

table(env$round)
table(is.na(env$env_date))
table(env$target)
table(env$round)
table(is.na(env$sampleid))
table(is.na(env$clusterid))
table(is.na(env$env_date))
table(is.na(env$pos))



#----------------------------------------
#make health dataset
#----------------------------------------

#Load child data
child <- read.csv(paste0(dropboxDir,"Data/MapSan/triPhase_database_20200824 1412_IPD.csv"))
colnames(child)
head(child)

#rename variables
child <- child %>% rename(childid=ï..totchildID, clusterid=compID, hhid=totHHid)
  
#Replace missingness code (99999999) with NA
child <- child %>% mutate_all(~na_if(., 99999999))

#Create child sample date
child <- child %>% mutate(child_date= ymd(paste0(ch_surveydate_coarsed,"-15")), child_date_pathogen=child_date)
table(is.na(child$child_date_pathogen))
child %>% select(diarrhea, child_date, ch_surveydate_coarsed, sampDate_coarsed)

table(child$ch_careEDUorig)

child <- child %>% select(childid, actualPhase, clusterid, hhid,
                          child_date, child_date_pathogen,
                   studyArm_binary, #studyArm_ternary, 
                   age_months, #age_sampMonths,
                   carerEDU,
                   ch_careEDUorig,
                   diarrhea,
                   # Kkhas, #has KK
                   # Kkany, #any sth
                   Kkasc,
                   KKtrc,
                   KKhkw,
                   Hhsize,
                   hhCement,
                   # drophole,
                   # ventpipe,
                   # pedestal,
                   # drinkWat,
                   hhrooms,
                   # compLat,
                   # watPoint,
                   # latWall,
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
                   # hl_den,
                   # hl_terc,
                   # rainfall_survey,
                   # rainfall_sample,
                   persons_room,
                   personsPerRoomCat,
                   # persons_latrine,
                   # households_latrine,
                   # householdLatCat,
                   # persons_waterpoint,
                   CompPop,
                   sampDate_coarsed,
                   female,
                   ch_careEDUorig,
                   # ch_surveydate_coarsed,
                   # ch_exclbreast,
                   hh_walls,
                   hh_drinkwatorig,
                   povNormal,
                   numHH,
                   anthro_hfa_2,
                   anthro_wfa_2,
                   anthro_wfh_2
)


head(child)
table(child$clusterid)

#Rename variables
child <- child %>% 
  rename(survey=actualPhase,
         diar7d=diarrhea,
         haz=anthro_hfa_2,
         waz=anthro_wfa_2,
         whz=anthro_wfh_2)
         
#clean variables for merge
child <- child %>% 
  mutate(hh=hhid %% 100,
         round = case_when(
           survey==0 ~"bl",
           survey==1 ~"ml",
           survey==2 ~"el"
           ))


#Check treatment assignments
table(child$studyArm_ternary)
table(soil$trial_arm)

table(child$clusterid, child$studyArm_binary)

saveRDS(child, file=paste0(dropboxDir,"Data/MapSan/mapsan_child_cleaned.rds"))


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
table(env2$tr, env2$studyArm_binary)
table(env2$tr, env2$clusterid)
table(is.na(env2$studyArm_binary))


temp1 <- child[child$hhid==203301,]
temp2 <- env2[env2$hhid==203301,] %>% filter(!is.na(clusterid))
temp3 <- env2[env2$clusterid==2033,] %>% filter(!is.na(clusterid))
table(temp2$sample)
table(temp3$sample)

#split covariates into child and hh and compound
hh <- child %>% group_by(clusterid, hhid, round) %>%
                summarise(hh_wealth=mean(povNormal, na.rm=T)) 

#Get values averaged over rounds for missing rounds in certain compounds in the child dataset
hh2 <- child %>% group_by(clusterid, hhid) %>%
  summarise(hh_wealth2=mean(povNormal, na.rm=T)) 

clust <- child %>% group_by(clusterid) %>%
  summarise(compound_wealth=mean(povNormal, na.rm=T),
            compElec2=mean(compElec, na.rm=T),
            compAnyAnimal2=mean(compAnyAnimal, na.rm=T),
            CompPop2=mean(CompPop, na.rm=T),
            comprooms=mean(hhrooms, na.rm=T),
            compwalls=mean(hh_walls, na.rm=T),
            compfloor=mean(hhCement, na.rm=T)
            ) 


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

table(env2$study, env2$studyArm_binary)
table(child$studyArm_binary)
table(env2$study, env2$round)
table(child$round)
table(child$round, ntile(child$povNormal,4))
table(child$round, is.na(child$pov))
table(child$round, is.na(child$clusterid))


table(unique(env$clusterid[env$study=="Capone 2021"]) %in% unique(child$clusterid))
table(unique(env$hhid[env$study=="Capone 2021"]) %in% unique(child$hhid))


table(env2$sample, env2$round)
table( child$round)
table(env2$sample, env2$studyArm_binary)
table( child$studyArm_binary)
table(env2$sample, env2$clusterid)
table( child$clusterid)


dim(child)
dim(env2)
dim(child)+dim(env2)
d1 <- full_join(env2, child,  by = c("clusterid", "hhid", "round", "studyArm_binary")) %>% filter(!is.na(pos))
d2 <- full_join(d1, hh,  by = c("clusterid", "hhid", "round")) %>% filter(!is.na(pos))
d3 <- full_join(d2, hh2,  by = c("clusterid", "hhid")) %>% filter(!is.na(pos))
d <- full_join(d3, clust,  by = c("clusterid")) %>% filter(!is.na(pos))

table(d$sample, is.na(d$Hhsize))
table(d$sample, d$Hhsize)



unique(d1$clusterid)
unique(hh$clusterid)

d[d$clusterid==2033,]
df<-d[d$hhid==203301,] %>% filter(!is.na(hhid))
dim(df)
df %>% select(childid, round, age_months,sample, povNormal, haz )

table(d$sample, is.na(d$povNormal))

d$povNormal[is.na(d$povNormal)] <- d$hh_wealth[is.na(d$povNormal)] 
table(d$sample, is.na(d$povNormal))

d$povNormal[is.na(d$povNormal)] <- d$hh_wealth2[is.na(d$povNormal)] 
table(d$sample, is.na(d$povNormal))

d$povNormal[is.na(d$povNormal)] <- d$compound_wealth[is.na(d$povNormal)] 
table(d$sample, is.na(d$povNormal))

d$compElec[is.na(d$compElec)] <- d$compElec2[is.na(d$compElec)]
d$compAnyAnimal[is.na(d$compAnyAnimal)] <- d$compAnyAnimal2[is.na(d$compAnyAnimal)]
d$CompPop[is.na(d$CompPop)] <- d$CompPop2[is.na(d$CompPop)] 

#fill in compound for missing HH
d$Hhsize[is.na(d$Hhsize)] <- d$CompPop[is.na(d$Hhsize)] 
d$hhrooms[is.na(d$hhrooms)] <-  d$comprooms[is.na(d$hhrooms)] 
d$hh_walls[is.na(d$hh_walls)] <-  d$compwalls[is.na(d$hh_walls)] 
d$hhCement[is.na(d$hhCement)] <-  d$compfloor[is.na(d$hhCement)] 

d$animals_observed_latrine <- ceiling(d$animals_observed_latrine)
d$compAnyAnimal <- ceiling(d$compAnyAnimal)


table(d$sample, d$compAnyAnimal)
table(d$sample, d$animals_observed_latrine)


table(d$sample, is.na(d$Hhsize))
table(d$sample, (d$Hhsize))


#rename variables to standardize
d <- d %>%
  rename(
         hhwealth=povNormal,
         Nhh=Hhsize,
         nrooms=hhrooms,
         walls=hh_walls,
         floor=hhCement,
         elec=compElec,
         abund_only_detect=logquant
         )%>%
  #mutate(tr=ifelse(floor(clusterid/1000)==2,"Sanitation","Control"),
  mutate(
    dataid=clusterid,
    # momedu=case_when(carerEDU==0 ~"Incomplete Primary",
    #                  carerEDU==1 ~"Primary",
    #                  is.na(carerEDU) ~"Missing"),
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
table(is.na(d$env_date)) 
table(is.na(d$child_date)) 


table(d$study, d$hhwealth)
table(d$sample, is.na(d$Nhh))
table(d$sample, d$momedu)

saveRDS(d, file=paste0(dropboxDir,"Data/MapSan/mapsan_cleaned.rds"))


df <- d %>% filter(type=="ls",target=="HF183")
df$hhwealth=factor(quantcut(df$hhwealth, na.rm=T), labels=c("1","2","3","4"))



#Split out just env data and covariates
colnames(d)
env_clean <- d %>% subset(., select = c(study, env_date,sampleid, clusterid, tr,
  dataid,  hh, hhid, round, sample, samp_level, target,
  effort, pos, abund_only_detect, abund, qual, censored, momedu,
  Nhh, hhwealth, nrooms, walls, floor, elec, season, compAnyAnimal, studyArm_binary
)) 





#Subset to hh-level observations
dim(env)
dim(d)
env_clean <- env_clean %>% 
             distinct(env_date, sampleid, dataid, hhid, clusterid, tr, sample, target, .keep_all=TRUE) %>%
             filter(!is.na(sample), sample!="") 
            
dim(env_clean)
#[1] 8390   27

table(env$sample, is.na(env$hhid))
table(env_clean$target, env_clean$pos, env_clean$sample)


#Save environmental data
saveRDS(env_clean, file=paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds"))

env_clean %>% distinct(study, target) %>% group_by(study) %>%
  summarize(n())


  