

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))



#----------------------------------------------------------------------------
#Make env dataset
#----------------------------------------------------------------------------

#Load covariates and treatment arms
enrol <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-enrol.csv"))
tr <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-real-tr.csv"))

#Load env MST/pathogen datasets
soilSTH <- read_dta(paste0(dropboxDir,"Data/WBB/WASHB-soil-sth.dta")) 

qPCR <- read.csv(paste0(dropboxDir,"Data/WBB/Erica Fuhrmeister - washb_qPCR_10_23_18_adjusted_var_day.csv")) 
PEC <- read.csv(paste0(dropboxDir,"Data/WBB/Erica Fuhrmeister - washb_PEC_10_23_18_adjusted_var_day.csv")) 

temp <- PEC %>% distinct(Unique.Numerical.ID)
dim(temp)

#Add date to PEC and QPCR
PEC <- PEC %>% mutate(env_date=dmy(paste0(intday,"-",Month.Collected,"-",2015)))
qPCR <- qPCR %>% mutate(env_date=dmy(paste0(intday,"-",Month.Collected,"-",2015)))


#world bank mst data
WB <- readRDS(paste0(dropboxDir,"Data/WBB/Clean/WBB_WB_env.RDS"))  %>% mutate(study="Boehm 2016")

#temp <- left_join(WB, tr, by=c("block","clusterid"))



head(PEC)
head(qPCR)
head(soilSTH)
head(WB)
table(WB$pos)
table(WB$sample, WB$target, !is.na(WB$abund))



#merge in abundance
qPCR_quant <- read.csv(paste0(dropboxDir,"Data/WBB/Erica Fuhrmeister - washb_qPCR_quant_10_23_18_adjusted_var_day.csv"))  

qPCR_quant <- qPCR_quant %>% mutate(env_date=dmy(paste0(intday,"-",Month.Collected,"-",2015))) %>%
select(PID,Assay:log.LOQ., env_date)

head(qPCR_quant)

#Impute BLOQ and BLOD
qPCR_quant <- qPCR_quant %>%
  mutate(
    qual=case_when(
      log.gc.sample.matrix. >0 ~ "ROQ",
      BLOQ==1 ~ "BLOQ",
      BLOD==1 ~ "BLOD"),
    log.gc.sample.matrix.=ifelse(BLOQ==1, log.LOQ., log.gc.sample.matrix.),
    log.gc.sample.matrix.=ifelse(BLOD==1, log.LOD., log.gc.sample.matrix.)
  )

head(qPCR)
head(qPCR_quant)


dim(qPCR)
qPCR <- left_join(qPCR, qPCR_quant, by=c("Unique.Numerical.ID", "Sample.Type","Assay","PID","env_date"))
dim(qPCR)
head(qPCR)

table(qPCR$Assay, !is.na(qPCR$Quantifiable))
table(qPCR$Sample.Type, !is.na(qPCR$Quantifiable))

qPCR %>% group_by(Quantifiable) %>%
  summarise(mean(log.gc.sample.matrix., na.rm=T), mean(log.LOD., na.rm=T), mean(log.LOQ., na.rm=T))
summary(qPCR$log.gc.sample.matrix.[qPCR$Quantifiable==1])
summary(10^(qPCR$log.gc.sample.matrix.[qPCR$Quantifiable==1]))


#Impute half the lower limit of detection 
qPCR <- qPCR %>% 
  mutate(
    log.gc.sample.matrix. = ifelse(Quantifiable==1, log.gc.sample.matrix., log.LOD./2),
    abund = 10^log.gc.sample.matrix.)


#code zoonotic vs. not zoonotic E. coli targets
PEC$EC_zoo <- ifelse(PEC$EPEC>0 | PEC$EHECSTX1>0 | PEC$EHECSTX2>0, 1,0)
PEC$EC_zoo <- ifelse(is.na(PEC$EPEC) & is.na(PEC$EHECSTX1) & is.na(PEC$EHECSTX2), NA,PEC$EC_zoo)
PEC$EC_not_zoo <- ifelse(PEC$EAEC>0 | PEC$EIEC>0 | PEC$ETEC>0, 1,0)
PEC$EC_not_zoo <- ifelse(is.na(PEC$EAEC)  & is.na(PEC$EIEC) & is.na(PEC$ETEC), NA,PEC$EC_not_zoo)
table(PEC$EC_zoo)
table(PEC$EC_not_zoo)
table(PEC$ECVG)

#Subset to the environmental vars (drop covariates)
colnames(PEC)
PEC <- PEC %>% subset(., select=c(PID, env_date, Unique.Numerical.ID, Sample.Type, round,
                      soilsun, m_hwobs, c_hwobs, dwcont, dwcov, raintime, animalno, MoistCont2, 
                      EPEC, EAEC, EHEC, EIEC, ETEC, ECVG, EHECSTX1, EHECSTX2, ETECLT1, ETECST1B,
                      EC_zoo, EC_not_zoo)) %>%
  rename(dataid=PID,
         sampleid=Unique.Numerical.ID,
         sample=Sample.Type) %>%
  gather(EPEC:EC_not_zoo, key = target, value = pos)
head(PEC)


#Just use any virulent gene - this is a combination variable of the pathogenic subtypes
# plus target split by zoonotic or not
PEC <- PEC %>% filter(target %in% c("ECVG","EC_zoo","EC_not_zoo"))

colnames(qPCR)
qPCR <- qPCR %>% subset(., select=c(PID, env_date, Unique.Numerical.ID, Sample.Type, Pos, abund, qual, log.gc.sample.matrix., Assay, round, 
                                    soilsun, m_hwobs, c_hwobs, dwcont, dwcov, raintime, animalno, MoistCont2)) %>%
  rename(dataid=PID,
         pos=Pos,
         log_conc=log.gc.sample.matrix.,
         sampleid=Unique.Numerical.ID,
         target=Assay,
         sample=Sample.Type)
head(qPCR)




#----------------------------------------------------------------------------
#Merge in env. STH data
#----------------------------------------------------------------------------

#get dates
soil_dates <- read_dta("C:/Users/andre/Downloads/WASHB-soil-sth-raw.dta") %>% 
  mutate(env_date=dmy(DateCollected)) %>% filter(!(UniqueID %in% c("LB","DS","SI","SS"))) %>%
  select(UniqueID, env_date)
summary(soil_dates$env_date)

#soil_dates <- soil_dates %>% filter(UniqueID %in% soilSTH$UniqueID) %>% arrange(env_date) %>% group_by(UniqueID) %>% slice(1)
soil_dates <- soil_dates %>% filter(UniqueID %in% soilSTH$UniqueID) %>% distinct() 

summary(soil_dates$env_date)

summary(soil_dates$la)
summary(soilSTH$lal)

dim(soilSTH)
soilSTH <- left_join(soilSTH, soil_dates, by="UniqueID")
dim(soilSTH)

summary(soilSTH$env_date)
table(is.na(soilSTH$env_date))

table(soilSTH$labmonth)
soilSTH <- soilSTH %>% 
  #mutate(env_date=dmy(paste0(16,"-",labmonth,"-",2015))-lag) %>%
  subset(., select=c(dataid, env_date, UniqueID, possth, posal, postt, epgal, epgtt, epgsth)) %>%
                       rename(sampleid=UniqueID)
                        
pos <- soilSTH %>% subset(., select=c(dataid, sampleid, env_date,
                                  possth, 
                                  posal, 
                                  postt)) %>%
  gather(possth:postt, key = target, value = pos) %>%
  mutate(target=gsub("pos","", target),
         target=case_when(
           target=="sth" ~ "sth",
           target=="al" ~ "ascaris",
           target=="tt"  ~ "trichuris"
         )) #%>% distinct(.)

quant <- soilSTH %>% subset(., select=c(dataid, sampleid, 
                                        epgal, epgtt, epgsth)) %>%
  gather(epgal:epgsth , key = target, value = abund) %>%
  mutate(target=case_when(
    target=="epgsth" ~ "sth",
    target=="epgal" ~ "ascaris",
    target=="epgtt"  ~ "trichuris"
  )) #%>% distinct(.)

dim(pos)
dim(quant)
dim(soilSTH)
soilSTH <- full_join(pos, quant, by=c("dataid","sampleid","target")) #%>% distinct(.)
dim(soilSTH)
table(is.na(soilSTH$pos),is.na(soilSTH$abund))

soilSTH$sample="S"
soilSTH$study="Kwong 2021"
soilSTH$round="endline"
soilSTH$dataid <- as.numeric(soilSTH$dataid)
soilSTH$sampleid <- as.numeric(soilSTH$sampleid)
head(soilSTH)



#----------------------------------------------------------------------------
#Merge env datasets
#----------------------------------------------------------------------------

env_fuhrmeister <- bind_rows(PEC, qPCR) %>% mutate(round=as.character(round), study="Fuhrmeister 2020")
env <- bind_rows(env_fuhrmeister, soilSTH)              
#env$round <- ""
env$sampleid <- as.character(env$sampleid)
env <- bind_rows(env, WB)
table(env$target, is.na(env$abund))

#Create asset PCA in WBB covariates
colnames(enrol)
df <-  enrol %>% subset(., select=c("dataid","roof","walls","cement","elec","asset_radio",
                      "asset_tvbw",      "asset_tvcol",     "asset_refrig",    "asset_bike",      "asset_moto",     
                      "asset_sewmach",   "asset_phone",     "asset_tv",        "asset_wardrobe",  "asset_table",    
                      "asset_chair",    
                      "asset_khat",      "asset_chouki",    "asset_mobile",   
                      "n_asset_wardrobe","n_asset_table",   "n_asset_chair",  
                      "n_asset_khat",  "n_asset_chouki",  "n_asset_mobile")) %>% 
            as.data.frame()

dim(df)
df <- df[complete.cases(df),]
dim(df)

# #Select assets and seperate out ID
id<-subset(df, select=c("dataid")) #drop subjectid
df<-df[,which(!(colnames(df) %in% c("dataid")))]

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(df) 


## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]   

pr.cp <- as.matrix(df) %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 

df$hhwealth <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.

#merge combined score back into main dataset
df.pca <- data.frame(dataid=id, hhwealth=df$hhwealth)

enrol <- left_join(enrol, df.pca, by="dataid")

#drop assets
enrol <-  enrol %>% subset(., select= -c(cement,asset_radio,
                                    asset_tvbw,      asset_tvcol,     asset_refrig,    asset_bike,      asset_moto,     
                                    asset_sewmach,   asset_phone,     asset_tv,        asset_wardrobe,  asset_table,    
                                    asset_chair,    
                                    asset_clock,  
                                    asset_khat,      asset_chouki,    asset_mobile,   
                                    n_asset_clock,
                                    n_asset_wardrobe,n_asset_table,   n_asset_chair,  
                                    n_asset_khat,  n_asset_chouki,  n_asset_mobile)) 


#Merge in covariates
env2 <-env
dim(env)
colnames(enrol)
env <- left_join(env2, enrol, by=c("dataid"))
table(is.na(env$clusterid))
env[is.na(env$clusterid),] #Missing is dataid 28698

WB[WB$dataid==28608,]
WB[WB$dataid==28698,]


env <- left_join(env, tr, by=c("block","clusterid"))
dim(env)
table(env$tr)
table(is.na(env$tr))

env <- env %>% rename(hhid=hhid.y) 
table(is.na(env$hhid))

#Make animsls binary
table(env$animals)
table(env$animalno)
table(is.na(env$animals), is.na(env$animalno))
env$animals[is.na(env$animals)] <- as.numeric(env$animalno[is.na(env$animals)]>0)
table(env$animals)
table(is.na(env$animals))

# #get animal ownership from other datasets for the sth dataset
# animals <- env %>% filter(!is.na(animals)) %>% distinct(dataid, .keep_all = T) %>% select(dataid, animals) %>% rename(animals_temp=animals)
# env <- left_join(env, animals, by="dataid")
# env$animals[is.na(env$animals)] <- env$animals_temp[is.na(env$animals)]

animals <- read.csv(paste0(dropboxDir,"Data/WBB/1. WASHB_Baseline_main_survey.csv")) %>% select(dataid, q4016, starts_with("q4114")) %>% rename(hhid=q4016) %>%
  mutate(animals2=as.numeric(q4114_1com + q4114_2com+ q4114_3com+ q4114_1h+ q4114_2h+ q4114_3h > 0), dataid=as.numeric(dataid)) %>% select(dataid, hhid, animals2)
head(animals)

env <- left_join(env, animals, by=c("dataid","hhid"))
table(env$animals2)
table(is.na(env$animals2))
table(env$animals, env$animals2)
table(is.na(env$animals), is.na(env$animals2))

#Use baseline animal ownership
env <- env %>% subset(., select = -c(animals)) %>% rename(animals=animals2)


#The one compound with dataid for WSH is sanitation arm based on Amy's positivity dataset
env$tr[env$tr=="WSH"] <- "Sanitation"


#Harmonize sample sample codes
table(env$sample, env$target)
env$sample[env$sample=="SW"] <- "W"

env$pos[env$pos==2] <- 1

summaryWBB <- env %>% group_by(tr, sample, target) %>% filter(!is.na(pos)) %>%
  summarise(N=n(), n=sum(pos, na.rm=T), 
            prev=round(mean(pos, na.rm=T)*100, 1), 
            abund=mean(log10(abund), na.rm=T))

write.csv(summaryWBB, file=here("figures/WBB_tab.csv"))

saveRDS(env, paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))





#----------------------------------------------------------------------------
#Make child health
#----------------------------------------------------------------------------


# Load Wash Benefits Bangladesh health datasets
anthro <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-anthro.csv")) %>% mutate(round=case_when(svy==1~"midline", svy==2~"endline"))
parasites <- read_dta(paste0(dropboxDir,"Data/WBB/wbb-parasite.dta"))

diar <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-diar.csv")) %>% mutate(round=case_when(svy==0~"baseline",svy==1~"midline", svy==2~"endline"))
#get hhid from enrol
hhid <- enrol %>% subset(., select =c("dataid","hhid")) %>% arrange(dataid, hhid)
diar <- left_join(diar, hhid, by=c("dataid"))

#raw intervention effect
diar_tr <- left_join(diar, tr, by=c("block","clusterid")) %>% filter(tr %in% c("Control","Sanitation"))

tab <- table(diar_tr$tr, diar_tr$diar7d) 
(tab[2,2]*tab[1,1])/(tab[2,1]*tab[1,2])  


#R01 diarrhea
r01_diar_full <- read.csv(paste0(dropboxDir,"Data/WBB/r01_child_health_all_variables.csv"))
colnames(r01_diar_full)
r01_diar <- r01_diar_full %>%
  subset(., select = c(dataid,
                       block,
                       cluster_id,
                       #child_id_measured,
                       child_id,
                       hhid,
                       date,
                       Round,
                       tr,
                       sex_child,
                       age_child_days,
                       who_diar7d)) %>%
  mutate(date=ymd(date),
         sex_child=case_when(sex_child==1 ~ "male", sex_child==2 ~"female"),
         round=as.character(Round)) %>%
  rename(clusterid=cluster_id,
         sex=sex_child,
         agedays=age_child_days,
         diar7d=who_diar7d,
         child_date=date,
         childid=child_id) 

tab <- table(r01_diar$tr, r01_diar$diar7d)
(tab[2,2]*tab[1,1])/(tab[2,1]*tab[1,2])  


#Check duplicates
dim(r01_diar)
dim(r01_diar %>% distinct(dataid, hhid, round, childid))

table(r01_diar$childid)

# temp <- r01_diar %>% group_by(dataid, hhid, round, childid) %>% mutate(N=n()) %>% arrange(desc(N),dataid, hhid, round, childid) %>% subset(., select = -c(N, round))
# table(temp$N)
# knitr::kable(head(temp, 17), "pipe")

r01_diar$childid[r01_diar$childid=="T"] <- "T1"  

table(r01_diar$round, r01_diar$diar7d) 
table(is.na(r01_diar$diar7d))
table(is.na(r01_diar$date))


diar <- diar %>%
  rename(child_date=svydate) %>%
  mutate(child_date =dmy(child_date)) %>%
  subset(., select = c(block,clusterid,dataid, hhid, round, child_date, agedays, sex,childid, diar7d))


#combine main study and r01 diarrhea
head(diar)
head(r01_diar)

table(r01_diar$childid)
table(diar$childid)

diar <- bind_rows(diar, r01_diar)
saveRDS(diar, file = paste0(dropboxDir, "Data/WBK/clean-wbb-diar.RDS"))



# temp <- diar2 %>% group_by( dataid, childid ) %>% summarize(sex1=first(sex), sex2=last(sex))
# table(temp$sex1, temp$sex2)
# temp2 <- temp %>% filter(sex1!=sex2)
# head(temp2)

# 1.	Child birth order/parity -aim 2 only

# 4.	Household food security -aim 2 only

# 6.	Parental age -aim 2 only

# Harmonizing anthro variable names with diar ahead of binding them
anthro<-anthro%>%
          rename(agedays=aged,
                 ageyrs=agey,
                 child_date_anthro=anthrodate) %>%
          mutate(svyyear=year(dmy(child_date_anthro)),
                 svyweek=week(dmy(child_date_anthro)))
#get hhid from enrol
anthro <- left_join(anthro, hhid, by=c("dataid"))


colnames(anthro)
anthro<-anthro%>%
  subset(., select = c(block,clusterid,dataid,hhid,round,child_date_anthro,agedays, sex,childid, laz,whz,waz))


# #Merge anthro and diar

anthro_diar <- bind_rows(diar, anthro)
table(is.na(anthro_diar$child_date))
anthro_diar$child_date_anthro <- dmy(anthro_diar$child_date_anthro)
anthro_diar$child_date[is.na(anthro_diar$child_date)] <- anthro_diar$child_date_anthro[is.na(anthro_diar$child_date)]
table(is.na(anthro_diar$child_date))

#----------------------------------------------------------------------------
#Merge in child/HH specific covariates
#----------------------------------------------------------------------------



colnames(enrol)
enrol <- enrol %>% subset(., select=c(dataid, momage, hfiacat))
dim(anthro_diar)
anthro_diar <- left_join(anthro_diar, enrol, by=c("dataid"))
dim(anthro_diar)
saveRDS(anthro_diar, paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS"))
