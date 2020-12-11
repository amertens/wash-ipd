


# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))
library(readxl)
library(caret)



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#Make WB abundance dataset
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#Details on coding from Amy:
# BLOQ means that we detected the target by qPCR but the concentration was below our limit of quantification 
# (Cq value corresponded to a concentration not within our standard curve). These are considered to be positive for 
# the target but not quantifiable. For your models using quantitative outcomes, you could substitute LOD/2 for non-detects 
# and substitute the mid-point between the LOD and the LOQ for the BLOQ samples. You could cite Erica's paper for this approach. 


#----------------------------------------------------------------------------
# Compiled Soil
#----------------------------------------------------------------------------

d <- read_excel(paste0(dropboxDir,"Data/WBB/Bangladesh_qPCRResult_WB.xlsx"), sheet="compiled soil")

compiled sw and H results
group_names <- colnames(d)
colnames(d) <- d[1,]
d <- d[-1,]
group_names[grepl("\\.\\.\\.",group_names)] <- NA

group_names<- fill(data.frame(names=group_names), "names")$names
group_names[is.na(group_names)] <-""
colnames(d) <- paste0(group_names, "_", colnames(d))
head(d)

#drop NA columns
index <- map_lgl(d, ~ all(is.na(.)))
d <- d[, -index]
head(d)

#drop empty columns
d <- d[,-c(nzv(d, freqCut = 100/0))]

colnames(d)
d <- d %>%
  rename(
    "sampleid"="_sample",                                    
    "hhid"="_house ID",                                   
    "date"="_sampling date",                              
    "RV_conc"="RV_cp/ul",                                    
    "RV_detect"="RV_# detected out of 2",                      
    "RV_qual"="RV_Qualifier",                                
    "RV_dilution"="RV_dilution level",                           
    "AV_conc"="Avian_cp/ul",                                 
    "AV_detect"="Avian_# detected out of 2",                   
    "AV_qual"="Avian_Qualifier",                             
    "BacR_conc"="BacR_cp/ul",                                  
    "BacR_detect"="BacR_# detected out of 2",                    
    "BacR_qual"="BacR_Qualifier",                              
    "GenBac3_conc"="GenBac3_cp/ul",                               
    "GenBac3_detect"="GenBac3_# detected out of 2",                 
    "GenBac3_qual"="GenBac3_Qualifier",                           
    "HumM2_conc"="HumM2 (Environmental MMX)_cp/ul",             
    "HumM2_detect"="HumM2 (Environmental MMX)_# detected out of 2",
    "HumM2_qual"="HumM2 (Environmental MMX)_Qualifier") %>%
  subset(., select = -c(date))


#Transform to long-form
soil_LF <- d %>%
  gather(RV_conc:HumM2_qual, key = var, value = val) %>%
  mutate(target=str_split(var, "_", simplify = T)[,1],
         var=str_split(var, "_", simplify = T)[,2]) %>%
  filter(sampleid!="EB") %>%
  pivot_wider(id_cols=c(sampleid, hhid, target), names_from=var, values_from=val) %>%
  mutate(sample="S",
         detect=ifelse(is.na(detect),0, detect))
head(soil_LF)


#----------------------------------------------------------------------------
# Compiled SW/hands
#----------------------------------------------------------------------------

d <- read_excel(paste0(dropboxDir,"Data/WBB/Bangladesh_qPCRResult_WB.xlsx"), sheet="compiled sw and H results")


group_names <- colnames(d)
colnames(d) <- d[1,]
d <- d[-1,]
group_names[grepl("\\.\\.\\.",group_names)] <- NA

group_names<- fill(data.frame(names=group_names), "names")$names
group_names[is.na(group_names)] <-""
colnames(d) <- paste0(group_names, "_", colnames(d))
head(d)

#drop NA columns
index <- map_lgl(d, ~ all(is.na(.)))
d <- d[, -index]
head(d)

#drop empty columns
d <- d[,-c(nzv(d, freqCut = 100/0))]

colnames(d)
d <- d %>%
  rename(
    "sampleid"="_SAMPLE ID",  
    "sample"="_SAMPLE TYPE         (H, W)",
    "hhid"="_UNIQUE NUMERICAL ID",                                   
    "date"="_DATE (DD.MM.YYYY)",           
    "volume"="_VOLUME",
    "RV_conc"="RV_cp/ul",                                    
    "RV_detect"="RV_# detected out of 2",                      
    "RV_qual"="RV_Qualifier",                                
    "AV_conc"="Avian_cp/ul",                                 
    "AV_detect"="Avian_# detected out of 2",                   
    "AV_qual"="Avian_Qualifier",                             
    "BacR_conc"="BacR_cp/ul",                                  
    "BacR_detect"="BacR_# detected out of 2",                    
    "BacR_qual"="BacR_Qualifier",                              
    "GenBac3_conc"="GenBac3_cp/ul",                               
    "GenBac3_detect"="GenBac3_# detected out of 2",                 
    "GenBac3_qual"="GenBac3_Qualifier",                           
    "HumM2_conc"="HumM2 (Environmental MMX)_cp/ul",             
    "HumM2_detect"="HumM2 (Environmental MMX)_# detected out of 2",
    "HumM2_qual"="HumM2 (Environmental MMX)_Qualifier") %>%
  subset(., select = -c(`_sample`, date, `_FILTERED BY:`, `_Comments:`))


#Transform to long-form
water_LF <- d %>%
  gather(RV_conc:HumM2_qual, key = var, value = val) %>%
  mutate(target=str_split(var, "_", simplify = T)[,1],
         var=str_split(var, "_", simplify = T)[,2]) %>%
  filter(!(sample %in% c("HB","LB")), !is.na(sampleid)) %>%
  pivot_wider(id_cols=c(sampleid, hhid, target, sample), names_from=var, values_from=val) %>%
  mutate(detect=ifelse(is.na(detect), 0, detect))
head(water_LF)


#----------------------------------------------------------------------------
# Impute non-detects
#----------------------------------------------------------------------------


# substitute LOD/2 for non-detects 
#   hands = 5
#   water = 1
#   soil = 1000-1800
# and substitute the mid-point between the LOD and the LOQ for the BLOQ samples
#Need to merge in soil moisture 

df <- bind_rows(water_LF, soil_LF)
table(df$detect)
table(df$qual)
