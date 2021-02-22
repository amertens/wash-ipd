


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

#Get limits from "Occurrence of Host-Associated Fecal Markers on Child Hands,
#Household Soil, and Drinking Water in Rural Bangladeshi
#Households" Boehm paper

#Lowest detectable concentration
# (LDC) and LLOQ values were calculated by converting one
# copy (cp) per reaction and 10 cp per reaction (the most dilute
#  standard that consistently amplified), respectively, to appropriate
# units (cp per 100 mL, cp per two hands, and cp per gram
#        of dry soil).

# https://pubs.acs.org/doi/suppl/10.1021/acs.estlett.6b00382/suppl_file/ez6b00382_si_001.pdf
# The following methods were used to interpret QPCR measurements from the MST
# marker validation study. Master standard curves for each assay were created by
# combining standard curves from individual plates (Table S9). The master standard curve
# was used to calculate concentrations in samples using the sample's mean cycle threshold
# (Cq). A sample was considered detected within the range of quantification (ROQ) if the
# sample's mean Cq value corresponded to a concentration between the highest and lowest
# standards. If the sample had a mean Cq value higher than the lowest standard, then the
# sample was reported as detected but below the lower limit of quantification (LLOQ). If
# the sample had a Cq less than that of the highest concentration standard, then the sample
# was decimally diluted until its concentration was within the ROQ. For the fecal samples,
# if 2 or more out of 3 of the reactions had undetermined Cq, the sample was reported as a
# non-detect (ND). 

# For the quantitative assessment approach, the concentration of the molecular target
# detected in a fecal composite sample was normalized to the amount of feces as measured
# by MPN of ENT (i.e., copies per MPN ENT). The sensitivity metric for the quantitative
# assessment approach is that the median concentration of the MST molecular target in
# target host feces should be higher than 10 copies per MPN ENT. Ten copies per MPN
# ENT corresponds to the minimum concentration of the molecular target that would be
# detectable in a 100 mL environmental sample with 100 MPN ENT/100 mL assuming that
# the 100 mL was filtered onto a filter, 100 µL of eluent was obtained during DNA
# extraction, and 2 µL of undiluted extract was added to the QPCR reaction which has a
# lower limit of quantification of 10 copies/µL. The specificity metric is that the
# concentration of a molecular target in non-target host feces must be below the range of
# target concentrations detected in target host feces. For the specificity analysis, only
# concentrations detected within in the ROQ were considered5


#----------------------------------------------------------------------------
# Compiled Soil
#----------------------------------------------------------------------------

d <- read_excel(paste0(dropboxDir,"Data/WBB/Bangladesh_qPCRResult_WB.xlsx"), sheet="compiled soil")

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
    "rv_conc"="RV_cp/ul",                                    
    "rv_detect"="RV_# detected out of 2",                      
    "rv_qual"="RV_Qualifier",                                
    "rv_dilution"="RV_dilution level",                           
    "av_conc"="Avian_cp/ul",                                 
    "av_detect"="Avian_# detected out of 2",                   
    "av_qual"="Avian_Qualifier",                             
    "br_conc"="BacR_cp/ul",                                  
    "br_detect"="BacR_# detected out of 2",                    
    "br_qual"="BacR_Qualifier",                              
    "gbc_conc"="GenBac3_cp/ul",                               
    "gbc_detect"="GenBac3_# detected out of 2",                 
    "gbc_qual"="GenBac3_Qualifier",                           
    "Hum_conc"="HumM2 (Environmental MMX)_cp/ul",             
    "Hum_detect"="HumM2 (Environmental MMX)_# detected out of 2",
    "Hum_qual"="HumM2 (Environmental MMX)_Qualifier") %>%
  mutate(uniqueID=sampleid) %>%
  subset(., select = -c(date))


#Transform to long-form
soil_LF <- d %>%
  gather(rv_conc:Hum_qual, key = var, value = val) %>%
  mutate(target=str_split(var, "_", simplify = T)[,1],
         var=str_split(var, "_", simplify = T)[,2]) %>%
  filter(sampleid!="EB") %>%
  pivot_wider(id_cols=c(uniqueID, sampleid, hhid, target), names_from=var, values_from=val) %>%
  mutate(type="S",
         hhid = as.numeric(hhid),
         detect=ifelse(is.na(detect),0, detect),
         conc = as.numeric(conc))
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
    "type"="_SAMPLE TYPE         (H, W)",
    "uniqueID"="_UNIQUE NUMERICAL ID",                                   
    "date"="_DATE (DD.MM.YYYY)",           
    "volume"="_VOLUME",
    "rv_conc"="RV_cp/ul",                                    
    "rv_detect"="RV_# detected out of 2",                      
    "rv_qual"="RV_Qualifier",                                
    "av_conc"="Avian_cp/ul",                                 
    "av_detect"="Avian_# detected out of 2",                   
    "av_qual"="Avian_Qualifier",                             
    "br_conc"="BacR_cp/ul",                                  
    "br_detect"="BacR_# detected out of 2",                    
    "br_qual"="BacR_Qualifier",                              
    "gbc_conc"="GenBac3_cp/ul",                               
    "gbc_detect"="GenBac3_# detected out of 2",                 
    "gbc_qual"="GenBac3_Qualifier",                           
    "Hum_conc"="HumM2 (Environmental MMX)_cp/ul",             
    "Hum_detect"="HumM2 (Environmental MMX)_# detected out of 2",
    "Hum_qual"="HumM2 (Environmental MMX)_Qualifier") %>%
  subset(., select = -c(`_sample`, date, `_FILTERED BY:`, `_Comments:`)) 

d$sampleid <- gsub("SW.","SW-",d$sampleid)
d$sampleid <- gsub("H.","H-",d$sampleid)
d$sampleid <- gsub("W.","W-",d$sampleid)
d$hhid <- str_split(d$sampleid, pattern="-", simplify = T)[,2]
d$hhid <- str_split(d$hhid, pattern="\\.", simplify = T)[,1]

# df <- d %>% filter(sampleid=="SW.05803.20.12") 
# df$hhid <- str_split(df$sampleid, pattern="-", simplify = T)[,2]
# df$hhid <- str_split(df$hhid, pattern="\\.", simplify = T)[,1]

# d %>% filter(sampleid=="SW.05803.20.12") %>% select(hhid)
# d %>% filter(sampleid=="SW-05803.20.12")%>% select(hhid)
# 
# summary(d$hhid)
# table(d$hhid)
# 
# d %>% filter(hhid=="05803" )

#Transform to long-form
water_LF <- d %>%
  gather(rv_conc:Hum_qual, key = var, value = val) %>%
  mutate(target=str_split(var, "_", simplify = T)[,1],
         var=str_split(var, "_", simplify = T)[,2],
         hhid = as.numeric(hhid),
         volume = as.numeric(volume)) %>%
  filter(!(type %in% c("HB","LB")), !is.na(hhid)) %>%
  pivot_wider(id_cols=c(uniqueID, sampleid, hhid, target, type, volume), names_from=var, values_from=val) %>%
  mutate(detect=ifelse(is.na(detect), 0, detect),
         conc = as.numeric(conc))
water_LF$type[water_LF$type=="SW"] <- "W"
head(water_LF)


#----------------------------------------------------------------------------
# Convert cp/ul to correct abundance units
#----------------------------------------------------------------------------

df <- bind_rows(water_LF, soil_LF)
df <- df %>% mutate(
  dilution=as.numeric(dilution),
  abund = case_when(
    type=="H" ~ conc * (100/volume) * 200,
    type=="W" ~ conc * (100/volume),
    type=="S" ~ conc * dilution * (100/0.25)
  ))
head(df)

summary(df$abund)
summary(df$abund[df$detect>0 & df$type=="H"])
summary(df$abund[df$detect>0 & df$type=="W"])
summary(df$abund[df$detect>0 & df$type=="S"])

summary(df$abund[df$qual=="ROQ" & df$type=="H"])
summary(df$abund[df$qual=="ROQ" & df$type=="W"])
summary(df$abund[df$qual=="ROQ" & df$type=="S"])

summary(df$abund[df$qual=="BLOQ" & df$type=="H"])
summary(df$abund[df$qual=="BLOQ" & df$type=="W"])
summary(df$abund[df$qual=="BLOQ" & df$type=="S"])

summary(df$abund[df$qual=="DNQ" & df$type=="H"])
summary(df$abund[df$qual=="DNQ" & df$type=="W"])
summary(df$abund[df$qual=="DNQ" & df$type=="S"])

# #Mark GB3 positivity (not in other dataset)
# table(df$target, df$qual)
# table(df$target, df$detect )


#----------------------------------------------------------------------------
# Load and clean positivity data
#----------------------------------------------------------------------------


WB <- read_dta(paste0(dropboxDir,"Data/WBB/BDdata_20AUG16_GENBAC_ADJUSTED_AMY.dta"))
lab<-makeVlist(WB)

write.csv(WB, paste0(dropboxDir,"Data/WBB/BDdata_20AUG16_GENBAC_ADJUSTED_AMY.csv"))



#Load covariates and treatment arms
# enrol <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-enrol.csv"))
# tr <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-real-tr.csv"))


#world bank MST data
WB2 <- WB %>% subset(., select = c(
  pid,
  WUNIQUENUMERICALID,
  HUNIQUENUMERICALID,
  Ssample,
  arm,
  ruminant_0_1,
  birds_0_1,
  W_br_1_0,
  H_br_1_0,
  S_br_1_0_combo,
  W_av_1_0_combo,
  H_av_1_0_combo,
  S_av_1_0_combo,
  W_rv_1_0,
  H_rv_1_0,
  S_rv_1_0,
  W_hm_1_0,
  H_hm_1_0,
  S_hm_1_0,
  WLGgbcp100ml,
  HLGgbcp2hds,
  SLGgbcpgram,
  fracmoisturesoil)) %>%
  rename(dataid=pid)
colnames(WB2) <- gsub("_combo","",colnames(WB2))
colnames(WB2) <- gsub("_0_1","",colnames(WB2))
colnames(WB2) <- gsub("_1_0","",colnames(WB2))

for(i in 1:ncol(WB2)){
  WB2[,i]<- na_if(WB2[,i], 9999)
  WB2[,i]<- na_if(WB2[,i], 99999)
}
glimpse(WB2)

WB3 <- WB2 %>%
  gather(W_br:SLGgbcpgram  , key = target, value = pos) %>%
  filter(!is.na(pos)) %>%
  mutate(target = case_when(
    target=="HLGgbcp2hds" ~ "H_gbc",
    target=="SLGgbcpgram" ~ "S_gbc",
    target=="WLGgbcp100ml" ~ "W_gbc",
    target==target ~ target
  )) 
#mutate(target=gsub("pos","", target))
head(WB3)
WB3$type <- str_split(WB3$target,"_", simplify = T)[,1]
WB3$target <- str_split(WB3$target,"_", simplify = T)[,2]
WB3$target[WB3$target=="hm"] <-  "Hum"
WB3$round <- "World Bank"
WB3$abund <- ifelse(WB3$target=="gbc", WB3$pos, NA)
WB3$pos[WB3$target=="gbc"] <- NA

WB4 <- WB3 %>% mutate(
  uniqueID  = case_when(
      type=="H" ~ as.character(HUNIQUENUMERICALID), 
      type=="W" ~ as.character(WUNIQUENUMERICALID), 
      type=="S" ~ as.character(Ssample)
    )
  ) %>%
  subset(., select = -c(WUNIQUENUMERICALID, HUNIQUENUMERICALID, Ssample))
head(WB4)

#----------------------------------------------------------------------------
# merge positivity and abundance data
#----------------------------------------------------------------------------

d1 <- WB4 %>% arrange(dataid) %>% subset(., select = -c(abund))
d2 <- df %>% mutate(hhid=as.numeric(hhid),
                          type=ifelse(type=="SW","W",type))  %>% 
  filter(!is.na(hhid)) %>%
  arrange(hhid) 
dim(d1)
dim(d2)
table(d1$type, d1$target)
table(d2$type, d2$target)
#df <- inner_join(d1, d2, by=c("uniqueID","target","type"))
df <- right_join(d1, d2, by=c("uniqueID","target","type"))
head(df)
dim(df)
table(df$type, df$target)
#Note: one hand sample not merging, and a few water GBC
table(df$detect, df$pos, df$type)
table(df$detect, df$pos, df$target)

#examine where hhid's don't match
df[df$hhid!=df$dataid,] %>% select(hhid, dataid)

#rows failing to merge
d1f <- anti_join(d1, d2, by=c("uniqueID","target","type"))
d2f <- anti_join(d2, d1, by=c("uniqueID","target","type"))
dim(d1f)
dim(d2f)
head(d1f)
head(d2f)
table(d1f$hhid)
table(d2f$hhid)
table(d1f$target)
table(d2f$target)
table(d1f$type)
table(d2f$type)

#mark positives for gbc
df$pos[df$target=="gbc"] <- ifelse(df$detect[df$target=="gbc"]==0,0,1)
df$pos[df$target=="gbc" & is.na(df$detect)] <- NA
table(df$pos[df$target=="gbc"])

#mark missing dataid
temp<-sapply(strsplit(df$sampleid, "-"), "[", 2)
temp<-as.numeric(sapply(strsplit(temp, "\\."), "[", 1))
df$dataid[is.na(df$dataid)] <- temp[is.na(df$dataid)]


#----------------------------------------------------------------------------
# Impute non-detects
#----------------------------------------------------------------------------


# substitute LOD/2 for non-detects 
# and substitute the mid-point between the LOD and the LOQ for the BLOQ samples

# Lowest detectable concentration
# (LDC) and LLOQ values were calculated by converting one
# copy (cp) per reaction and 10 cp per reaction (the most dilute
# standard that consistently amplified), respectively, to appropriate
# units (cp per 100 mL, cp per two hands, and cp per gram
# of dry soil).

# LDC and LLOQ values for the environmental samples were 50
# and 500 cp/100 mL of water, 125 and 1250 cp/two hands, and
# approximately 400 and 4000 cp/g of soil (exact value depended
#                                          on moisture content)

#Need to merge in soil moisture 

table(df$detect)
table(df$qual)
table(df$type)

df <- df %>%
  mutate(
    conc = as.numeric(conc),
    abund = case_when(
      type=="H" & detect==0 ~ 125, 
      type=="W" & detect==0 ~ 50,
      type=="S" & detect==0 ~ 400, #Need to make based on soil moisture
      type=="H" & detect!=0 & qual=="BLOQ" ~ (1250-125)/2, 
      type=="W" & detect!=0 & qual=="BLOQ" ~ (500-50)/2,
      type=="S" & detect!=0 & qual=="BLOQ" ~ (4000-400)/2, #Need to make based on soil moisture
      qual=="DNQ" ~ NA_real_,
      qual=="ROQ" ~ abund
    )
  )

summary(df$conc)
summary(df$abund)


#Harmonize names with WBB followup
df$type[df$type=="H"] <- "CH"

#Save data
saveRDS(df, paste0(dropboxDir, "Data/WBB/Clean/WBB_WB_env.RDS"))

