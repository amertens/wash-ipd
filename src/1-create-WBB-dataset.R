

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))



#----------------------------------------------------------------------------
#Make env dataset
#----------------------------------------------------------------------------


#Load env MST/pathogen datasets
PEC <- read.csv(paste0(dropboxDir,"Data/WBB/washb_PEC_10_23_18_adjusted_var.csv"))
qPCR <- read.csv(paste0(dropboxDir,"Data/WBB/washb_qPCR_10_23_18_adjusted_var.csv"))  
soilSTH <- read_dta(paste0(dropboxDir,"Data/WBB/WASHB-soil-sth.dta"))
#world bank mst data
WB <- read_dta(paste0(dropboxDir,"Data/WBB/BDdata_20AUG16_GENBAC_ADJUSTED_AMY.dta"))
#abundance data


qPCR_quant <- read.csv(paste0(dropboxDir,"Data/WBB/Erica - washb_qPCR_quant_10_23_18_adjusted_var_abundance.csv"))  %>% select(Assay:log.LOQ.)
head(qPCR)
head(qPCR_quant)


qPCR <- left_join(qPCR, qPCR_quant, by=c("Unique.Numerical.ID", "Sample.Type","Assay","PID","Month.Collected"))
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


# labs <- makeVlist(soilSTH) %>% mutate(label=as.character(label)) %>% as.data.frame()
# write.csv(labs, paste0(dropboxDir,"Data/WBB/wbb_STH_soil_codebook.csv"))

# labs <- makeVlist(WB) %>% mutate(label=as.character(label)) %>% as.data.frame()
# write.csv(labs, paste0(dropboxDir,"Data/WBB/wbb_WB_codebook.csv"))


#Load covariates and treatment arms
enrol <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-enrol.csv"))
tr <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-real-tr.csv"))


#Subset to the environmental vars (drop covariates)
colnames(PEC)
PEC <- PEC %>% subset(., select=c(PID, Month.Collected, Unique.Numerical.ID, Sample.Type, EPEC, EAEC, EHEC,              
                      EIEC, ETEC, ECVG, EHECSTX1, EHECSTX2, ETECLT1, ETECST1B,
                      soilsun, m_hwobs, c_hwobs, dwcont, dwcov, raintime, animalno, MoistCont2)) %>%
  rename(dataid=PID,
         sampleid=Unique.Numerical.ID,
         pec.month=Month.Collected,
         type=Sample.Type) %>%
  gather(EPEC:ETECST1B, key = target, value = pos)
head(PEC)

#Just use any virulent gene
PEC <- PEC %>% filter(target=="ECVG")

colnames(qPCR)
qPCR <- qPCR %>% subset(., select=c(PID, Month.Collected, Unique.Numerical.ID, Sample.Type, Pos, log.gc.sample.matrix., Assay, 
                                    soilsun, m_hwobs, c_hwobs, dwcont, dwcov, raintime, animalno, MoistCont2)) %>%
  rename(dataid=PID,
         pos=Pos,
         log_conc=log.gc.sample.matrix.,
         sampleid=Unique.Numerical.ID,
         pec.month=Month.Collected,
         target=Assay,
         type=Sample.Type)
head(qPCR)

#world bank MST data
WB2 <- WB %>% subset(., select = c(
  pid,
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


#soil sth
soilSTH <- soilSTH %>% subset(., select=c(dataid, UniqueID, possth, posal, postt, epgal, epgtt, epgsth)) %>%
                       rename(sampleid=UniqueID)
pos <- soilSTH %>% subset(., select=c(dataid, sampleid, 
                                  possth, 
                                  posal, 
                                  postt)) %>%
  gather(possth:postt, key = target, value = pos) %>%
  mutate(target=gsub("pos","", target))

quant <- soilSTH %>% subset(., select=c(dataid, sampleid, 
                                        epgal, epgtt, epgsth)) %>%
  gather(epgal:epgsth , key = target, value = abund) %>%
  mutate(target=case_when(
    target=="epgal" ~ "sth",
    target=="epgsth" ~ "ascaris",
    target=="epgtt"  ~ "trichuris"
  ))





soilSTH <- full_join(pos, quant, by=c("dataid","sampleid","target"))

soilSTH$target[soilSTH$target=="al"] <- "ascaris"
soilSTH$target[soilSTH$target=="tt"] <- "trichuris"
soilSTH$type="S"
soilSTH$dataid <- as.numeric(soilSTH$dataid)
soilSTH$sampleid <- as.numeric(soilSTH$sampleid)
head(soilSTH)

#Merge env datasets
env <- bind_rows(PEC, qPCR, soilSTH)
env$round <- "WBB followup"
env <- bind_rows(env, WB3)

head(env)


#Create asset PCA in WBB covariates
colnames(enrol)




df <-  enrol %>% subset(., select=c("dataid","roof","walls","cement","elec","asset_radio",
                      "asset_tvbw",      "asset_tvcol",     "asset_refrig",    "asset_bike",      "asset_moto",     
                      "asset_sewmach",   "asset_phone",     "asset_tv",        "asset_wardrobe",  "asset_table",    
                      "asset_chair",    
                      #"asset_clock",    don't use clock due to missingness
                      "asset_khat",      "asset_chouki",    "asset_mobile",   
                      "n_asset_wardrobe","n_asset_table",   "n_asset_chair",  
                      #"n_asset_clock",   
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
hist(enrol$hhwealth)

#drop assets
enrol <-  enrol %>% subset(., select= -c(roof,walls,cement,elec,asset_radio,
                                    asset_tvbw,      asset_tvcol,     asset_refrig,    asset_bike,      asset_moto,     
                                    asset_sewmach,   asset_phone,     asset_tv,        asset_wardrobe,  asset_table,    
                                    asset_chair,    
                                    asset_clock,  
                                    asset_khat,      asset_chouki,    asset_mobile,   
                                    n_asset_clock,
                                    n_asset_wardrobe,n_asset_table,   n_asset_chair,  
                                    n_asset_khat,  n_asset_chouki,  n_asset_mobile)) 

#Merge in covariates
env <- left_join(env, enrol, by=c("dataid"))
env <- left_join(env, tr, by=c("block","clusterid"))
env <- env %>% filter(env$tr!="WSH")

env <- env %>% rename(
  abund_only_detect=logquant,
  abund=censquant  
)


saveRDS(env, paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))



#----------------------------------------------------------------------------
#Merge in env. STH data
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
#Make child health
#----------------------------------------------------------------------------


# Load Wash Benefits Bangladesh primary datasets
anthro <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-anthro.csv"))
diar <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-diar.csv"))
parasites <- read_dta(paste0(dropboxDir,"Data/WBB/wbb-parasite.dta"))
  
 
# Harmonizing anthro variable names with diar ahead of binding them
anthro<-anthro%>%
          rename(agedays=aged,
                 ageyrs=agey,
                 svydate=anthrodate)
anthro<-anthro%>%
          mutate(svyyear=year(dmy(svydate)),
                 svyweek=week(dmy(svydate)))




#----------------------------------------------------------------------------
#Merge in STH data
#----------------------------------------------------------------------------

# reading in public IDs
pub_ids <- read.csv(paste0(dropboxDir,"Data/WBB/public-ids.csv"))



# merge in public IDs to anthro/diar
anthro_diar_pub <- inner_join(anthro_diar,pub_ids,by=c("block","clusterid","dataid"))

# Load Wash Benefits Bangladesh child soil transmitted helminth datasets
sth <- read.csv(paste0(dropboxDir,"Data/WBB//washb-bangladesh-sth-public.csv"))

#subset to needed variables
# Trimming vars that overlap with enrol.csv (which will be merged later)
intersect_sth_enrol<-intersect(names(sth),names(enrol))[-1]
sth <- sth[,which(!(colnames(sth) %in% intersect_sth_enrol))]
head(sth)

sth <- sth %>% subset(., select = c(dataid, personid,tr,sex, agem, logalepg, loghwepg, logttepg, al, tt, hw, sth))

#merge in public IDs
sth <- left_join(sth, pub_ids, by="dataid")

# Harmonizing sth variable names to facilitate bind with anthro/diar/env
sth <- sth %>% subset(., select = -c(dataid, block, clusterid)) %>%
           rename(dataid=dataid_r,
                  clusterid=clusterid_r,
                  block=block_r,
                  childid=personid)


# binding child health outcomes
anthro$measure = "anthro"
diar$measure = "diar"
sth$measure = "sth"
child_health <- bind_rows(anthro, diar, sth)

saveRDS(child_health, paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS"))

#Old code to merge in ecoli measures

# # reading in public IDs
# pub_ids <- read.csv(paste0(dropboxDir,"Data/WBB/public-ids.csv"))
# 
# # merge in public IDs to anthro/diar
# anthro_diar_pub <- inner_join(anthro_diar,pub_ids,by=c("block","clusterid","dataid"))
# 
# # merge treatment arms into diar dataset
# 
# # removing non-public ID vars
# # non_pub_vars<-c("block","clusterid","dataid")
# # anthro_diar_pub <- anthro_diar_pub%>%select(-non_pub_vars)
# 
# # Load Wash Benefits Bangladesh environmental sample datasets
# env_early <- read_dta(paste0(dropboxDir,"Data/WBB/washb-bangladesh-early-env-data-public.dta"))
# env_mid_end <- read_dta(paste0(dropboxDir,"Data/WBB/washb-bangladesh-midend-env-data-public.dta"))
# 
# # Trimming vars that overlap with enrol.csv (which will be merged later)
# intersect_env_early_enrol<-intersect(names(env_early),names(enrol))
# intersect_env_mid_end_enrol<-intersect(names(env_mid_end),names(enrol))
# env_early <- env_early%>%select(-intersect_env_early_enrol)
# env_mid_end <- env_mid_end%>%select(-intersect_env_mid_end_enrol)
# 
# # Environmental data apply to target children only - adding childid and tchild to reflect this
# env_early$childid<-"T1"
# env_early$tchild<-"Target child"
# env_mid_end$childid<-"T1"
# env_mid_end$tchild<-"Target child"
# 
# # Harmonizing variable types between anthro/diar and env_early, by coercing types of the latter to types of the former
# # storing var names of intersect(anthro/diar,env_early) vars in vectors by type of var
# ints<-c()
# nums<-c()
# factors<-c()
# intersect_1<-intersect(names(anthro_diar_pub),names(env_early))
# for (i in 1:length(intersect_1)){
#   if (class(anthro_diar_pub[,intersect_1[i]])=="integer"){
#     ints<-c(ints,intersect_1[i])
#   }
#   if (class(anthro_diar_pub[,intersect_1[i]])=="numeric"){
#     nums<-c(nums,intersect_1[i])
#   }
#   if (class(anthro_diar_pub[,intersect_1[i]])=="factor"){
#     factors<-c(factors,intersect_1[i])
#   }
# }
# 
# # converting env_early var types to the types of the matching vars in anthro/diar
# 
# for (i in 1:length(names(env_early))){
#   if (names(env_early)[i] %in% ints){
#     env_early[,i]<-as.integer(unlist(env_early[,i]))
#   }
#   if (names(env_early)[i] %in% nums){
#     env_early[,i]<-as.numeric(unlist(env_early[,i]))
#   }
#   if (names(env_early)[i] %in% factors){
#     env_early[,i]<-as.factor(unlist(env_early[,i]))
#   }
# }
# 
# # binding anthro/diar with env_early
# env_early$sex <- as.character(env_early$sex)
# env_early$sex[env_early$sex=="0"] <- "female"
# env_early$sex[env_early$sex=="1"] <- "male"
# anthro_diar_env_early<-bind_rows(anthro_diar_pub,env_early)
# 
# # Harmonizing variable types between anthro/diar/env_early and env_mid_end
# # storing var names of intersect(anthro/diar/env_early,env_mid_end) vars in vectors by type of var
# ints<-c()
# nums<-c()
# factors<-c()
# chars<-c()
# intersect_2<-intersect(names(anthro_diar_env_early),names(env_mid_end))
# for (i in 1:length(intersect_2)){
#   if (class(anthro_diar_env_early[,intersect_2[i]])=="integer"){
#     ints<-c(ints,intersect_2[i])
#   }
#   if (class(anthro_diar_env_early[,intersect_2[i]])=="numeric"){
#     nums<-c(nums,intersect_2[i])
#   }
#   if(class(anthro_diar_env_early[,intersect_2[i]])=="factor"){
#     factors<-c(factors,intersect_2[i])
#   }
#   if(class(anthro_diar_env_early[,intersect_2[i]])=="character"){
#     chars<-c(chars,intersect_2[i])
#   }
# }
# 
# # converting env_mid_end var types to the types of the matching vars in anthro/diar/env_early
# 
# for (i in 1:length(names(env_mid_end))){
#   if (names(env_mid_end)[i] %in% ints){
#     env_mid_end[,i]<-as.integer(unlist(env_mid_end[,i]))
#   }
#   if (names(env_mid_end)[i] %in% nums){
#     env_mid_end[,i]<-as.numeric(unlist(env_mid_end[,i]))
#   }
#   if (names(env_mid_end)[i] %in% factors){
#     env_mid_end[,i]<-as.factor(unlist(env_mid_end[,i]))
#   }
#   if (names(env_mid_end)[i] %in% chars){
#     env_mid_end[,i]<-as.character(unlist(env_mid_end[,i]))
#   }
# }
# 
# # binding anthro/diar/env_early with env_mid_end
# anthro_diar_env<-bind_rows(anthro_diar_env_early,env_mid_end)





# # storing var names of intersect(anthro/diar/env,sth) vars in vectors by type of var
# ints<-c()
# nums<-c()
# factors<-c()
# chars<-c()
# intersect_3<-intersect(names(anthro_diar_env),names(sth))
# for (i in 1:length(intersect_3)){
#   if (class(anthro_diar_env[,intersect_3[i]])=="integer"){
#     ints<-c(ints,intersect_3[i])
#   }
#   if (class(anthro_diar_env[,intersect_3[i]])=="numeric"){
#     nums<-c(nums,intersect_3[i])
#   }
#   if(class(anthro_diar_env[,intersect_3[i]])=="factor"){
#     factors<-c(factors,intersect_3[i])
#   }
#   if(class(anthro_diar_env[,intersect_3[i]])=="character"){
#     chars<-c(chars,intersect_3[i])
#   }
# }
# 
# # converting sth var types to the types of the matching vars in anthro/diar/env
# 
# for (i in 1:length(names(sth))){
#   if (names(sth)[i] %in% ints){
#     sth[,i]<-as.integer(unlist(sth[,i]))
#   }
#   if (names(sth)[i] %in% nums){
#     sth[,i]<-as.numeric(unlist(sth[,i]))
#   }
#   if (names(sth)[i] %in% factors){
#     sth[,i]<-as.factor(unlist(sth[,i]))
#   }
#   if (names(sth)[i] %in% chars){
#     sth[,i]<-as.character(unlist(sth[,i]))
#   }
# }
# 
# # binding anthro/diar/env and sth
# anthro_diar_env <- anthro_diar_env %>% mutate(
#   tr = case_when(
#     tr==1~"Control",
#     tr==2~"Water",
#     tr==3~"Sanitation",
#     tr==4~"Handwashing",
#     tr==5~"WSH",
#     tr==6~"Nutrition",
#     tr==7~"Nutrition + WSH"
#   ) 
# )
# anthro_diar_env_sth<-bind_rows(anthro_diar_env,sth)
# 
# # Merging in enrol (baseline covariates)
# # creating enrol-specific svydate var
# enrol<-enrol%>%rename(svydate.enrol=svydate)
# 
# # merging in public IDs to enrol
# enrol_pub <- inner_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
# 
# # removing non-public ID vars
# non_pub_vars<-c("block","clusterid","dataid")
# enrol_pub <- enrol_pub%>%select(-non_pub_vars)
# 
# # merging anthro/diar/env/sth and enrol datasets
# anthro_diar_env_sth_enrol<-left_join(anthro_diar_env_sth,enrol_pub,by=c("dataid_r","clusterid_r","block_r"))
# 
# # sorting by child- and time-indexing variables
# anthro_diar_env_sth_enrol <- anthro_diar_env_sth_enrol%>%arrange(dataid_r,childid,agedays)
# 
# # rearranging order of columns
# dataid_col<-grep("dataid_r", colnames(anthro_diar_env_sth_enrol))
# childid_col<-grep("childid", colnames(anthro_diar_env_sth_enrol))
# agedays_col<-grep("agedays", colnames(anthro_diar_env_sth_enrol))
# clusterid_col<-grep("clusterid_r", colnames(anthro_diar_env_sth_enrol))
# block_col<-grep("block_r", colnames(anthro_diar_env_sth_enrol))
# tr_col<-grep("tr", colnames(anthro_diar_env_sth_enrol))
# a_cols<-c(dataid_col,childid_col,agedays_col,clusterid_col,block_col,tr_col)
# b_cols<-seq(1,length(colnames(anthro_diar_env_sth_enrol)))
# b_cols<-b_cols[! b_cols %in% a_cols]
# anthro_diar_env_sth_enrol<-anthro_diar_env_sth_enrol[,c(a_cols,b_cols)]
# 
# # Converting character variables that should be factors into factors
# anthro_diar_env_sth_enrol$childid<-as.factor(anthro_diar_env_sth_enrol$childid)
# anthro_diar_env_sth_enrol$tchild<-as.factor(anthro_diar_env_sth_enrol$tchild)
# anthro_diar_env_sth_enrol$sex<-as.factor(anthro_diar_env_sth_enrol$sex)
# 
# 
# # Copying childid to motherid since they effectively are the same (per Andrew)
# anthro_diar_env_sth_enrol$motherid <- anthro_diar_env_sth_enrol$childid
# 
# #Save long-form dataset in dropbox directory
# saveRDS(anthro_diar_env_sth_enrol, paste0(dropboxDir,"Data/WBB/clean/WBB-longform.RDS"))
# 
# #----------------------------------------------------------------------#
# #------------------------QA-ing long-form dataset----------------------#
# #----------------------------------------------------------------------#
# 
# # Checking missings in the ID/Age variables
# sum(is.na(anthro_diar_env_sth_enrol$dataid_r))
# sum(is.na(anthro_diar_env_sth_enrol$childid))
# sum(is.na(anthro_diar_env_sth_enrol$agedays))
# 
# # Note: 2824 missings come from 2824 individuals with missing "agedays" in sth dataset
# # these 2824 observations don't have data on any vars that might approximate age other than svyweek and svyyear
# # directly below is code that isolates these 2824 observations in case one wants to examine them
# sth2<-sth%>%filter(is.na(agedays))
# summary(sth2)
#  
# # comparing long data to anthro
# # not interested in comparing ID and time-related variables as those will differ between the datasets
# # (particularly, long dataset will have additional observations of these variables from non-anthro constituent datasets)
# id_time_vars <- c("dataid","childid","motherid","tchild","clusterid","block","svy","svydate","svyyear","svyweek","month","dob","agedays","agem","ageyrs","sex","birthord")
# anthro_comp<-anthro%>%select(-id_time_vars)
# anthro_cols<-names(anthro_comp)
# comp_anthro_full_df <- data.frame(matrix(NA,nrow=length(anthro_cols),ncol=2))
# names(comp_anthro_full_df) <- c("Variable","Equal b/t Anthro and long?")
# for (i in 1:length(anthro_cols)){
#     var <- anthro_cols[i]
#     anthro_var<-anthro_comp[,anthro_cols[i]]
#     anthro_var<-sort(anthro_var[!is.na(anthro_var)])
#     fulldf_var <- anthro_diar_env_sth_enrol[,anthro_cols[i]]
#     fulldf_var <- sort(fulldf_var[!is.na(fulldf_var)])
#     equal <- all.equal(anthro_var,fulldf_var)
#     row <- c(var,equal)
#     comp_anthro_full_df[i,] <- row
# }
# comp_anthro_full_df
# 
# # comparing long data to Diar
# # not interested in comparing ID and time-related variables as those will differ between the datasets
# id_time_vars <- c("dataid","clusterid","block","childid","agedays","tchild","svy","svydate","month","sex","dob","ageyrs")
# diar_comp<-diar%>%select(-id_time_vars)
# diar_cols<-names(diar_comp)
# comp_diar_full_df <- data.frame(matrix(NA,nrow=length(diar_cols),ncol=2))
# names(comp_diar_full_df) <- c("Variable","Equal b/t diar and long?")
# for (i in 1:length(diar_cols)){
#   var <- diar_cols[i]
#   diar_var<-diar_comp[,diar_cols[i]]
#   diar_var<-sort(diar_var[!is.na(diar_var)])
#   fulldf_var <- anthro_diar_env_sth_enrol[,diar_cols[i]]
#   fulldf_var <- sort(fulldf_var[!is.na(fulldf_var)])
#   equal <- all.equal(diar_var,fulldf_var)
#   row <- c(var,equal)
#   comp_diar_full_df[i,] <- row
# }
# comp_diar_full_df
# 
# # comparing long data to env datasets (env_early and env_mid_end combined)
# # not interested in comparing ID and time-related variables as those will differ between the datasets
# id_time_vars <- c("dataid_r","clusterid_r","block_r","childid","agedays","tr","tchild","month","sex","wet")
# env<-bind_rows(env_early,env_mid_end)
# env_comp<-env%>%select(-id_time_vars)
# env_cols<-names(env_comp)
# comp_env_full_df <- data.frame(matrix(NA,nrow=length(env_cols),ncol=2))
# names(comp_env_full_df) <- c("Variable","Equal b/t env and long?")
# for (i in 1:length(env_cols)){
#   var <- env_cols[i]
#   env_var<-as.numeric(unlist(env_comp[,env_cols[i]]))
#   env_var<-sort(env_var[!is.na(env_var)])
#   fulldf_var <- anthro_diar_env_sth_enrol[,env_cols[i]]
#   fulldf_var <- sort(fulldf_var[!is.na(fulldf_var)])
#   equal <- all.equal(env_var,fulldf_var)
#   row <- c(var,equal)
#   comp_env_full_df[i,] <- row
# }
# comp_env_full_df
# 
# # comparing long data to sth
# # not interested in comparing ID and time-related variables as those will differ between the datasets
# id_time_vars <- c("dataid_r","childid","clusterid_r","block_r","tr","sex","agedays","agem","ageyrs","svyweek","svyyear","wet","birthord","month")
# sth_comp<-sth%>%select(-id_time_vars)
# sth_cols<-names(sth_comp)
# comp_sth_full_df <- data.frame(matrix(NA,nrow=length(sth_cols),ncol=2))
# names(comp_sth_full_df) <- c("Variable","Equal b/t sth and long?")
# for (i in 1:length(sth_cols)){
#   var <- sth_cols[i]
#   sth_var<-sth_comp[,sth_cols[i]]
#   sth_var<-sort(sth_var[!is.na(sth_var)])
#   fulldf_var <- anthro_diar_env_sth_enrol[,sth_cols[i]]
#   fulldf_var <- sort(fulldf_var[!is.na(fulldf_var)])
#   equal <- all.equal(sth_var,fulldf_var)
#   row <- c(var,equal)
#   comp_sth_full_df[i,] <- row
# }
# comp_sth_full_df
# 
# #----------------------------------------------------------------------#
# #-------Checking that enrol, environmental, and sth datasets-----------#
# #--------------have matching values for overlapping vars---------------#
# #----(justifies merging enrol at the end when creating long dataset)---#
# #----------------------------------------------------------------------#
# 
# # Load Wash Benefits Bangladesh environmental sample datasets
# env_early <- read_dta(paste0(dropboxDir,"Data/WBB/3-env-datasets/washb-bangladesh-early-env-data-public.dta"))
# env_mid_end <- read_dta(paste0(dropboxDir,"Data/WBB/3-env-datasets/washb-bangladesh-midend-env-data-public.dta"))
# sth <- read.csv(paste0(dropboxDir,"Data/WBB/2-sth-kk-outcome-datasets/Public/washb-bangladesh-sth-public.csv"))
# # adjusting sth ID var names to match enrol
# sth<-sth%>%rename(dataid_r=dataid,
#                   clusterid_r=clusterid,
#                   block_r=block)
# # Outersect finds elements that are the complement of intersect (i.e. elements unique to each object)
# outersect <- function(x, y) {
#   sort(c(setdiff(x, y),
#          setdiff(y, x)))
# }
# 
# # comparing enrol and env_early
# enrol_pub<-full_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
# intersect_enrol_enve<-intersect(names(enrol_pub),names(env_early))
# # subsetting to overlapping vars only
# env_early2<-env_early[,c(intersect_enrol_enve)]
# enrol2<-enrol_pub[,c(intersect_enrol_enve)]
# # enrol represents more households (i.e. has more dataid's) than env_early, so enrol needs to be subsetted to where the two overlap
# enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(env_early2$dataid_r),unique(enrol2$dataid_r)))
# # identifying and printing dataid's in env dataset that are NOT in enrol
# mis<-outersect(unique(env_early2$dataid_r),unique(enrol2$dataid_r))
# print(mis)
# # the dataID printed above had NAs for all the enrol-type covariates, so no info is lost by exlcuding it
# env_early2<-env_early2%>%filter(dataid_r!=mis)
# # forcing data types to be numbers for comparison
# enrol2<-data.frame(lapply(enrol2,function(x) as.integer(x)))
# env_early2<-data.frame(lapply(env_early2,function(x) as.integer(as.character(x))))
# momedu<-env_early2$momedu<-as.numeric(as_factor(env_early2$momedu))
# env_early2$momedu<-momedu
# # sorting for comparison
# enrol2<-enrol2%>%arrange(dataid_r)
# env_early2<-env_early2%>%arrange(dataid_r)
# #if the line below produces "TRUE", all values match between the datasets
# print(all.equal(enrol2,env_early2))
# 
# 
# # comparing enrol and env_mid_end, timepoint=1
# enrol_pub<-full_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
# env_mid_end_t1<-env_mid_end%>%filter(timepoint==1)
# intersect_enrol_envme<-intersect(names(enrol_pub),names(env_mid_end_t1))
# env_mid_end2<-env_mid_end_t1[,c(intersect_enrol_envme)]
# enrol2<-enrol_pub[,c(intersect_enrol_envme)]
# enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r)))
# # identifying and printing dataid's in env dataset that are NOT in enrol
# mis<-outersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r))
# print(mis)
# # the dataID printed above had NAs for all the enrol-type covariates, so no info is lost by exlcuding it
# env_mid_end2<-env_mid_end2%>%filter(dataid_r!=mis)
# enrol2<-data.frame(lapply(enrol2,function(x) as.integer(x)))
# momedu<-env_mid_end2$momedu<-as.numeric(as_factor(env_mid_end2$momedu))
# env_mid_end2<-data.frame(lapply(env_mid_end2,function(x) as.integer(as.character(x))))
# env_mid_end2$momedu<-momedu
# enrol2<-enrol2%>%arrange(dataid_r)
# env_mid_end2<-env_mid_end2%>%arrange(dataid_r)
# #if the line below produces "TRUE", all values match between the datasets
# print(all.equal(enrol2,env_mid_end2))
# 
# # comparing enrol and env_mid_end, timepoint=2
# enrol_pub<-full_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
# env_mid_end_t2<-env_mid_end%>%filter(timepoint==2)
# intersect_enrol_envme<-intersect(names(enrol_pub),names(env_mid_end_t2))
# env_mid_end2<-env_mid_end_t2[,c(intersect_enrol_envme)]
# enrol2<-enrol_pub[,c(intersect_enrol_envme)]
# enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r)))
# # identifying and printing dataid's in env dataset that are NOT in enrol
# mis<-outersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r))
# print(mis)
# # nothing in outersect, i.e. dataid's perfectly match
# enrol2<-data.frame(lapply(enrol2,function(x) as.integer(x)))
# momedu<-env_mid_end2$momedu<-as.numeric(as_factor(env_mid_end2$momedu))
# env_mid_end2<-data.frame(lapply(env_mid_end2,function(x) as.integer(as.character(x))))
# env_mid_end2$momedu<-momedu
# enrol2<-enrol2%>%arrange(dataid_r)
# env_mid_end2<-env_mid_end2%>%arrange(dataid_r)
# #if the line below produces "TRUE", all values match between the datasets
# print(all.equal(enrol2,env_mid_end2))
# 
# 
# # comapring enrol and sth
# enrol_pub <- inner_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
# sth_uni<-sth[!duplicated(sth$dataid_r),]
# intersect_enrol_sth<-intersect(names(enrol_pub),names(sth_uni))
# sth2<-sth_uni[,c(intersect_enrol_sth)]
# enrol2<-enrol_pub[,c(intersect_enrol_sth)]
# enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(sth2$dataid_r),unique(enrol2$dataid_r)))
# # identifying and printing dataid's in env dataset that are NOT in enrol
# mis<-outersect(unique(sth2$dataid_r),unique(enrol2$dataid_r))
# print(mis)
# # nothing in outersect, i.e. dataid's perfectly match
# sth2$momedu<-as.factor(sth2$momedu)
# sth2$hfiacat<-as.factor(sth2$hfiacat)
# enrol2<-data.frame(lapply(enrol2,function(x) as.numeric(x)))
# sth2<-data.frame(lapply(sth2,function(x) as.numeric(x)))
# #recoding sth vars to match enrol for comparison
# sth_change<-c("asset_radio",
#               "asset_refrig",
#               "asset_bike",
#               "asset_moto",
#               "asset_sewmach",
#               "asset_tv",
#               "asset_wardrobe",
#               "asset_table",
#               "asset_chair",
#               "asset_khat",
#               "asset_chouki",
#               "asset_mobile",
#               "roof",
#               "walls",
#               "floor",
#               "elec")
# # changing levels of factor vars to match between enrol and sth
# # binary variable changes, sth->enrol: 2->0
# # factor variable changes. sth->enrol: "asset_phone", 1->9, 2->1, 3->0; "asset_clock", 1->NA,3->0,2->1
# # no change needed: asset_tvbw,asset_tvcol
# for (i in 1:length(names(sth2))){
#   if (names(sth2)[i] %in% sth_change){
#     sth2[,i]<-ifelse(sth2[,i]==2,0,1)
#   }
#   if(names(sth2)[i]=="asset_phone"){
#     sth2[,i]<-ifelse(sth2[,i]==1,9,ifelse(sth2[,i]==2,1,ifelse(sth2[,i]==3,0,NA)))
#   }
#   if(names(sth2)[i]=="asset_clock"){
#     sth2[,i]<-ifelse(sth2[,i]==1,NA,ifelse(sth2[,i]==3,0,ifelse(sth2[,i]==2,1,NA)))
#   }
# }
# enrol2<-enrol2%>%arrange(dataid_r)
# sth2<-sth2%>%arrange(dataid_r)
# #if the line below produces "TRUE", all values match between the datasets
# print(all.equal(enrol2,sth2))