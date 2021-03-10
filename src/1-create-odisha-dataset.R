

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))



#----------------------------------------------------------------------------
#Clean env dataset
#----------------------------------------------------------------------------

village <- read.csv(paste0(dropboxDir,"Data/Odisha/Data set (effect of MST and pathogen marker detection on diarrhea rates)_11-02-14.csv")) %>% filter(!is.na(villageID))
child <- read_dta(paste0(dropboxDir,"Data/Odisha/diarrhoea and weight data Odisha san trial.dta"))

#extract stata labels
makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  labs <- tibble(name = names(labels),
                 label = labels)
  labs$label<- as.character(labs$label)
  return(labs)
}

labs <- makeVlist(child)


head(village)
head(child)

colnames(village)
colnames(child)


# qPCR <- left_join(qPCR, qPCR_quant, by=c("Unique.Numerical.ID", "Sample.Type","Assay","PID","Month.Collected"))
# head(qPCR)
# 
# table(qPCR$Assay, !is.na(qPCR$Quantifiable))
# table(qPCR$Sample.Type, !is.na(qPCR$Quantifiable))
# 
# qPCR %>% group_by(Quantifiable) %>%
#   summarise(mean(log.gc.sample.matrix., na.rm=T), mean(log.LOD., na.rm=T), mean(log.LOQ., na.rm=T))
# summary(qPCR$log.gc.sample.matrix.[qPCR$Quantifiable==1])
# summary(10^(qPCR$log.gc.sample.matrix.[qPCR$Quantifiable==1]))
# 
# 
# #Impute half the lower limit of detection 
# qPCR <- qPCR %>% 
#   mutate(
#     log.gc.sample.matrix. = ifelse(Quantifiable==1, log.gc.sample.matrix., log.LOD./2),
#     abund = 10^log.gc.sample.matrix.)
# 
# 
# # labs <- makeVlist(soilSTH) %>% mutate(label=as.character(label)) %>% as.data.frame()
# # write.csv(labs, paste0(dropboxDir,"Data/Odisha/wbb_STH_soil_codebook.csv"))
# 
# # labs <- makeVlist(WB) %>% mutate(label=as.character(label)) %>% as.data.frame()
# # write.csv(labs, paste0(dropboxDir,"Data/Odisha/wbb_WB_codebook.csv"))
# 
# 
# 
# 
# #Subset to the environmental vars (drop covariates)
# colnames(PEC)
# PEC <- PEC %>% subset(., select=c(PID, Month.Collected, Unique.Numerical.ID, Sample.Type, EPEC, EAEC, EHEC,              
#                       EIEC, ETEC, ECVG, EHECSTX1, EHECSTX2, ETECLT1, ETECST1B,
#                       soilsun, m_hwobs, c_hwobs, dwcont, dwcov, raintime, animalno, MoistCont2)) %>%
#   rename(dataid=PID,
#          sampleid=Unique.Numerical.ID,
#          pec.month=Month.Collected,
#          sample=Sample.Type) %>%
#   gather(EPEC:ETECST1B, key = target, value = pos)
# head(PEC)
# 
# #Just use any virulent gene
# PEC <- PEC %>% filter(target=="ECVG")
# 
# colnames(qPCR)
# qPCR <- qPCR %>% subset(., select=c(PID, Month.Collected, Unique.Numerical.ID, Sample.Type, Pos, log.gc.sample.matrix., Assay, 
#                                     soilsun, m_hwobs, c_hwobs, dwcont, dwcov, raintime, animalno, MoistCont2)) %>%
#   rename(dataid=PID,
#          pos=Pos,
#          log_conc=log.gc.sample.matrix.,
#          sampleid=Unique.Numerical.ID,
#          pec.month=Month.Collected,
#          target=Assay,
#          sample=Sample.Type)
# head(qPCR)
# 
# 
# 
# 
# #----------------------------------------------------------------------------
# #Merge in env. STH data
# #----------------------------------------------------------------------------
# soilSTH <- soilSTH %>% subset(., select=c(dataid, UniqueID, possth, posal, postt, epgal, epgtt, epgsth)) %>%
#                        rename(sampleid=UniqueID)
# pos <- soilSTH %>% subset(., select=c(dataid, sampleid, 
#                                   possth, 
#                                   posal, 
#                                   postt)) %>%
#   gather(possth:postt, key = target, value = pos) %>%
#   mutate(target=gsub("pos","", target),
#          target=case_when(
#            target=="sth" ~ "sth",
#            target=="al" ~ "ascaris",
#            target=="tt"  ~ "trichuris"
#          )) %>% distinct(.)
# 
# quant <- soilSTH %>% subset(., select=c(dataid, sampleid, 
#                                         epgal, epgtt, epgsth)) %>%
#   gather(epgal:epgsth , key = target, value = abund) %>%
#   mutate(target=case_when(
#     target=="epgsth" ~ "sth",
#     target=="epgal" ~ "ascaris",
#     target=="epgtt"  ~ "trichuris"
#   )) %>% distinct(.)
# 
# dim(pos)
# dim(quant)
# dim(soilSTH)
# soilSTH <- full_join(pos, quant, by=c("dataid","sampleid","target")) #%>% distinct(.)
# dim(soilSTH)
# table(is.na(soilSTH$pos),is.na(soilSTH$abund))
# 
# soilSTH$sample="S"
# soilSTH$dataid <- as.numeric(soilSTH$dataid)
# soilSTH$sampleid <- as.numeric(soilSTH$sampleid)
# head(soilSTH)
# 
# #----------------------------------------------------------------------------
# #Merge env datasets
# #----------------------------------------------------------------------------
# 
# env <- bind_rows(PEC, qPCR, soilSTH)
# env$round <- ""
# env$sampleid <- as.character(env$sampleid)
# env <- bind_rows(env, WB)
# 
# 
# #Create asset PCA in WBB covariates
# colnames(enrol)
# df <-  enrol %>% subset(., select=c("dataid","roof","walls","cement","elec","asset_radio",
#                       "asset_tvbw",      "asset_tvcol",     "asset_refrig",    "asset_bike",      "asset_moto",     
#                       "asset_sewmach",   "asset_phone",     "asset_tv",        "asset_wardrobe",  "asset_table",    
#                       "asset_chair",    
#                       #"asset_clock",    don't use clock due to missingness
#                       "asset_khat",      "asset_chouki",    "asset_mobile",   
#                       "n_asset_wardrobe","n_asset_table",   "n_asset_chair",  
#                       #"n_asset_clock",   
#                       "n_asset_khat",  "n_asset_chouki",  "n_asset_mobile")) %>% 
#             as.data.frame()
# 
# dim(df)
# df <- df[complete.cases(df),]
# dim(df)
# 
# # #Select assets and seperate out ID
# id<-subset(df, select=c("dataid")) #drop subjectid
# df<-df[,which(!(colnames(df) %in% c("dataid")))]
# 
# ##Computing the principal component using eigenvalue decomposition ##
# princ.return <- princomp(df) 
# 
# 
# ## To get the first principal component in a variable ##
# load <- loadings(princ.return)[,1]   
# 
# pr.cp <- as.matrix(df) %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
# 
# df$hhwealth <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.
# 
# #merge combined score back into main dataset
# df.pca <- data.frame(dataid=id, hhwealth=df$hhwealth)
# 
# enrol <- left_join(enrol, df.pca, by="dataid")
# 
# #drop assets
# enrol <-  enrol %>% subset(., select= -c(roof,walls,cement,elec,asset_radio,
#                                     asset_tvbw,      asset_tvcol,     asset_refrig,    asset_bike,      asset_moto,     
#                                     asset_sewmach,   asset_phone,     asset_tv,        asset_wardrobe,  asset_table,    
#                                     asset_chair,    
#                                     asset_clock,  
#                                     asset_khat,      asset_chouki,    asset_mobile,   
#                                     n_asset_clock,
#                                     n_asset_wardrobe,n_asset_table,   n_asset_chair,  
#                                     n_asset_khat,  n_asset_chouki,  n_asset_mobile)) 
# 
# #Merge in covariates
# env2 <-env
# dim(env)
# env <- left_join(env2, enrol, by=c("dataid"))
# env <- left_join(env, tr, by=c("block","clusterid"))
# dim(env)
# 
# table(env$tr)
# table(is.na(env$tr))
# 
# #The one compound with dataid for WSH is sanitation arm based on Amy's positivity dataset
# env$tr[env$tr=="WSH"] <- "Sanitation"
# 
# 
# #Harmonize sample sample codes
# table(env$sample, env$target)
# env$sample[env$sample=="SW"] <- "W"
# 
# env$pos[env$pos==2] <- 1
# 
# 
# 
# saveRDS(env, paste0(dropboxDir, "Data/Odisha/Clean/WBB_env.RDS"))
# 
# 
# 
# 
# 
# #----------------------------------------------------------------------------
# #Make child health
# #----------------------------------------------------------------------------
# 
# 
# # Load Wash Benefits Bangladesh primary datasets
# anthro <- read.csv(paste0(dropboxDir,"Data/Odisha/washb-bangladesh-anthro.csv"))
# diar <- read.csv(paste0(dropboxDir,"Data/Odisha/washb-bangladesh-diar.csv"))
# parasites <- read_dta(paste0(dropboxDir,"Data/Odisha/wbb-parasite.dta"))
# 
# 
# # Harmonizing anthro variable names with diar ahead of binding them
# anthro<-anthro%>%
#           rename(agedays=aged,
#                  ageyrs=agey,
#                  svydate=anthrodate)
# anthro<-anthro%>%
#           mutate(svyyear=year(dmy(svydate)),
#                  svyweek=week(dmy(svydate)))
# 
# #Merge anthro and diar
# dim(anthro)
# dim(diar)
# anthro_diar <- full_join(diar, anthro, by=c("block","clusterid","dataid","svy"))
# dim(anthro_diar)
# 
# anthro_diar <- anthro_diar %>%
#   filter(svy==2) %>%
#   mutate(svy=as.character(svy))
# 
# #----------------------------------------------------------------------------
# #Merge in STH data
# #----------------------------------------------------------------------------
# 
# 
# # Load Wash Benefits Bangladesh child soil transmitted helminth datasets
# sth <- read.csv(paste0(dropboxDir,"Data/Odisha//washb-bangladesh-sth-public.csv"))
# 
# # reading in public IDs
# pub_ids <- read.csv(paste0(dropboxDir,"Data/Odisha/public-ids.csv"))
# 
# 
# # Harmonizing sth variable names to facilitate bind with anthro/diar/env
# sth <- sth %>%
#            rename(dataid_r=dataid,
#                   clusterid_r=clusterid,
#                   block_r=block,
#                   childid=previousCid)
# 
# #merge in public IDs
# sth <- left_join(sth, pub_ids, by=c("dataid_r","clusterid_r","block_r"))
# sth <- sth %>% subset(., select = -c(dataid_r, block_r, clusterid_r)) 
# head(sth)
# 
# #subset to needed variables
# sth <- sth %>% subset(., select = c(dataid, childid,tr,sex, agem, logalepg, loghwepg, logttepg, al, tt, hw, sth)) %>%
#   mutate(svy="sth")
# 
# 
# 
# # binding child health outcomes
# anthro_diar$measure = "anthro_diar"
# sth$measure = "sth"
# child_health <- bind_rows(anthro_diar, sth)
# 
# saveRDS(child_health, paste0(dropboxDir, "Data/Odisha/Clean/WBB_child_health.RDS"))
