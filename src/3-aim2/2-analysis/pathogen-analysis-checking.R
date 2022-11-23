
rm(list=ls())
source(here::here("0-config.R"))
source(paste0(here::here(),"/src/3-aim2/2-analysis/0-pathogen-function.R"))

# #targets in CH data

ch_pathogens <- c(
  "Giardia",
  "Ascaris",
  "Trichuris",
  "Pathogenic E. coli",
  "Cryptosporidium",
  "Entamoeba histolytica",
  "Adenovirus",
  "Norovirus",
  "Rotavirus",
  "Shigella",
  "Yersinia",
  "C. difficile",
  "Campylobacter",
  "Salmonella",
  "V. cholerae"
)


#notes on dates
#Kwong measured STH after 2 years, KK sth after 2.5 years. But note the years are coming from the EE taq data so need to use child_date_STH
#Steinbaum also good based on dates in data. Check manuscripts
#Grembi measured after 14 months, but Furhmeister after 2 years
#need to check or median-impute missing pathogen dates
#need to update table to have right numbers and prevalences after dropping dates


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))



# #drop aggregate values:
d <- d %>% filter(!(grepl("Any ",target) & target != "Any STH"), sample!="any sample type",
                  !(target %in% c("Zoonotic E. coli","Non-zoonotic E. coli")))
table(d$study, d$ch_pos_cdiff)


# df <- d %>% filter(!is.na(ch_pos_giardia),  sample=="CH" & target=="Giardia")
# head(df)
# 
# table(df$study)
# table(df$pos, df$ch_pos_giardia)

#drop flagged values based on time ordering
#table(d$pathogen_drop_flag)
#d <- d %>% filter(pathogen_drop_flag!=1 | is.na(pathogen_drop_flag))

# dim(d)
# d <- d %>% filter(!is.na(ch_pos_giardia)|  
#                   !is.na(ch_pos_entamoeba)| 
#                   !is.na(ch_pos_crypto)| 
#                   !is.na(ch_qpcr_pos_ascaris)|   
#                   !is.na(ch_qpcr_pos_trichuris)| 
#                   !is.na(ch_pos_ascaris)|
#                   !is.na(ch_pos_trichuris)|
#                   !is.na(ch_pos_giardia_EE)|     
#                   !is.na(ch_pos_entamoeba_EE)| 
#                   !is.na(ch_pos_crypto_EE)|      
#                   !is.na(ch_pos_ascaris_EE)|  
#                   !is.na(ch_pos_trichuris_EE)|  
#                   !is.na(ch_pos_adenovirus)|   
#                   !is.na(ch_pos_norovirus)|   
#                   !is.na(ch_pos_rotavirus)|     
#                   !is.na(ch_pos_cdiff)|      
#                   !is.na(ch_pos_campylobacter)| 
#                   !is.na(ch_pos_salmonella)|  
#                   !is.na(ch_pos_shigella)|   
#                   !is.na(ch_pos_cholera)| 
#                   !is.na(ch_pos_yersinia)|     
#                   !is.na(ch_pos_path_ecoli)) %>% droplevels()
# dim(d)

table(d$study)

#use sth date for Kwong
d$child_date_pathogen[d$study=="Kwong 2021"] <- d$child_date_sth[d$study=="Kwong 2021"]
d$child_date_pathogen[d$study=="Fuhrmeister 2020" & d$target=="Giardia"] <- d$child_date_sth[d$study=="Fuhrmeister 2020" & d$target=="Giardia"]
d$date_diff <- d$child_date_pathogen - d$env_date


# capone_upper= 124 +14 #add buffer due to coarsened dates
# capone_lower= -14
# table(d$pos[d$study=="Capone 2021"],d$ch_pos_norovirus[d$study=="Capone 2021"])
# d$ch_pos_shigella[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_salmonella[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_ascaris[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_trichuris[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_giardia[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_norovirus[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_cdiff[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_path_ecoli[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
# table(d$pos[d$study=="Capone 2021"], d$ch_pos_norovirus[d$study=="Capone 2021"])
# 
# table(d$ch_pos_shigella[d$study=="Capone 2022 in prep"])
# d$ch_pos_shigella[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_salmonella[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_ascaris[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_trichuris[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_giardia[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_norovirus[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_cdiff[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# d$ch_pos_path_ecoli[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
# table(d$ch_pos_shigella[d$study=="Capone 2022 in prep"])

#Maybe get extra giardia from EE study
table(d$ch_pos_giardia[d$study=="Fuhrmeister 2020"])

#d <- d %>% filter( date_diff>0)
table(as.numeric(d$date_diff[d$study=="Fuhrmeister 2020"]))
#d <- d %>% filter(study=="Fuhrmeister 2020")
table(d$ch_pos_giardia)
table(d$ch_pos_giardia_EE)
table(d$ch_pos_giardia,d$ch_pos_giardia_EE)
table(1*is.na(d$ch_pos_giardia),is.na(d$ch_pos_giardia_EE))
table(1*is.na(d$ch_pos_giardia),is.na(d$ch_pos_giardia_EE), d$pos)
d$ch_pos_giardia[is.na(d$ch_pos_giardia)] <- d$ch_pos_giardia_EE[is.na(d$ch_pos_giardia)]

d$ch_pos_giardia[d$study=="Fuhrmeister 2020" & d$date_diff > 121 |  d$study=="Fuhrmeister 2020" &  d$date_diff<=0 ] <- NA
d$ch_pos_path_ecoli[d$study=="Fuhrmeister 2020" & d$date_diff > 121 |  d$study=="Fuhrmeister 2020" &  d$date_diff<=0 ] <- NA
table(d$ch_pos_giardia[d$study=="Fuhrmeister 2020"])


d$ch_qpcr_pos_ascaris[d$study=="Kwong 2021" & d$date_diff > 124 | d$study=="Kwong 2021" &  d$date_diff < 0] <- NA
d$ch_qpcr_pos_trichuris[d$study=="Kwong 2021" & d$date_diff > 124 | d$study=="Kwong 2021" &  d$date_diff < 0] <- NA
d$ch_pos_ascaris[d$study=="Steinbaum 2019" & d$date_diff > 124 | d$study=="Steinbaum 2019" &  d$date_diff < 0] <- NA
d$ch_pos_trichuris[d$study=="Steinbaum 2019" & d$date_diff > 124 | d$study=="Steinbaum 2019" &  d$date_diff < 0] <- NA

#Save data for table 1
saveRDS(d, file=paste0(dropboxDir,"Data/pathogen_analysis_data.rds"))




#make list of pairs - binary
paired_pathogens <- as.data.frame(t(data.frame(
           c("ch_pos_giardia",   "Giardia"),     
           c("ch_pos_entamoeba", "Entamoeba histolytica"),     
           c("ch_pos_crypto", "Cryptosporidium"),       
           c("ch_qpcr_pos_ascaris",   "Ascaris"),
           c("ch_qpcr_pos_trichuris", "Trichuris"),
           c("ch_pos_ascaris",   "Ascaris"),
          c("ch_pos_trichuris",  "Trichuris"),
          c("ch_pos_giardia_EE",   "Giardia"),     
          c("ch_pos_entamoeba_EE", "Entamoeba histolytica"),     
          c("ch_pos_crypto_EE", "Cryptosporidium"),       
          c("ch_pos_ascaris_EE",   "Ascaris"),
          c("ch_pos_trichuris_EE",  "Trichuris"),
         c( "ch_pos_adenovirus", "Adenovirus"),   
         c("ch_pos_norovirus",   "Norovirus"),   
         c("ch_pos_rotavirus",  "Rotavirus"),    
         c("ch_pos_cdiff",  "C. difficile"),       
         c( "ch_pos_campylobacter",  "Campylobacter"),
         c( "ch_pos_salmonella",  "Salmonella"),   
         c( "ch_pos_shigella",   "Shigella"),   
         c( "ch_pos_cholera", "V. cholerae"),       
         c("ch_pos_yersinia",   "Yersinia"),     
         c("ch_pos_path_ecoli", "Pathogenic E. coli"))))

rownames(paired_pathogens) <- NULL
colnames(paired_pathogens) <- c("outcome","exposure")

#make list of pairs - continious

paired_pathogens_cont <- as.data.frame(t(data.frame(
  exposure=c("ch_abund_giardia",  "Giardia"),     
            c("ch_abund_entamoeba", "Entamoeba histolytica"),
            c("ch_abund_ascaris",    "Ascaris"), 
            c("ch_abund_trichuris", "Trichuris"),
            c("ch_abund_crypto",     "Cryptosporidium"),
            c("ch_abund_giardia_EE",   "Giardia"),     
            c("ch_abund_entamoeba_EE", "Entamoeba histolytica"),     
            c("ch_abund_crypto_EE", "Cryptosporidium"),       
            c("ch_abund_ascaris_EE",   "Ascaris"),
            c("ch_abund_trichuris_EE",  "Trichuris"),
            c("ch_qpcr_abund_ascaris", "Ascaris"), 
            c("ch_qpcr_abund_trichuris", "Trichuris"))))
rownames(paired_pathogens_cont) <- NULL
colnames(paired_pathogens_cont) <- c("outcome","exposure")


#NOTE: Need to get right covariates including age... make age pathogens above
d$age[!is.na(d$aged_pathogen)] <- d$aged_pathogen[!is.na(d$aged_pathogen)]


#Covariate list
Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#Capone 2022 in prep ,  Fly ,  Ascaris

outcome="ch_pos_campylobacter"
exposure="pos"
study="Capone 2022 in prep"
sample="Fly"
target= "Campylobacter"
family="binomial"
forcedW=c("age", "hhwealth")
Ws=Wvars
minN_thres=minN=0


res <- aim2_glm_pathogens(d, Ws=Wvars, forcedW=forcedW, outcome="ch_pos_campylobacter",
                          exposure="pos", study="Capone 2022 in prep", sample="Fly",
                          target="Campylobacter", family="binomial", minN_thres = 0)
res


d %>% ungroup() %>% filter(target=="Ascaris") %>% droplevels() %>% select(pos, ch_pos_ascaris, study) %>% table

temp =d %>% filter(target=="Ascaris", study == "Capone 2021", !is.na(pos), !is.na(ch_pos_ascaris), sample=="LS")
res <- glm(ch_pos_ascaris~pos, data=temp, family = "binomial")
exp(coef(res)[2])
summary(res)$coefficients[2,4]


#
table(is.na(d$child_date_pathogen), d$ch_pos_path_ecoli, d$study)

df <- d %>% filter(study=={{study}}) %>% droplevels(.)
table(df$target)

table(d$study)

df <- d %>% filter(study=={{study}} ,    target=={{target}}) %>% droplevels(.)
dim(df)

table(df$child_date_pathogen - df$env_date)


#impute missing dates
# table(is.na(d$child_date_pathogen))
# d <- d %>% group_by(study) %>% mutate(child_date_pathogen = ifelse(is.na(child_date_pathogen),mean(child_date_pathogen, na.rm=T),child_date_pathogen))
# table(is.na(d$child_date_pathogen))
# summary(d$child_date_pathogen)
# 
table(d$child_date_pathogen - d$env_date)




# #temp
# res_adj <- d %>% group_by(study, sample) %>%
#   do(res=aim2_glm(., Ws = Wvars, forcedW=NULL, outcome=paired_pathogens$outcome[7], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[7], family="binomial", minN_thres=4))
# 
# df <- d %>% filter(study=="Capone 2021" ,  sample=="LS" ,  target=="Trichuris")
# Ws = Wvars, forcedW=NULL, outcome=paired_pathogens$outcome[7], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[7], family="binomial"

# XXXXXXXXXXXXXX TEMP XXXXXXXXXXXXXXXXXXXXXXX
minN=1
# XXXXXXXXXXXXXX TEMP XXXXXXXXXXXXXXXXXXXXXXX
#minN=5

#----------------------------------------------------------
# unadjusted 
#----------------------------------------------------------
res_full_unadj <- NULL

for(i in 1:nrow(paired_pathogens)){
  res_unadj=NULL
  try(res_unadj <- d %>% group_by(study, sample) %>% do(aim2_glm_pathogens(., Ws = NULL, forcedW=NULL, outcome=paired_pathogens$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[i], family="binomial", minN_thres=minN))) 
  res_full_unadj <- bind_rows(res_full_unadj, res_unadj)
}
res_full_unadj <- res_full_unadj %>% filter(!is.na(RR))
res_full_unadj

#----------------------------------------------------------
# adjusted 
#----------------------------------------------------------
res_full <- NULL

for(i in 1:nrow(paired_pathogens)){
  res_adj=NULL
  try(res_adj <- d %>% group_by(study, sample) %>% do(aim2_glm_pathogens(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome=paired_pathogens$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[i], family="binomial", minN_thres=minN))) 
  res_full <- bind_rows(res_full, res_adj)
}
res_full <- res_full %>% filter(!is.na(RR))



