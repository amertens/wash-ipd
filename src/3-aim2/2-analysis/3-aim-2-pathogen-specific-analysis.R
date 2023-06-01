
rm(list=ls())
source(here::here("0-config.R"))
source(paste0(here::here(),"/src/3-aim2/2-analysis/0-pathogen-function.R"))


#notes on dates
#Kwong measured STH after 2 years, KK sth after 2.5 years. But note the years are coming from the EE taq data so need to use child_date_STH
#Steinbaum also good based on dates in data. Check manuscripts
#Grembi measured after 14 months, but Furhmeister after 2 years
#need to check or median-impute missing pathogen dates

d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))


# #drop aggregate values:
d <- d %>% filter(!(grepl("Any ",target) & target != "Any STH"), sample!="any sample type",
                  !(target %in% c("Zoonotic E. coli","Non-zoonotic E. coli")))
table(d$study, d$ch_pos_cdiff)


#use sth date for Kwong
d$child_date_pathogen[d$study=="Kwong 2021"] <- d$child_date_sth[d$study=="Kwong 2021"]
d$child_date_pathogen[d$study=="Fuhrmeister 2020" & d$target=="Giardia"] <- d$child_date_sth[d$study=="Fuhrmeister 2020" & d$target=="Giardia"]

#Make sure proper time ordering
d$date_diff <- d$child_date_pathogen - d$env_date


table(d$pos[d$study=="Capone 2021"],d$ch_pos_norovirus[d$study=="Capone 2021"])
d$ch_pos_shigella[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
d$ch_pos_salmonella[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
d$ch_pos_ascaris[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
d$ch_pos_trichuris[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
d$ch_pos_giardia[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
d$ch_pos_norovirus[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
d$ch_pos_cdiff[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
d$ch_pos_path_ecoli[d$study=="Capone 2021" & d$date_diff > 124 |  d$study=="Capone 2021" &  d$date_diff<=0 ] <- NA
table(d$pos[d$study=="Capone 2021"], d$ch_pos_norovirus[d$study=="Capone 2021"])

table(d$ch_pos_shigella[d$study=="Capone 2022 in prep"])
d$ch_pos_shigella[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
d$ch_pos_salmonella[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
d$ch_pos_ascaris[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
d$ch_pos_trichuris[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
d$ch_pos_giardia[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
d$ch_pos_norovirus[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
d$ch_pos_cdiff[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
d$ch_pos_path_ecoli[d$study=="Capone 2022 in prep" & d$date_diff > 124 |  d$study=="Capone 2022 in prep" &  d$date_diff<=0 ] <- NA
table(d$ch_pos_shigella[d$study=="Capone 2022 in prep"])

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

d$ch_pos_giardia[d$study=="Fuhrmeister 2020" & d$date_diff > 124 |  d$study=="Fuhrmeister 2020" &  d$date_diff<=0 ] <- NA
d$ch_pos_path_ecoli[d$study=="Fuhrmeister 2020" & d$date_diff > 124 |  d$study=="Fuhrmeister 2020" &  d$date_diff<=0 ] <- NA
d$ch_pos_norovirus[d$study=="Fuhrmeister 2020" & d$date_diff > 124 |  d$study=="Fuhrmeister 2020" &  d$date_diff<=0 ] <- NA
table(d$ch_pos_giardia[d$study=="Fuhrmeister 2020"])


d$ch_qpcr_pos_ascaris[d$study=="Kwong 2021" & d$date_diff > 124 | d$study=="Kwong 2021" &  d$date_diff < 0] <- NA
d$ch_qpcr_pos_trichuris[d$study=="Kwong 2021" & d$date_diff > 124 | d$study=="Kwong 2021" &  d$date_diff < 0] <- NA
d$ch_pos_ascaris[d$study=="Steinbaum 2019" & d$date_diff > 124 | d$study=="Steinbaum 2019" &  d$date_diff < 0] <- NA
d$ch_pos_trichuris[d$study=="Steinbaum 2019" & d$date_diff > 124 | d$study=="Steinbaum 2019" &  d$date_diff < 0] <- NA

#Save data for table 1
head(d)
dim(d)
d <- distinct(d)
dim(d)
saveRDS(d, file=paste0(dropboxDir,"Data/pathogen_analysis_data.rds"))

# #check count for table
# df <- d %>% filter(study=="Fuhrmeister 2020", !is.na(ch_pos_giardia) & target=="Giardia" |!is.na(ch_pos_path_ecoli) & target=="Pathogenic E. coli") #%>% distinct(sampleid, dataid , childid , sample, target, .keep_all = T   )
# head(df)
# length(unique(paste0(df$dataid, "__", df$childid)))
# df2 <- df %>% filter(target=="Giardia", !is.na(ch_pos_giardia))
# table(df2$ch_pos_giardia)
# df3 <- d %>% filter(study=="Fuhrmeister 2020",target=="Pathogenic E. coli", !is.na(ch_pos_path_ecoli))
# df3 <- d %>% filter(study=="Fuhrmeister 2020", !is.na(ch_pos_path_ecoli))
# table(df3$ch_pos_path_ecoli)
# table(df3$target)

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




#Covariate list
d$age[!is.na(d$aged_pathogen)] <- d$aged_pathogen[!is.na(d$aged_pathogen)]
Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#set miniumum
minN=1


#----------------------------------------------------------
# unadjusted 
#----------------------------------------------------------
res_full_unadj <- NULL

for(i in 1:nrow(paired_pathogens)){
  res_unadj=NULL
  try(res_unadj <- d %>% group_by(study, sample) %>% do(aim2_glm_pathogens(., Ws = NULL,  outcome=paired_pathogens$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[i], family="binomial", minN_thres=minN))) 
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
  try(res_adj <- d %>% group_by(study, sample) %>% do(aim2_glm_pathogens(., Ws = Wvars,  outcome=paired_pathogens$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[i], family="binomial", minN_thres=minN))) 
  res_full <- bind_rows(res_full, res_adj)
}
res_full <- res_full %>% filter(!is.na(RR))




saveRDS(res_full_unadj, file=here("results/pathogen_specific_aim2_res_unadj.Rds"))
saveRDS(res_full, file=here("results/pathogen_specific_aim2_res.Rds"))


res_full
res_full_unadj
