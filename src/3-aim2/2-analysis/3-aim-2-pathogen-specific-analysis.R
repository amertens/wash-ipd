
rm(list=ls())
source(here::here("0-config.R"))

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

#Capone are generally good, but some samples get dropped

#need to check or median-impute missing pathogen dates
#need to update table to have right numbers and prevalences after dropping dates


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))

# table(d$study, d$ch_pos_cdiff, is.na(d$target))
# df <- d %>% filter(study=="Capone 2021", ch_pos_cdiff==1)
# head(df)

# #drop aggregate values:
d <- d %>% filter(!(grepl("Any ",target) & target != "Any STH"), sample!="any sample type",
                  !(target %in% c("Zoonotic E. coli","Non-zoonotic E. coli")))
table(d$study, d$ch_pos_cdiff)

dim(d)
d <- d %>% filter(!is.na(ch_pos_giardia)|  
                  !is.na(ch_pos_entamoeba)| 
                  !is.na(ch_pos_crypto)| 
                  !is.na(ch_qpcr_pos_ascaris)|   
                  !is.na(ch_qpcr_pos_trichuris)| 
                  !is.na(ch_pos_ascaris)|
                  !is.na(ch_pos_trichuris)|
                  !is.na(ch_pos_giardia_EE)|     
                  !is.na(ch_pos_entamoeba_EE)| 
                  !is.na(ch_pos_crypto_EE)|      
                  !is.na(ch_pos_ascaris_EE)|  
                  !is.na(ch_pos_trichuris_EE)|  
                  !is.na(ch_pos_adenovirus)|   
                  !is.na(ch_pos_norovirus)|   
                  !is.na(ch_pos_rotavirus)|     
                  !is.na(ch_pos_cdiff)|      
                  !is.na(ch_pos_campylobacter)| 
                  !is.na(ch_pos_salmonella)|  
                  !is.na(ch_pos_shigella)|   
                  !is.na(ch_pos_cholera)| 
                  !is.na(ch_pos_yersinia)|     
                  !is.na(ch_pos_path_ecoli)) %>% droplevels()
dim(d)

table(d$study)

#use sth date for Kwong
d$child_date_pathogen[d$study=="Kwong 2021"] <- d$child_date_sth[d$study=="Kwong 2021"]
d$child_date_pathogen[d$study=="Fuhrmeister 2020" & d$target=="Giardia"] <- d$child_date_sth[d$study=="Fuhrmeister 2020" & d$target=="Giardia"]

table(d$study, is.na(d$child_date_pathogen))

d$date_diff <- d$child_date_pathogen - d$env_date

table(is.na(d$date_diff))
d<-d %>% group_by(study, sample, target) %>% mutate(date_diff=ifelse(is.na(date_diff), median(date_diff, na.rm=T), date_diff))
table(is.na(d$date_diff))

table(d$study)
summary(as.numeric(d$date_diff[d$study=="Capone 2021"]))
summary(as.numeric(d$date_diff[d$study=="Capone 2022 in prep"]))

df <- d %>% filter(study=="Fuhrmeister 2020", !is.na(ch_pos_giardia), !is.na(pos), target=="Giardia", sample=="CH")
head(df)
table(is.na(df$date_diff))
summary(as.numeric(df$date_diff))
table(df$pos, 1*df$ch_pos_giardia==1)
df2 <- df %>% filter(date_diff < 124 +15 & date_diff> -15)
table(df2$pos, 1*df2$ch_pos_giardia==1)
table(df$date_diff[df$pos==1 & df$ch_pos_giardia==1])
table(df$date_diff[df$pos==1 & df$ch_pos_giardia==0])
table(df$date_diff[df$pos==0 & df$ch_pos_giardia==0])
table(df$date_diff[df$pos==0 & df$ch_pos_giardia==1])

df <- d %>% filter(study=="Capone 2022 in prep", !is.na(ch_pos_shigella), !is.na(pos), target=="Shigella", sample=="Fly")
head(df)
table(is.na(df$date_diff))
summary(as.numeric(df$date_diff))
table(df$pos, 1*df$ch_pos_shigella==1)
df2 <- df %>% filter(date_diff < 124 +15 & date_diff> -15)
table(df2$pos, 1*df2$ch_pos_shigella==1)
table(df$date_diff[df$pos==1 & df$ch_pos_shigella==1])
table(df$date_diff[df$pos==1 & df$ch_pos_shigella==0])
table(df$date_diff[df$pos==0 & df$ch_pos_shigella==0])
table(df$date_diff[df$pos==0 & df$ch_pos_shigella==1])

capone_upper= 124 +14 #add buffer due to coarsened dates
capone_lower= -14
table(d$pos[d$study=="Capone 2021"],d$ch_pos_norovirus[d$study=="Capone 2021"])
d$ch_pos_shigella[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_salmonella[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_ascaris[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_trichuris[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_giardia[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_norovirus[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_cdiff[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_path_ecoli[d$study=="Capone 2021" & d$date_diff > capone_upper |  d$study=="Capone 2021" &  d$date_diff<capone_lower ] <- NA
table(d$pos[d$study=="Capone 2021"], d$ch_pos_norovirus[d$study=="Capone 2021"])

table(d$ch_pos_shigella[d$study=="Capone 2022 in prep"])
d$ch_pos_shigella[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_salmonella[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_ascaris[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_trichuris[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_giardia[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_norovirus[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_cdiff[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
d$ch_pos_path_ecoli[d$study=="Capone 2022 in prep" & d$date_diff > capone_upper |  d$study=="Capone 2022 in prep" &  d$date_diff<capone_lower ] <- NA
table(d$ch_pos_shigella[d$study=="Capone 2022 in prep"])


table(d$ch_pos_giardia[d$study=="Fuhrmeister 2020"])
d$ch_pos_giardia[d$study=="Fuhrmeister 2020" & d$date_diff > 124 |  d$study=="Fuhrmeister 2020" &  d$date_diff<0 ] <- NA
d$ch_pos_path_ecoli[d$study=="Fuhrmeister 2020" & d$date_diff > 124 |  d$study=="Fuhrmeister 2020" &  d$date_diff<0 ] <- NA
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


outcome="ch_pos_path_ecoli"
exposure="pos"
study="Fuhrmeister 2020"
sample="S"
target= "Ascaris"
family="binomial"
forcedW=c("age", "hhwealth")
Ws=Wvars

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

minN=5
#minN=10

res_full <- NULL
#Don't forc W for Giardia because of convergence errors
# res_adj1 <- d %>% group_by(study, sample) %>%
#   do(aim2_glm(., Ws = Wvars, forcedW=NULL, outcome=paired_pathogens$outcome[1], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[1], family="binomial", minN_thres=minN))
# res_full <- bind_rows(res_full, res_adj1)
for(i in 1:nrow(paired_pathogens)){
  res_adj=NULL
  try(res_adj <- d %>% group_by(study, sample) %>% do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome=paired_pathogens$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[i], family="binomial", minN_thres=minN))) 
  res_full <- bind_rows(res_full, res_adj)
}
res_full <- res_full %>% filter(!is.na(RR))

#

#LS pathogenic E-coli failed to converge. Rerun here
res_adj2 <- d %>% group_by(study, sample) %>%
  do(aim2_glm(., Ws = Wvars[Wvars!="nrooms"], forcedW=NULL, outcome="ch_pos_path_ecoli", exposure="pos", study="Capone 2021", sample=.$sample[1], target="Pathogenic E. coli", family="binomial", minN_thres=minN))
res_adj2 <- res_adj2 %>% filter(!is.na(RR),sample=="LS")

#LS Trichuris failed to converge. Rerun here
res_adj3 <- d %>% group_by(study, sample) %>%
  do(aim2_glm(., Ws = Wvars[Wvars!="walls"], forcedW=NULL, outcome="ch_pos_trichuris", exposure="pos", study="Capone 2021", sample=.$sample[1], target="Trichuris", family="binomial", minN_thres=minN))
res_adj3 <- res_adj3 %>% filter(!is.na(RR),sample=="LS")
res_adj3

#FLY pathogenic E-coli failed to converge. Rerun here
res_adj4 <- d %>% group_by(study, sample) %>%
  do(aim2_glm(., Ws = Wvars[Wvars!="sex"], forcedW=NULL, outcome="ch_pos_path_ecoli", exposure="pos", study="Capone 2022 in prep", sample=.$sample[1], target="Pathogenic E. coli", family="binomial", minN_thres=minN))
res_adj4 <- res_adj4 %>% filter(!is.na(RR),sample=="Fly")



res_full <- res_full %>%
  filter(!(Y=="ch_pos_path_ecoli" & sample=="LS")) %>%
  filter(!(Y=="ch_pos_path_ecoli" & sample=="Fly")) %>%
  filter(!(Y=="ch_pos_trichuris" & sample=="LS"))
res_full <- bind_rows(res_full, res_adj2, res_adj3, res_adj4)

r#es_full <-res_full %>% filter(n>10)
# res_full_cont <- NULL
# for(i in 1:nrow(paired_pathogens_cont)){
#   res_adj_cont <- d %>% group_by(study, sample) %>%
#     do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome=paired_pathogens_cont$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens_cont$exposure[i], family="gaussian", minN_thres=minN)) 
#   res_full_cont <- bind_rows(res_full_cont, res_adj_cont)
# }
# 
# res_full_cont <- res_full_cont %>% filter(!is.na(coef))
# res_full_cont



saveRDS(res_full, file=here("results/pathogen_specific_aim2_res.Rds"))
#saveRDS(res_full_cont, file=here("results/pathogen_specific_aim2_cont_res.Rds"))


res_full_temp <- res_full %>% select(study, Y,sample, target, minN, n, RR, ci.lb, ci.ub, pval) %>%
  filter(sample!="any sample type") %>% mutate(pval=pval<0.05) %>% arrange(study, target) #%>% 
  #filter(!(study=="Kwong 2021" & (Y=="ch_pos_ascaris" | Y=="ch_pos_trichuris")))



# res_full_unadj=NULL
# for(i in 1:nrow(paired_pathogens)){
#   res_unadj <- d %>% group_by(study, sample) %>%
#     do(aim2_glm(., Ws = NULL, outcome=paired_pathogens$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[i], family="binomial", minN_thres=minN)) 
#   res_full_unadj <- bind_rows(res_full_unadj, res_unadj)
# }
# res_full_unadj <- res_full_unadj %>% filter(!is.na(RR)) %>% select(study, Y,sample, target, minN, n, RR, ci.lb, ci.ub, pval) %>%
#   filter(sample!="any sample type") %>% mutate(pval=pval<0.05) %>% arrange(target)
# res_full_unadj



as.data.frame(res_full_temp)
