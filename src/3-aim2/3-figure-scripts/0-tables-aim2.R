

rm(list=ls())
source(here::here("0-config.R"))
library(table1)
library(rvest)


#Clean results function
clean_est <- function(res, outcome="binomial"){
  if(outcome=="binomial"){
    est = round(res$RR, 2)
    ci.lb = round(res$ci.lb, 2) 
    ci.ub = round(res$ci.ub, 2)
    est.ci = paste0(est," (",ci.lb,", ",ci.ub,")")
  }else{
    est = round(res$coef, 2)
    ci.lb = round(res$ci.lb, 2) 
    ci.ub = round(res$ci.ub, 2)
    est.ci = paste0("difference= ",est," (95% CI: ",ci.lb,", ",ci.ub,")")
  }
  return(est.ci)
}



d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data.rds"))
path <- readRDS(paste0(dropboxDir,"Data/pathogen_analysis_data.rds"))

d <- d %>% mutate(
  sample =case_when(
              sample == "any sample type" ~ "Any sample",
              sample == "SW" ~ "Source water",
              sample == "W" ~ "Stored water",
              sample == "CH" ~ "Child hands",
              sample == "MH" ~ "Mother's hands",
              sample == "FlyKitch" ~ "Flies in kitchen",
              sample == "FlyLat" ~ "Flies in latrine",
              sample == "LS" ~ "Latrine soil",
              sample == "S" ~ "House soil"
            ), 
              sample = factor(sample, 
                                levels=c("Any sample","Source water","Stored water",
                                         "Child hands", "Mother's hands", "Latrine soil",
                                         "House soil", "Flies in kitchen",  "Flies in latrine", "Sparse data"))) %>%
            droplevels()







#table1
tab1 <- table1(~target+sample |study, format_number = TRUE, data=d)




#covariate table
Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")         
Wvars[!(Wvars %in% colnames(d))]
df <- d %>% subset(., select = c("study", Wvars)) %>% filter(study!="Odagiri 2016")
#harmonize covariates


#rename covariates
df <- df %>% rename(
  `Household\nwealth`=hhwealth, 
  `Number of people\nin the household`=Nhh,
  `Number of rooms\nin the household`=nrooms,
  `Improved wall`=walls, 
  `Improved floor`=floor,
  `Improved roof`=roof,
  `Electricity`=elec,
  `Father in\nagriculture`=dadagri,
  `Land owned`=landown, 
  `Acres of\nland owned`=landacre, 
  `Maternal\neducation`=momedu, 
  `Maternal\nage`=momage)

tab2 <- table1(~. |study, format_number = TRUE, data=df)
tab2 <- as.data.frame(read_html(tab2) %>% html_table(fill=TRUE))
#Drop overall column
tab2 <- tab2[,!grepl("Overall",colnames(tab2))]
tab2 <- tab2[-c(1:8),]
colnames(tab2) <- paste0(str_split(colnames(tab2),"\\.", simplify = T)[,1]," ", str_split(colnames(tab2),"\\.", simplify = T)[,2])
colnames(tab2)[1] <- "."

#Prevalence and abundance of outcomes by sample sample
df <- d %>% group_by(study, target, sample) %>% filter(!is.na(pos)) %>%
  summarize(N=sum(!is.na(pos)), prev=mean(pos, na.rm=T), abund=mean(abund, na.rm=T)) %>%
  mutate(prev=round(prev,1), abund=round(abund,1))
df2 <- d %>% group_by(study,round, target, sample) %>% filter(!is.na(pos)) %>%
  summarize(N=sum(!is.na(pos)), prev=mean(pos, na.rm=T), abund=mean(abund, na.rm=T)) %>%
  mutate(prev=round(prev,1), abund=round(abund,1))

unique(df$sample)
unique(df$target)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Get outcome N's and means by study
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# dY_path <- path %>% 
#   filter(
#     !is.na(ch_pos_giardia) & target==" Giardia"|
#       !is.na(ch_pos_entamoeba) & target==" Entamoeba histolytica"|
#       !is.na(ch_pos_crypto) & target==" Cryptosporidium"|
#       !is.na(ch_qpcr_pos_ascaris) & target==" Ascaris"|
#       !is.na(ch_qpcr_pos_trichuris) & target==" Trichuris"|
#       !is.na(ch_pos_ascaris) & target==" Ascaris"|
#       !is.na(ch_pos_trichuris) & target=="Trichuris"|
#       !is.na(ch_pos_giardia_EE) & target==" Giardia"|
#       !is.na(ch_pos_entamoeba_EE) & target==" Entamoeba histolytica"|
#       !is.na(ch_pos_crypto_EE) & target==" Cryptosporidium"|
#       !is.na(ch_pos_ascaris_EE) & target==" Ascaris"|
#       !is.na(ch_pos_trichuris_EE) & target=="Trichuris"|
#       !is.na(ch_pos_adenovirus) & target==" Adenovirus"|
#       !is.na(ch_pos_norovirus) & target==" Norovirus"|
#       !is.na(ch_pos_rotavirus) & target=="Rotavirus"|
#       !is.na(ch_pos_cdiff) & target=="C. difficile"|
#       !is.na(ch_pos_campylobacter) & target=="Campylobacter"|
#       !is.na(ch_pos_salmonella) & target=="Salmonella"|
#       !is.na(ch_pos_shigella) & target==" Shigella"|
#       !is.na(ch_pos_cholera) & target==" V. cholerae"|
#       !is.na(ch_pos_yersinia) & target==" Yersinia"|
#       !is.na(ch_pos_path_ecoli) & target==" Pathogenic E. coli") %>%
#   group_by(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid) %>%
#   distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, target, ch_pos_giardia,  
#          ch_pos_entamoeba, 
#          ch_pos_crypto, 
#          ch_qpcr_pos_ascaris,   
#          ch_qpcr_pos_trichuris, 
#          ch_pos_ascaris,   
#          ch_pos_trichuris,  
#          ch_pos_giardia_EE,     
#          ch_pos_entamoeba_EE, 
#          ch_pos_crypto_EE,      
#          ch_pos_ascaris_EE,  
#          ch_pos_trichuris_EE,  
#          ch_pos_adenovirus,   
#          ch_pos_norovirus,   
#          ch_pos_rotavirus,     
#          ch_pos_cdiff,      
#          ch_pos_campylobacter, 
#          ch_pos_salmonella,  
#          ch_pos_shigella,   
#          ch_pos_cholera, 
#          ch_pos_yersinia,     
#          ch_pos_path_ecoli) %>%
#   mutate(pos_path= ifelse(
#            ch_pos_giardia==1 & target=="Giardia"|
#            ch_pos_entamoeba==1 & target=="Entamoeba histolytica"|
#            ch_pos_crypto==1 & target=="Cryptosporidium"|
#            ch_qpcr_pos_ascaris==1 & target=="Ascaris"|
#            ch_qpcr_pos_trichuris==1 & target=="Trichuris"|
#            ch_pos_ascaris==1 & target=="Ascaris"|
#            ch_pos_trichuris==1 & target=="Trichuris"|
#            ch_pos_giardia_EE==1 & target=="Giardia"|
#            ch_pos_entamoeba_EE==1 & target=="Entamoeba histolytica"|
#            ch_pos_crypto_EE==1 & target=="Cryptosporidium"|
#            ch_pos_ascaris_EE==1 & target=="Ascaris"|
#            ch_pos_trichuris_EE==1 & target=="Trichuris"|
#            ch_pos_adenovirus==1 & target=="Adenovirus"|
#            ch_pos_norovirus==1 & target=="Norovirus"|
#            ch_pos_rotavirus==1 & target=="Rotavirus"|
#            ch_pos_cdiff==1 & target=="C. difficile"|
#            ch_pos_campylobacter==1 & target=="Campylobacter"|
#            ch_pos_salmonella==1 & target=="Salmonella"|
#            ch_pos_shigella==1 & target=="Shigella"|
#            ch_pos_cholera==1 & target=="V. cholerae"|
#            ch_pos_yersinia==1 & target=="Yersinia"|
#            ch_pos_path_ecoli==1 & target=="Pathogenic E. coli",1,0),
#          pos_path= ifelse(
#            is.na(ch_pos_giardia) & is.na(ch_pos_giardia_EE) & target=="Giardia"|
#              is.na(ch_pos_entamoeba) & target=="Entamoeba histolytica"|
#              is.na(ch_pos_crypto) & target=="Cryptosporidium"|
#              is.na(ch_qpcr_pos_ascaris) & is.na(ch_pos_ascaris) & is.na(ch_pos_ascaris_EE) & target=="Ascaris"|
#              is.na(ch_qpcr_pos_trichuris) & is.na(ch_pos_trichuris) & is.na(ch_pos_trichuris_EE) & target=="Trichuris"|
#              is.na(ch_pos_entamoeba_EE) & target=="Entamoeba histolytica"|
#              is.na(ch_pos_crypto_EE) & target=="Cryptosporidium"|
#              is.na(ch_pos_adenovirus) & target=="Adenovirus"|
#              is.na(ch_pos_norovirus) & target=="Norovirus"|
#              is.na(ch_pos_rotavirus) & target=="Rotavirus"|
#              is.na(ch_pos_cdiff) & target=="C. difficile"|
#              is.na(ch_pos_campylobacter) & target=="Campylobacter"|
#              is.na(ch_pos_salmonella) & target=="Salmonella"|
#              is.na(ch_pos_shigella) & target=="Shigella"|
#              is.na(ch_pos_cholera) & target=="V. cholerae"|
#              is.na(ch_pos_yersinia) & target=="Yersinia"|
#              is.na(ch_pos_path_ecoli) & target=="Pathogenic E. coli",NA_integer_,pos_path)) %>%
#    mutate(N=n(), path_infections=sum(pos_path), path_obs=sum(!is.na(pos_path))) %>%  
#   # mutate(N=n(), path_infections=sum(ch_pos_giardia,  
#   #                            ch_pos_entamoeba, 
#   #                            ch_pos_crypto, 
#   #                            ch_qpcr_pos_ascaris,   
#   #                            ch_qpcr_pos_trichuris, 
#   #                            ch_pos_ascaris,   
#   #                            ch_pos_trichuris,  
#   #                            ch_pos_giardia_EE,     
#   #                            ch_pos_entamoeba_EE, 
#   #                            ch_pos_crypto_EE,      
#   #                            ch_pos_ascaris_EE,  
#   #                            ch_pos_trichuris_EE,  
#   #                            ch_pos_adenovirus,   
#   #                            ch_pos_norovirus,   
#   #                            ch_pos_rotavirus,     
#   #                            ch_pos_cdiff,      
#   #                            ch_pos_campylobacter, 
#   #                            ch_pos_salmonella,  
#   #                            ch_pos_shigella,   
#   #                            ch_pos_cholera, 
#   #                            ch_pos_yersinia,     
#   #                            ch_pos_path_ecoli, na.rm=T),
#          # path_obs=sum(!is.na(ch_pos_giardia),  
#          #              !is.na(ch_pos_entamoeba), 
#          #              !is.na(ch_pos_crypto), 
#          #              !is.na(ch_qpcr_pos_ascaris),   
#          #              !is.na(ch_qpcr_pos_trichuris), 
#          #              !is.na(ch_pos_ascaris),   
#          #              !is.na(ch_pos_trichuris),  
#          #              !is.na(ch_pos_giardia_EE),     
#          #              !is.na(ch_pos_entamoeba_EE), 
#          #              !is.na(ch_pos_crypto_EE),      
#          #              !is.na(ch_pos_ascaris_EE),  
#          #              !is.na(ch_pos_trichuris_EE),  
#          #              !is.na(ch_pos_adenovirus),   
#          #              !is.na(ch_pos_norovirus),   
#          #              !is.na(ch_pos_rotavirus),     
#          #              !is.na(ch_pos_cdiff),      
#          #              !is.na(ch_pos_campylobacter), 
#          #              !is.na(ch_pos_salmonella),  
#          #              !is.na(ch_pos_shigella),   
#          #              !is.na(ch_pos_cholera), 
#          #              !is.na(ch_pos_yersinia),     
#          #              !is.na(ch_pos_path_ecoli), na.rm=T)
# #) %>%
#   select(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, target,
#            ch_pos_giardia,  
#            ch_pos_entamoeba, 
#            ch_pos_crypto, 
#            ch_qpcr_pos_ascaris,   
#            ch_qpcr_pos_trichuris, 
#            ch_pos_ascaris,   
#            ch_pos_trichuris,  
#            ch_pos_giardia_EE,     
#            ch_pos_entamoeba_EE, 
#            ch_pos_crypto_EE,      
#            ch_pos_ascaris_EE,  
#            ch_pos_trichuris_EE,  
#            ch_pos_adenovirus,   
#            ch_pos_norovirus,   
#            ch_pos_rotavirus,     
#            ch_pos_cdiff,      
#            ch_pos_campylobacter, 
#            ch_pos_salmonella,  
#            ch_pos_shigella,   
#            ch_pos_cholera, 
#            ch_pos_yersinia,     
#            ch_pos_path_ecoli, N,
#            path_infections, path_obs) %>% filter(!is.na(path_infections), path_obs>0)
# 
# dim(dY_path)
# dim(path %>% filter(!is.na(path_infections)) %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid))
# 
# #get distinct pathogens
# path_distinct <- dY_path %>%  group_by(study, trial) %>%
#   summarise( N_paths=  
#                1*(sum(ch_pos_giardia, na.rm=T)>0& target=="Giardia") +  
#                1*(sum(ch_pos_entamoeba,na.rm=T)>0& target=="Entamoeba histolytica") + 
#                1*(sum(ch_pos_crypto,na.rm=T)>0& target=="Cryptosporidium") + 
#                1*(sum(ch_qpcr_pos_ascaris,na.rm=T)>0& target=="Ascaris") +   
#                1*(sum(ch_qpcr_pos_trichuris,na.rm=T)>0& target=="Trichuris") + 
#                1*(sum(ch_pos_ascaris,na.rm=T)>0& target=="Ascaris") +   
#                1*(sum(ch_pos_trichuris,na.rm=T)>0& target=="Trichuris") +  
#                1*(sum(ch_pos_giardia_EE,na.rm=T)>0& target=="Giardia") +     
#                1*(sum(ch_pos_entamoeba_EE,na.rm=T)>0& target=="Entamoeba histolytica") + 
#                1*(sum(ch_pos_crypto_EE,na.rm=T)>0& target=="Cryptosporidium") +      
#                1*(sum(ch_pos_ascaris_EE,na.rm=T)>0& target=="Ascaris") +  
#                1*(sum(ch_pos_trichuris_EE,na.rm=T)>0& target=="Trichuris") +  
#                1*(sum(ch_pos_adenovirus,na.rm=T)>0& target=="Adenovirus") +   
#                1*(sum(ch_pos_norovirus,na.rm=T)>0& target=="Norovirus") +   
#                1*(sum(ch_pos_rotavirus,na.rm=T)>0& target=="Rotavirus") +     
#                1*(sum(ch_pos_cdiff,na.rm=T)>0& target=="C. difficile") +      
#                1*(sum(ch_pos_campylobacter,na.rm=T)>0& target=="Campylobacter") + 
#                1*(sum(ch_pos_salmonella,na.rm=T)>0& target=="Salmonella") +  
#                1*(sum(ch_pos_shigella,na.rm=T)>0& target=="Shigella") +   
#                1*(sum(ch_pos_cholera,na.rm=T)>0& target=="V. cholerae") + 
#                1*(sum(ch_pos_yersinia,na.rm=T)>0& target=="Yersinia") +     
#                1*(sum(ch_pos_path_ecoli,na.rm=T)>0& target=="Pathogenic E. coli")) %>% 
#   distinct()


dY_path <- path %>% subset(., select=c(study, dataid, childid, target, pos,
                                            ch_pos_giardia,  
                                            ch_pos_entamoeba, 
                                            ch_pos_crypto, 
                                            ch_qpcr_pos_ascaris,   
                                            ch_qpcr_pos_trichuris, 
                                            ch_pos_ascaris,   
                                            ch_pos_trichuris,  
                                            ch_pos_giardia_EE,     
                                            ch_pos_entamoeba_EE, 
                                            ch_pos_crypto_EE,      
                                            ch_pos_ascaris_EE,  
                                            ch_pos_trichuris_EE,  
                                            ch_pos_adenovirus,   
                                            ch_pos_norovirus,   
                                            ch_pos_rotavirus,     
                                            ch_pos_cdiff,      
                                            ch_pos_campylobacter, 
                                            ch_pos_salmonella,  
                                            ch_pos_shigella,   
                                            ch_pos_cholera, 
                                            ch_pos_yersinia,     
                                            ch_pos_path_ecoli)) %>%
  gather( "ch_target","ch_pos",6:27) %>% filter(!is.na(ch_pos)) %>%
  filter(  ch_target=="ch_pos_giardia" & target=="Giardia"|
             ch_target=="ch_pos_entamoeba"& target=="Entamoeba histolytica"|
             ch_target=="ch_pos_crypto"& target=="Cryptosporidium"|
             # ch_target=="ch_qpcr_pos_ascaris"& target=="Ascaris"|
             # ch_target=="ch_qpcr_pos_trichuris"& target=="Trichuris"|
             ch_target=="ch_pos_ascaris"& target=="Ascaris"|
             ch_target=="ch_pos_trichuris"& target=="Trichuris"|
             # ch_target=="ch_pos_giardia_EE"& target=="Giardia"|
             # ch_target=="ch_pos_entamoeba_EE"& target=="Entamoeba histolytica"|
             # ch_target=="ch_pos_crypto_EE"& target=="Cryptosporidium"|
             # ch_target=="ch_pos_ascaris_EE"& target=="Ascaris"|
             # ch_target=="ch_pos_trichuris_EE"& target=="Trichuris"|
             ch_target=="ch_pos_adenovirus"& target=="Adenovirus"|
             ch_target=="ch_pos_norovirus"& target=="Norovirus"|
             ch_target=="ch_pos_rotavirus"& target=="Rotavirus"|
             ch_target=="ch_pos_cdiff"& target=="C. difficile"|
             ch_target=="ch_pos_campylobacter"& target=="Campylobacter"|
             ch_target=="ch_pos_salmonella"& target=="Salmonella"|
             ch_target=="ch_pos_shigella"& target=="Shigella"|
             ch_target=="ch_pos_cholera"& target=="V. cholerae"|
             ch_target=="ch_pos_yersinia"& target=="Yersinia"|
             ch_target=="ch_pos_path_ecoli"& target=="Pathogenic E. coli")

head(dY_path)

tab_path <- dY_path %>% filter(study!="Boehm 2016") %>% #drop because 6 children
  group_by(study,dataid, childid) %>%
  mutate(prev_path=1*(sum(ch_pos)>0)) %>%
  group_by(study) %>%
  summarise(
    N_paths=length(unique(ch_target)), 
    #N_path_obs=n(), 
    N_path_children=length(unique(paste0(dataid, "__", childid))), 
    N_path_cases=sum(ch_pos, na.rm=T), 
    prev_path=round(mean(prev_path, na.rm=T)*100, 1))




dY_path_kwong <- dY_path %>% filter(study=="Kwong 2021")
dY_path_Boehm <- dY_path %>% filter(study=="Boehm 2016")

dY_path_Holcomb <- dY_path %>% filter(study=="Holcomb 2021")
table(dY_path_Holcomb$path_infections)


dY_diar <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, diar7d) %>% filter(!is.na(diar7d))
dY_haz <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, haz) %>% filter(!is.na(haz))
dY_waz <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, waz) %>% filter(!is.na(waz))
dY_whz <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, whz) %>% filter(!is.na(whz))
dim(dY_diar)
dim(dY_haz)

# tab_path <- dY_path %>% group_by(trial, study) %>%
#   summarise(#N_paths=max(path_obs, na.rm=T), 
#             N_path_obs=sum(!is.na(path_infections), na.rm=T), N_path_cases=sum(path_infections, na.rm=T), prev_path=round(mean(path_infections>0, na.rm=T)*100, 1))


tab_diar <- dY_diar %>% group_by(trial, study) %>%
  summarise(N_diar_obs=n(), N_diar_cases=sum(diar7d, na.rm=T), prev_diar=round(mean(diar7d, na.rm=T)*100, 1))

tab_haz <- dY_haz %>% group_by(trial, study) %>%
  summarise(N_haz=n(), mean_haz=round(mean(haz, na.rm=T), 2), prev_stunting=round(mean(haz < (-2), na.rm=T)*100, 1))
tab_waz <- dY_waz %>% group_by(trial, study) %>%
  summarise(N_waz=n(), mean_waz=round(mean(waz, na.rm=T), 2), prev_underwt=round(mean(waz < (-2), na.rm=T)*100, 1))
tab_whz <- dY_whz %>% group_by(trial, study) %>%
  summarise(N_whz=n(), mean_whz=round(mean(whz, na.rm=T), 2), prev_wasting=round(mean(whz < (-2), na.rm=T)*100, 1))

tab_Y <- left_join(tab_diar, tab_haz, by =c("trial","study")) 
tab_Y <- left_join(tab_Y, tab_waz, by =c("trial","study")) 
tab_Y <- left_join(tab_Y, tab_whz, by =c("trial","study")) 
tab_Y <- left_join(tab_Y, tab_path, by =c("study")) 

colnames(tab_Y)
N_cols <- ncol(tab_Y)
tab_Y <- tab_Y[,c(2,1,(N_cols-3):(N_cols),3:(N_cols-4))]
colnames(tab_Y) <- c( "Study","Trial","Distinct pathogens measured","# children with pathogens measured","# pathogen infections","Pathogen prev.","# diarrhea obs.","# diarrhea cases","Diarrhea prev.","# HAZ obs.",
                      "Mean HAZ","Stunting prev.", "# WAZ obs.","Mean WAZ","Underweight prev.","# WHZ obs.",
                      "Mean WHZ","Wasting prev." )

save(tab_Y,
     file=here("figures/aim2_all_tables.Rdata"))









