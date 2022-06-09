

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

colnames(d)
dY_path <- d %>% 
  group_by(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid) %>%
  distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, ch_pos_giardia,  
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
         ch_pos_path_ecoli) %>%
  mutate(N=n(), path_infections=sum(ch_pos_giardia,  
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
                             ch_pos_path_ecoli, na.rm=T),
         path_obs=sum(!is.na(ch_pos_giardia),  
                      !is.na(ch_pos_entamoeba), 
                      !is.na(ch_pos_crypto), 
                      !is.na(ch_qpcr_pos_ascaris),   
                      !is.na(ch_qpcr_pos_trichuris), 
                      !is.na(ch_pos_ascaris),   
                      !is.na(ch_pos_trichuris),  
                      !is.na(ch_pos_giardia_EE),     
                      !is.na(ch_pos_entamoeba_EE), 
                      !is.na(ch_pos_crypto_EE),      
                      !is.na(ch_pos_ascaris_EE),  
                      !is.na(ch_pos_trichuris_EE),  
                      !is.na(ch_pos_adenovirus),   
                      !is.na(ch_pos_norovirus),   
                      !is.na(ch_pos_rotavirus),     
                      !is.na(ch_pos_cdiff),      
                      !is.na(ch_pos_campylobacter), 
                      !is.na(ch_pos_salmonella),  
                      !is.na(ch_pos_shigella),   
                      !is.na(ch_pos_cholera), 
                      !is.na(ch_pos_yersinia),     
                      !is.na(ch_pos_path_ecoli), na.rm=T)) %>%
  select(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, ch_pos_giardia,  
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
           ch_pos_path_ecoli, N,
           path_infections, path_obs) %>% filter(!is.na(path_infections), path_obs>0)

df <- dY_path %>% filter(ch_pos_ascaris_EE==1 & ch_pos_ascaris==1)
df
table(d$ch_pos_ascaris_EE, d$ch_pos_ascaris)

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

dY_path_Holcomb <- dY_path %>% filter(study=="Holcomb 2021")
table(dY_path_Holcomb$path_infections)


dY_diar <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, diar7d) %>% filter(!is.na(diar7d))
dY_haz <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, haz) %>% filter(!is.na(haz))
dY_waz <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, waz) %>% filter(!is.na(waz))
dY_whz <- d %>% distinct(study, trial, dataid, hhid, clusterid, child_date, agedays, sex, childid, whz) %>% filter(!is.na(whz))
dim(dY_diar)
dim(dY_haz)

tab_path <- dY_path %>% group_by(trial, study) %>%
  summarise(N_paths=max(path_obs, na.rm=T), N_path_obs=sum(path_infections, na.rm=T),N_path_obs=n(), N_path_cases=sum(path_infections, na.rm=T), prev_path=round(mean(path_infections>0, na.rm=T)*100, 1))


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
tab_Y <- left_join(tab_Y, tab_path, by =c("trial","study")) 

colnames(tab_Y)
N_cols <- ncol(tab_Y)
tab_Y <- tab_Y[,c(2,1,(N_cols-3):N_cols,3:(N_cols-4))]
colnames(tab_Y) <- c( "Study","Trial","Distinct pathogens measured","# pathogen obs.","# pathogen cases","Pathogen prev.","# diarrhea obs.","# diarrhea cases","Diarrhea prev.","# HAZ obs.",
                      "Mean HAZ","Stunting prev.", "# WAZ obs.","Mean WAZ","Underweight prev.","# WHZ obs.",
                      "Mean WHZ","Wasting prev." )

save(tab_Y,
     file=here("figures/aim2_all_tables.Rdata"))









