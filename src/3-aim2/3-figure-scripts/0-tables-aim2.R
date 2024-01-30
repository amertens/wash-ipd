

rm(list=ls())
source(here::here("0-config.R"))
library(table1)
library(rvest)


#Clean results function
clean_est <- function(res, outcome="binomial"){
  if(outcome=="binomial"){
    est = sprintf("%.2f",res$RR)
    ci.lb = sprintf("%.2f",res$ci.lb) 
    ci.ub = sprintf("%.2f",res$ci.ub)
    est.ci = paste0(est," (",ci.lb,", ",ci.ub,")")
  }else{
    est = sprintf("%.2f",res$coef)
    ci.lb = sprintf("%.2f",res$ci.lb) 
    ci.ub = sprintf("%.2f",res$ci.ub)
    est.ci = paste0("difference= ",est," (95% CI: ",ci.lb,", ",ci.ub,")")
  }
  return(est.ci)
}



d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
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




#Add sex breakdown to table 1 per journal guidelines
d <- d %>% ungroup() %>%
  rename(sex_factor=sex) %>%
  mutate(
    sex = case_when(
      sex_factor=="female" ~0,
      sex_factor=="male" ~1,
      sex_factor=="Female" ~0,
      sex_factor=="Male" ~1,
      sex_factor=="1" & study %in% c("Holcomb 2021","Capone 2022 in prep","Capone 2021") ~0,
      sex_factor=="0" & study %in% c("Holcomb 2021","Capone 2022 in prep","Capone 2021") ~1,
      sex_factor=="1" & study %in% c("Reese 2017","Odagiri 2016") ~1,
      sex_factor=="2" & study %in% c("Reese 2017","Odagiri 2016") ~0)
  )

head(d)


#df <- d %>% filter(!is.na(haz)|!is.na(waz)|!is.na(diar7d), !is.na(pos))
#df <- d %>% filter(study=="Steinbaum 2019")

#get breakdown by N child
df_N <- d %>% ungroup() %>%
  filter(!is.na(age)|!is.na(age_anthro), !is.na(pos)) %>%
  distinct(study, dataid, childid, childNo, sex) %>%
  group_by(study) %>%
  summarize(N=n()) %>%
  select(study, N)
df_N

temp <- d %>% filter(study=="Steinbaum 2019", sample=="any sample type", target=="Any pathogen") %>% arrange(dataid, childid, childNo, sex)

#get breakdown by sex
df_sex <- d %>% ungroup() %>%
  filter(!is.na(age)|!is.na(age_anthro), !is.na(pos)) %>%
  distinct(study, dataid, childid, childNo, sex) %>%
  group_by(study) %>%
  mutate(perc_male=mean(sex))  %>% 
  group_by(study,sex) %>%
  summarize(N=n(), perc_female=sprintf("%.1f", (1-perc_male[1])*100), perc_male=sprintf("%.1f", perc_male[1]*100)) %>%
  mutate(sex_N_f = ifelse(sex==1, paste0(N," (",perc_male,"%)"), paste0(N," (",perc_female,"%)")),
         sex=case_when(sex==1 ~"Male",
                       sex==0 ~"Female")) %>%
  select(study, sex, sex_N_f)
df_sex


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
table(path$diar7d)
table(is.na(path$diar7d))
dY_path <- path %>% filter(!is.na(pos)) %>%
  subset(., select=c(study, env_date, child_date,
                                            dataid, childid, target, pos,
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
  gather( "ch_target","ch_pos",8:29) %>% filter(!is.na(ch_pos)) %>%
  filter(  ch_target=="ch_pos_giardia" & target=="Giardia"|
             ch_target=="ch_pos_entamoeba"& target=="Entamoeba histolytica"|
             ch_target=="ch_pos_crypto"& target=="Cryptosporidium"|
             ch_target=="ch_qpcr_pos_ascaris"& target=="Ascaris" & study=="Kwong 2021" |
             ch_target=="ch_qpcr_pos_trichuris"& target=="Trichuris" & study=="Kwong 2021" |
             ch_target=="ch_pos_ascaris"& target=="Ascaris" & study!="Kwong 2021" |
             ch_target=="ch_pos_trichuris"& target=="Trichuris" & study!="Kwong 2021" |
             ch_target=="ch_pos_adenovirus"& target=="Adenovirus"|
             ch_target=="ch_pos_norovirus"& target=="Norovirus"|
             ch_target=="ch_pos_rotavirus"& target=="Rotavirus"|
             ch_target=="ch_pos_cdiff"& target=="C. difficile"|
             ch_target=="ch_pos_campylobacter"& target=="Campylobacter"|
             ch_target=="ch_pos_salmonella"& target=="Salmonella"|
             ch_target=="ch_pos_shigella"& target=="Shigella"|
             ch_target=="ch_pos_cholera"& target=="V. cholerae"|
             ch_target=="ch_pos_yersinia"& target=="Yersinia"|
             ch_target=="ch_pos_path_ecoli"& target=="Pathogenic E. coli") %>% droplevels()

head(dY_path)

tab_path <- dY_path %>% filter(study!="Boehm 2016") %>% #drop because 6 children
  group_by(study,dataid, childid) %>% distinct(.) %>% 
  mutate(prev_path=1*(sum(ch_pos)>0)) %>%
  group_by(study) %>%
  summarise(
    N_paths=length(unique(ch_target)), 
    N_path_children=length(unique(paste0(dataid, "__", childid))), 
    N_path_cases=sum(ch_pos, na.rm=T), 
    prev_path=round(mean(prev_path, na.rm=T)*100, 1))
tab_path <- left_join(tab_path, df_N, by="study") %>% mutate(perc_children=sprintf("%.1f", (N_path_children / N)*100))
tab_path

dY_path_kwong <- dY_path %>% filter(study=="Kwong 2021")
dY_path_Boehm <- dY_path %>% filter(study=="Boehm 2016")
dY_path_Holcomb <- dY_path %>% filter(study=="Holcomb 2021")
table(dY_path_Holcomb$path_infections)



#ensure time ordering of diarrhea (anthro has been set in individual studies)
d_diar <- d
d_diar$diar7d[d_diar$child_date <= d_diar$env_date | d_diar$child_date > d_diar$env_date+124] <- NA
#dY_diar <- d_diar %>% ungroup() %>% filter(!is.na(diar7d) & !is.na(age))%>% distinct(study, trial, dataid, hhid, clusterid, childid, diar7d) 
dY_diar <- d_diar %>% ungroup() %>% filter(!is.na(diar7d) & !is.na(age) & !is.na(pos), !is.na(env_date))%>% 
distinct(study, trial, dataid, hhid, clusterid, childid, diar7d, child_date) %>%
  arrange(study, trial, dataid, hhid, clusterid, childid, diar7d, child_date)

tab_diar <- dY_diar %>% group_by(trial, study) %>% 
  summarise(N_diar_obs=n(), N_diar_cases=sum(diar7d, na.rm=T), prev_diar=sprintf("%.1f",mean(diar7d, na.rm=T)*100))
tab_diar

#distinct growth measures
dY_haz <- d %>% ungroup() %>%filter(!is.na(haz) & !is.na(age_anthro), !is.na(env_date)) %>% distinct(study, trial, dataid, hhid, clusterid,  childid, haz, child_date_anthro) %>%
  arrange(study, trial, dataid, hhid, clusterid,  childid, haz, child_date_anthro)
dY_waz <- d %>% ungroup() %>%filter(!is.na(waz) & !is.na(age_anthro)) %>% distinct(study, trial, dataid, hhid, clusterid,  childid, waz) 
dY_whz <- d %>% ungroup() %>%filter(!is.na(whz) & !is.na(age_anthro)) %>% distinct(study, trial, dataid, hhid, clusterid,  childid, whz) 




tab_haz <- dY_haz %>% group_by(trial, study) %>% 
  summarise(N_haz=n(), mean_haz=sprintf("%.2f",mean(haz, na.rm=T)), sd_haz=sprintf("%.2f",sd(haz, na.rm=T)), prev_stunting=sprintf("%.1f",mean(haz < (-2), na.rm=T)*100, 1), n_stunting=sum(haz < (-2), na.rm=T))
tab_waz <- dY_waz %>% group_by(trial, study) %>% 
  summarise(N_waz=n(), mean_waz=sprintf("%.2f",mean(waz, na.rm=T)), sd_waz=sprintf("%.2f",sd(waz, na.rm=T)), prev_underwt=sprintf("%.1f",mean(waz < (-2), na.rm=T)*100, 1), n_underwt=sum(waz < (-2), na.rm=T))
tab_whz <- dY_whz %>% group_by(trial, study) %>% 
  summarise(N_whz=n(), mean_whz=sprintf("%.2f",mean(whz, na.rm=T)), sd_whz=sprintf("%.2f",sd(whz, na.rm=T)), prev_wasting=sprintf("%.1f",mean(whz < (-2), na.rm=T)*100, 1), n_wasting=sum(whz < (-2), na.rm=T))

tab_Y <- left_join(tab_diar, tab_haz, by =c("trial","study")) 
tab_Y <- left_join(tab_Y, tab_waz, by =c("trial","study")) 
tab_Y <- left_join(tab_Y, tab_whz, by =c("trial","study")) 
tab_Y <- left_join(tab_Y, tab_path, by =c("study")) 

#Save for transposed format
tab_Y_transpose <- tab_Y

colnames(tab_Y)
N_cols <- ncol(tab_Y)
tab_Y <- tab_Y[,c(2,1,(N_cols-3):(N_cols),3:(N_cols-4))]
colnames(tab_Y) <- c( "Study","Trial","Distinct pathogens measured","# children with pathogens measured","# pathogen infections","Pathogen prev.","# diarrhea obs.","# diarrhea cases","Diarrhea prev.","# HAZ obs.",
                      "Mean HAZ","Stunting prev.", "# WAZ obs.","Mean WAZ","Underweight prev.","# WHZ obs.",
                      "Mean WHZ","Wasting prev." )

save(tab_Y,
     file=here("figures/aim2_all_tables.Rdata"))



#
tab_Y_transpose

#paste0 columns
colnames(tab_Y_transpose)
tab_Y_transpose <- tab_Y_transpose %>% 
  mutate(`# children with pathogens measured` = N_path_children,
         `Pathogen prevalence (n/N)` = paste0(prev_path," (",N_path_cases,")"),
         `Diarrhea prevalence (n/N)` = paste0(prev_diar," (",N_diar_cases,"/",N_diar_obs,")"),
         `Mean HAZ` = paste0(mean_haz," (",sd_haz,")"),
         `Stunting prevalence (n/N)` = paste0(prev_stunting," (",n_stunting,"/",N_haz,")"),
         `Mean WHZ` = paste0(mean_whz," (",sd_whz,")"),
         `Wasting prevalence (n/N)` = paste0(prev_wasting," (",n_wasting,"/",N_whz,")"),
         `Mean WAZ` = paste0(mean_waz," (",sd_waz,")"),
         `Underweight prevalence (n/N)` = paste0(prev_underwt," (",n_underwt,"/",N_waz,")")
         ) %>%
  select("trial", "study", 
         "Diarrhea prevalence (n/N)",
         "Mean HAZ" ,
         "Stunting prevalence (n/N)",
         "Mean WHZ",
         "Wasting prevalence (n/N)",
         "Mean WAZ",
         "Underweight prevalence (n/N)") %>%
  rename(Trial=trial, Study=study)

colnames <- tab_Y_transpose$Trial
rownames <- colnames(tab_Y_transpose)



#transpose table
tab_Y_transpose <- as.data.frame(t(as.matrix(tab_Y_transpose)))
colnames(tab_Y_transpose) <- colnames
tab_Y_transpose$variables <- rownames
for(i in 1:ncol(tab_Y_transpose)){
  tab_Y_transpose[,i] <- as.character(tab_Y_transpose[,i])
  tab_Y_transpose[grepl("NA",tab_Y_transpose[,i]),i] <- ""
  tab_Y_transpose[grepl("<NA>",tab_Y_transpose[,i]),i] <- ""
}


#save as csv
write.csv(tab_Y_transpose,here("figures/aim2_table1.csv"))




