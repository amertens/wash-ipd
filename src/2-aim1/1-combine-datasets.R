
rm(list=ls())
source(here::here("0-config.R"))


#Gram vikas
gv <- readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds")) %>% 
  mutate(study="Reese et al. 2017",
         trial="Gram Vikas",
         #dataid=hh_hid*10000 + dataid,
         dataid=as.numeric(factor(hh_mid))*10+round,
         dadagri=as.numeric(dadagri),
         landacre=as.numeric(landacre))
head(gv)



table(gv$target, gv$sample, gv$pos)

gv2 <- gv %>% filter(pos==1)
table(gv2$target, gv2$sample, gv2$round)

#Mapsan
mapsan <- readRDS(paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds")) %>% 
  mutate(study="Holcomb et al. 2020",
         trial="MapSan",
         momedu=factor(momedu)) %>%
  rename(animals=compAnyAnimal)


#Wash benefits
WBB <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))
WBB <- WBB %>% mutate(study=case_when(round=="" & target %in% c("sth","ascaris","trichuris") ~ "Kwong et al. 2021",
                                      round=="" ~ "Fuhrmeister et al. 2020",
                                      round=="World Bank" ~ "Boehm et al. 2016"
                                      ),
                      trial="WBB")

WBK <- readRDS(paste0(dropboxDir, "Data/WBK/Clean/WBK_env.RDS"))
WBK <- WBK %>% mutate(study="Steinbaum et al. 2019", trial="WBK")

colnames(mapsan)
colnames(WBB)
colnames(WBK)

WBK <- WBK %>% rename( dataid=compoundid, Nhh=num_hh, hhwealth=assetquintile, sampleid=soil_id) %>%
  mutate(round="el")

WBB <- WBB %>% subset(., select = c(study, trial, sampleid, dataid, clusterid, tr, sample,  env_date,
                                    target, pos, abund, qual, round, block, Nhh, momage, momheight, 
                                    momedu, dadagri,landacre, hfiacat,watmin,  floor, hhwealth,
                                    roof, elec, walls, animals)) 
WBK <- WBK %>% subset(., select = c(study, trial, sampleid, dataid, clusterid, tr, sample, target, pos, abund, round, block, Nhh, 
                                    momage, momedu, hfiacat,  animals,
                                    floor, hhwealth, env_date, roof, elec, walls)) %>%
                mutate(hfiacat=factor(hfiacat))



#Odisha
odisha <- readRDS(file=paste0(dropboxDir,"Data/Odisha/Odisha_env_cleaned.rds")) %>% mutate(study="Odagiri et al. 2016", trial="Odisha")



mapsan$sampleid<-as.character(mapsan$sampleid)
WBB$sampleid<-as.character(WBB$sampleid)
WBK$sampleid<-as.character(WBK$sampleid)
gv$round<-as.character(gv$round)
odisha$sampleid<-as.character(odisha$sampleid)
#mapsan$momedu<-factor(mapsan$momedu)





d <- bind_rows(WBB, WBK, mapsan, gv, odisha) %>%
  mutate(study=factor(study, 
            levels=c("Odagiri et al. 2016", "Boehm et al. 2016", "Reese et al. 2017", "Steinbaum et al. 2019", "Fuhrmeister et al. 2020", "Holcomb et al. 2020",  "Kwong et al. 2021")))
head(d)
table(d$study, is.na(d$sampleid))
table(d$study, is.na(d$sampleid), d$pos)
table(d$study, d$sample)

d %>% distinct(study, sample, target)



#-----------------------------------------------
#check duplicates
#-----------------------------------------------

dim(d)
dim(d %>% distinct(.))
dim(d %>% distinct(study,  dataid, tr, clusterid, sample, target,  sampleid, pos))
dim(d %>% distinct(study,  dataid, tr, round, clusterid, sample, target,  sampleid, pos))
dim(d %>% distinct(study,  dataid, tr, env_date, clusterid, sample, target,  sampleid, pos))
df <- d %>% group_by(study,  dataid, tr, clusterid, sample, target,  sampleid) %>% mutate(N=n()) %>%
  filter(N>1) %>% arrange(study,  dataid, tr, clusterid, sample, target,  sampleid)
head(df)

  #Mark positives
table(d$pos)
d$pos <- ifelse(d$pos==2, 1, d$pos)
table(d$pos)

#create aggregate outcomes
head(d)
d %>% distinct(study, target)

#-----------------------------------------------
#Give targets more descriptive names
#-----------------------------------------------
unique(d$target)
d <- d %>% mutate(
  target_raw=target,
  target = case_when(
    target=="ECVG" ~ "Pathogenic E. coli",
    target=="Hum" ~ "Human (HumM2)",
    target=="BC" ~ "Animal (BacCow)",
    target=="BacCow" ~ "Animal (BacCow)",
    target=="BacCan" ~ "Animal (BacCan)",
    target=="Gia" ~ "Giardia",
    target=="Noro" ~ "Norovirus",
    target=="norovirus_GIGII" ~ "Norovirus",
    target=="sth" ~ "Any STH",
    target=="ascaris" ~ "Ascaris",
    target=="trichuris" ~ "Trichuris",
    target=="a.lumbricoides" ~ "Ascaris",
    target=="br" ~ "Animal (BacR)",
    target=="av" ~ "Avian (GFD)",
    target=="rv" ~ "Rotavirus",
    target=="pan_enterovirus" ~ "Pan enterovirus",
    target=="gbc" ~ "General (GenBac3)",
    target=="GFD" ~ "Avian (GFD)", 
    target=="BacHum" ~ "Human (Bacteroides)",
    target=="HF183" ~ "Human (Bacteroides)",
    target=="Mnif" ~ "Human (M. smithii)",
    target=="adenovirus_40_41" ~ "Adenovirus",
    target=="pan_adenovirus" ~ "Adenovirus",
    target=="ascaris_lumbricoides" ~ "Ascaris",
    target=="astrovirus" ~ "Astrovirus",
    target=="cryptosporidium_parvum" ~ "Cryptosporidium",
    target=="cryptosporidium_parvum_or_hominis" ~ "Cryptosporidium",
    target=="shigella" ~ "Shigella",
    target=="shigella_eiec" ~ "Shigella",
    target=="entamoeba_histolytica" ~ "Entamoeba histolytica",
    target=="giardia" ~ "Giardia",
    target=="rotavirus_A" ~ "Rotavirus",
    target=="rotavirus" ~ "Rotavirus",
    target=="salmonella_spp" ~ "Salmonella",
    target=="trichuris_trichiura" ~ "Trichuris",
    target=="t.trichiura" ~ "Trichuris",
    target=="vibrio_cholera" ~ "V. cholerae",
    target=="c.cholerae" ~ "V. cholerae",
    target=="yersinia_spp" ~ "Yersinia",
    target=="sapovirus" ~ "Sapovirus",
    target=="campylobacter_jejuni_coli" ~ "Campylobacter",
    target=="campylobacter" ~ "Campylobacter",
    target=="clostridium_difficile" ~ "C. difficile",
    target=="pathogenic_ecoli" ~ "Pathogenic E. coli",
    target=="norovirus_GI_GII" ~ "Norovirus",
    target=="BacUni" ~ "General (BacUni)",
    target=="Ruminant" ~ "Animal (BacR)",
    target==target ~target
  )
)

unique(d$target)[!(unique(d$target) %in% c(any_pathogens,any_MST))]
unique(d$target[d$target_raw==d$target])

table(d$study, d$target)

#-----------------------------------------------
# Drop abundances without variation
#-----------------------------------------------

#Drop abundances with less than or equal to 5 unique values or continious estimation doesn't work
d <- d %>% group_by(study, target, sample) %>% 
  mutate(Nunique=length(unique(abund)), abund=ifelse(Nunique<=5,NA, abund))

#-----------------------------------------------
#Clean covariates
#-----------------------------------------------


d <- d %>% mutate(tr=case_when(tr=="Control"~"Control",
                               tr=="Intervention"|tr=="Sanitation"~"Intervention"),
                  tr=factor(tr, levels=c("Control","Intervention")))
table(d$tr)

d <- d %>% group_by(trial) %>% 
  mutate(hhwealth=factor(ntile(hhwealth,4), levels=c("1","2","3","4")),
         hhwealth=fct_explicit_na(hhwealth, na_level = "Missing"),
         Nhh=factor(case_when(
           Nhh<5 ~ "<5",
           Nhh>=5 & Nhh <=8 ~ "5-8",
           Nhh>8 ~ ">8"
         ), levels=c("5-8","<5",">8")),
         Nhh=fct_explicit_na(Nhh, na_level = "Missing"),
         nrooms=as.numeric(nrooms),
         nrooms=factor(case_when(
           nrooms<3 ~ "1-2",
           nrooms>2  ~ ">3",
         ), levels=c("1-2",">3")),
         nrooms=fct_explicit_na(nrooms, na_level = "Missing"),
         dadagri=factor(dadagri),
         dadagri=fct_explicit_na(dadagri, na_level = "Missing"),
         momedu=factor(momedu),
         momedu=fct_explicit_na(momedu, na_level = "Missing"),
         hfiacat=factor(hfiacat),
         hfiacat=fct_explicit_na(hfiacat, na_level = "Missing"),
         walls=factor(walls),
         walls=fct_explicit_na(walls, na_level = "Missing"),
         roof=factor(roof),
         roof=fct_explicit_na(roof, na_level = "Missing"),
         floor=factor(floor),
         floor=fct_explicit_na(floor, na_level = "Missing"),
         elec=factor(elec),
         elec=fct_explicit_na(elec, na_level = "Missing")
  )

# 1.	Child birth order/parity -aim 2 only
#-missing from all

# 2.	Asset-based wealth index 
table(d$study, d$hhwealth) #check Reese and Holcomb missing
table(d$study, is.na(d$hhwealth))

# 3.	Number of individuals and children in household
table(d$study, d$Nhh) #check Reese and Holcomb missing
table(d$study, is.na(d$Nhh))

# 4.	Household food security -aim 2 only
table(d$study, d$hfiacat) 
table(d$study, is.na(d$hfiacat)) #add to reese and steinbaum


# 5.	Household electrification and construction, including wall/roof material
table(d$study, d$elec) #check missing in Holcomb
table(d$study, is.na(d$elec))
table(d$study, d$walls) #check missing in Holcomb
table(d$study, is.na(d$walls))
table(d$study, d$floor) #check missing in Holcomb
table(d$study, is.na(d$floor))

table(d$study, d$nrooms) #check missing in WBB
table(d$study, is.na(d$nrooms))

# 6.	Parental age -aim 2 only
table(d$study, d$momage)
table(d$study, is.na(d$momage))
# 7.	Parental education 
table(d$study, d$momedu)
table(d$study, is.na(d$momedu)) #add to Steinbaum and check Holcomb
# 8.	Parental employment  - check all
# a.	Indicator for works in agriculture 
table(d$study, d$dadagri)
table(d$study, is.na(d$dadagri)) #add missingness check Steinbaum

# 9.	Land ownership 
table(d$study, is.na(d$landacre)) #check Steinbaum, reese



#-----------------------------------------------
# drop unneeded columns
#-----------------------------------------------
colnames(d)
d <- d %>% subset(., select=c(study,            trial,            sampleid,         dataid,           clusterid,       
                              tr,               sample,           env_date,         target,           pos,             
                              abund,            qual,             round,            Nhh,             
                              momage,           momedu,           dadagri,          landacre,        
                              hfiacat,          watmin,           floor,            hhwealth,         roof,            
                              elec,             walls,            nrooms,           season,  animals))


#-----------------------------------------------
#create aggregate outcomes
#-----------------------------------------------

agg_function <- function(targets, name){
  df <- d %>% group_by(study,  dataid, tr, clusterid, sample, round,  sampleid) %>%
    filter(target %in% !!(targets)) %>%
    select(pos, target) %>%
    group_by(study,  dataid, tr, clusterid, sample, round, sampleid,  target)# %>%
    # mutate(perc_pos = mean(pos, na.rm=T), N_neg=sum(pos==0,na.rm=T)) %>%
    # ungroup()
  
  #Drop too-positive strata
  df <- df %>% filter(!(study=="Odagiri et al. 2016" & sample=="W" & target=="Animal (BacCow)"), 
                      !(study=="Boehm et al. 2016" & sample=="CH" & target=="General (GenBac3)"),
                      !(study=="Fuhrmeister et al. 2020" & sample=="CH" & target=="Animal (BacCow)"),
                      !(study=="Holcomb et al. 2020" & sample=="FlyLat" & target=="Human (Bacteroides)"))
  
  
  
  # if(any(df$perc_pos>0.85)){
  #   cat("Dropping too positive:\n")
  #   df_dropped <- df %>% filter(perc_pos>0.9 | N_neg<10)
  #   print(df_dropped %>% distinct(study, sample, target, perc_pos,N_neg))
  #   df <- df %>% filter(perc_pos<=0.9 & N_neg>=10)
  # }
    
  # summary(df$perc_pos)
  # table(df$target, df$perc_pos)
  # table(df$study, df$target, df$pos)
  # 
  # df %>% distinct(study,  target, perc_pos) %>% arrange(-perc_pos)
  # 
  d_agg <- df %>% group_by(study,  dataid, tr, clusterid, sample, round, sampleid) %>%
    summarise(
      any_pos = 1*(sum(pos==1)>0), N=n()) %>% 
    ungroup()
  table(d_agg$any_pos)
  
  d_agg <- d_agg %>% rename(pos=any_pos) %>% select(study,  tr, dataid,clusterid, sample,round, sampleid, pos) %>% mutate(target=!!(name))

  tab <- bind_rows(df, d_agg) %>%
    group_by(study, target) %>%
    summarise(N=n(), n=sum(pos, na.rm=T)) %>%
    mutate(target=factor(target, levels=c(!!(name),!!(targets))),
           N=paste0(n,"/",N)) %>%
    arrange(target) %>%
    pivot_wider(id_cols = study, names_from = target, values_from = N)
  
  return(list(df=d_agg, tab=tab))
}

d_any_pathogen <- agg_function(any_pathogens, "Any pathogen")
d_any_virus <- agg_function(any_virus, "Any virus")
d_any_protozoa <- agg_function(any_protozoa, "Any protozoa")
d_any_bacteria <- agg_function(any_bacteria, "Any bacteria")


d_any_MST <- agg_function(any_MST, "Any MST")
d_any_general_MST <- agg_function(general_MST, "Any general MST")
d_any_human_MST <- agg_function(human_MST, "Any human MST")
d_any_animal_MST <- agg_function(animal_MST, "Any animal MST")


#Specifically, BacCow MST markers from Odagiri et al. 2016, GenBac3 in Boehm et al. 2016, and human Bacteroides in Holcomb et al. 2020 had close to 100% prevalence, also leading to high positivity in aggregate targets. 


as.data.frame(d_any_protozoa$tab)

d_agg <- bind_rows(d_any_pathogen$df, d_any_virus$df, d_any_protozoa$df, d_any_bacteria$df,
                   d_any_MST$df, d_any_general_MST$df, d_any_human_MST$df, d_any_animal_MST$df)
table(d_agg$pos)
table(d_agg$target,d_agg$pos)
table(d_agg$study, d_agg$target)

#Save tables
save(d_any_pathogen, d_any_virus, d_any_protozoa, d_any_bacteria,
                   d_any_general_MST, d_any_human_MST, d_any_animal_MST,
     file=here("figures/agg_tables.Rdata"))


#-----------------------------------------------
# Merge covariates into aggregate outcomes
#-----------------------------------------------


#covariates
dim(d)
#compound covariates
cov <- d %>% group_by(study, tr,  dataid, clusterid, sample) %>%
  #arrange(Nhh, floor, hhwealth) %>% fill(Nhh, floor, hhwealth) %>%
  slice(1) %>% ungroup() %>%
  subset(., select = -c(round, tr, sampleid, target, pos, abund)) %>% 
  distinct(.)
head(cov)

dim(d)
dim(d_agg)
dim(cov)
d_agg_cov <- left_join(d_agg, cov, by=c("study","dataid","clusterid","sample"))
dim(d_agg_cov)
d <- bind_rows(d, d_agg_cov)
dim(d)

table(d$target)
table(d$study, d$target)





#mark aggregate outcomes
aggregate_vars <- c("Any general MST",       "Any human MST",        "Any animal MST",  
      "Any pathogen",    "Any bacteria",                       
      "Any virus",       "Any STH", "Any protozoa")

d <- d %>% mutate(
      aggregate_Y = case_when(
        target %in% aggregate_vars ~ 1,
        !(target %in% aggregate_vars) ~ 0
        ))


#Create rows for positivity in any sample type
dim(d)
table(d$pos)
table(is.na(d$pos))

df <- d %>% 
  group_by(study, clusterid, dataid, tr, target, round) %>%
  #Drop too-positive strata
  filter(!(study=="Odagiri et al. 2016" & sample=="W" & target=="Animal (BacCow)"), 
                      !(study=="Boehm et al. 2016" & sample=="CH" & target=="General (GenBac3)"),
                      !(study=="Fuhrmeister et al. 2020" & sample=="CH" & target=="Animal (BacCow)"),
                      !(study=="Holcomb et al. 2020" & sample=="FlyLat" & target=="Human (Bacteroides)")) %>%
  mutate(pos=max(pos, na.rm = TRUE), sample="any sample type", animals=max(animals, na.rm=T)) %>% 
  slice(1)
dim(df)
table(df$pos)

df$animals[is.infinite(df$animals)] <- NA 
table(df$animals)
table(df$target, df$pos, df$study)
df %>% tabyl(target, pos, study, show_na = T, show_missing_levels = F)
df %>% tabyl(target, tr, study, show_na = T, show_missing_levels = F)

d <- bind_rows(d , df)


#Add in seasonality variable
d <- d %>%
  mutate(
    month=month(env_date),
    wet=
      case_when(
        #bangladesh monsoon may to october: https://www.weather-atlas.com/en/bangladesh/mymensingh-climate
        #India is may to october https://www.weather-atlas.com/en/india/bhubaneswar-climate#rainfall
        trial%in% c("WBB","Gram Vikas","Odisha") & month>=5 & month<=10  ~ 1,
        trial%in% c("WBB","Gram Vikas","Odisha") & (month<5 | month>10)  ~ 0,
        # Kenya has two peaks, march-may and oct-dec:  https://www.weather-atlas.com/en/kenya/kakamega-climate
        trial=="WBK" & month %in% c(3,4,5,10,11,12)  ~ 1,
        trial=="WBK" & month %in% c(1,2,6,7,8,9)  ~ 0,
        #Maputo is November to april: https://www.weather-atlas.com/en/mozambique/maputo-climate#rainfall
        trial=="MapSan" & month %in% c(11,12,1,2,3,4)  ~ 1,
        trial=="MapSan" & month %in% c(5,6,7,8,9,10)  ~ 0
      )
  )
table(d$trial, d$wet)
#table(d$env_date[d$trial=="Odisha"]) #note no variation in Odisha seasonality
table(d$sample, d$wet)
table(d$target, d$wet)


table(d$trial, d$animals)
table(d$sample, d$animals)
table(d$target, d$animals)


saveRDS(d, file=paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))

# #Check covariates
# table(d$trial, (d$hhwealth))
# table(d$trial, (d$Nhh))
# 
# table(d$trial, (d$nrooms))
# table(d$trial, (d$walls))
# table(d$trial, (d$floor))
# table(d$trial, (d$elec))


