
rm(list=ls())
source(here::here("0-config.R"))


#Gram vikas
gv <- readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds")) 
head(gv)



table(gv$target, gv$sample, gv$pos)

gv2 <- gv %>% filter(pos==1)
table(gv2$target, gv2$sample, gv2$round)

#Mapsan
mapsan <- readRDS(paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds")) %>% 
  mutate( trial="MapSan",
         momedu=factor(momedu)) %>%
  rename(animals=compAnyAnimal)


#Wash benefits
WBB <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))
WBB <- WBB %>% mutate(momedu=gsub(" \\(1-5y\\)","",momedu), momedu=gsub(" \\(>5y\\)","",momedu), 
                      trial="WBB")
table(WBB$study)
table(WBB$study, WBB$round)


WBK <- readRDS(paste0(dropboxDir, "Data/WBK/Clean/WBK_env.RDS"))
WBK <- WBK %>% mutate(study="Steinbaum 2019", trial="WBK")

table(WBB$momedu)
table(WBK$momedu)
table(gv$momedu)

colnames(mapsan)
colnames(WBB)
colnames(WBK)

WBK <- WBK %>% rename( dataid=compoundid, Nhh=num_hh, hhwealth=assetquintile, sampleid=soil_id) %>%
  mutate(round="el")

WBB <- WBB %>% subset(., select = c(study, trial, sampleid, dataid, hhid, clusterid, tr, sample,  env_date,
                                    target, pos, abund, qual, round, block, Nhh, momage, momheight, 
                                    momedu, dadagri,landacre, hfiacat,watmin,  floor, hhwealth,
                                    roof, elec, walls, animals)) 
WBK <- WBK %>% subset(., select = c(study, trial, sampleid, dataid, hhid, clusterid, tr, sample, target, pos, abund, round, block, Nhh, 
                                    momage, momedu, hfiacat,  animals,
                                    floor, hhwealth, env_date, roof, elec, walls)) %>%
                mutate(hfiacat=factor(hfiacat))



#Odisha
odisha <- readRDS(file=paste0(dropboxDir,"Data/Odisha/Odisha_env_cleaned.rds")) %>% mutate(study="Odagiri 2016", trial="Odisha", hhid=dataid)
table(odisha$target)


mapsan$sampleid<-as.character(mapsan$sampleid)
WBB$sampleid<-as.character(WBB$sampleid)
WBK$sampleid<-as.character(WBK$sampleid)
gv$round<-as.character(gv$round)
odisha$sampleid<-as.character(odisha$sampleid)
#mapsan$momedu<-factor(mapsan$momedu)





d <- bind_rows(WBB, WBK, mapsan, gv, odisha) %>%
  mutate(study=factor(study, 
            levels=c("Odagiri 2016", "Boehm 2016", "Reese 2017", "Steinbaum 2019", "Fuhrmeister 2020", "Holcomb 2020", "Capone 2021","Kwong 2021")))
table(d$study, d$target)


#mark STH qual as ROQ
d$qual <- ifelse(d$study %in% c("Steinbaum 2019","Kwong 2021"),"ROQ",d$qual)


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

table(d$study, is.na(d$hhid))

#-----------------------------------------------
#Give targets more descriptive names
#-----------------------------------------------
unique(d$target)
d <- d %>% mutate(
  target_raw=target,
  target = case_when(
    target=="ECVG" ~ "Pathogenic E. coli",
    target=="Hum" ~ "Human (HumM2)",
    target=="BC" ~ "Cow (BacCow)",
    target=="BacCow" ~ "Cow (BacCow)",
    target=="BacCan" ~ "Dog (BacCan)",
    target=="Gia" ~ "Giardia",
    target=="Noro" ~ "Norovirus",
    target=="norovirus_GIGII" ~ "Norovirus",
    target=="sth" ~ "Any STH",
    target=="ascaris" ~ "Ascaris",
    target=="trichuris" ~ "Trichuris",
    target=="a.lumbricoides" ~ "Ascaris",
    target=="br" ~ "Ruminant (BacR)",
    target=="av" ~ "Avian (GFD)",
    target=="rv" ~ "Rotavirus",
    target=="pan_enterovirus" ~ "Pan enterovirus",
    target=="gbc" ~ "General (GenBac3)",
    target=="GFD" ~ "Avian (GFD)", 
    target=="BacHum" ~ "Human (BacHum)",
    target=="HF183" ~ "Human (HF183)",
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
    target=="Ruminant" ~ "Ruminant (BacR)",
    target=="EC_not_zoo" ~ "Non-zoonotic E. coli",
    target=="EC_zoo" ~ "Zoonotic E. coli",
    target==target ~target
  )
)

unique(d$target)[!(unique(d$target) %in% c(any_pathogens,any_MST))]
unique(d$target[d$target_raw==d$target])


table(d$study, d$target)
table(d$target, !is.na(d$abund), d$sample, d$study)

#Drop Any STH from WBB because it will be re-aggregated below
d <- d %>% filter(target!="Any STH")

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
         landown=fct_explicit_na(landown, na_level = "Missing"),
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
table(d$hhwealth) 
table(d$study, d$hhwealth) #check Reese and Holcomb missing
table(d$study, is.na(d$hhwealth))

# 3.	Number of individuals and children in household
table(d$Nhh) 
d$Nhh <- factor(d$Nhh, levels=c("<5","5-8",">8", "Missing"))
table(d$study, d$Nhh) #check Reese and Holcomb missing
table(d$study, is.na(d$Nhh))

# 4.	Household food security -aim 2 only
levels(d$hfiacat) 
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
levels(d$momedu)
d$momedu <- factor(d$momedu, levels=c("No education", "Incomplete Primary", "Primary",  "Secondary", "More than secondary", "Missing"))
table(d$study, d$momedu)
table(d$study, is.na(d$momedu)) #add to Steinbaum and check Holcomb
# 8.	Parental employment  - check all
# a.	Indicator for works in agriculture 
table(d$study, d$dadagri)
table(d$study, is.na(d$dadagri)) #add missingness check Steinbaum

# 9.	Land ownership 
table(d$study, is.na(d$landown)) #check Steinbaum, reese



#-----------------------------------------------
# drop unneeded columns
#-----------------------------------------------
colnames(d)
d <- d %>% subset(., select=c(study,            trial,            sampleid,         dataid, hhid, clusterid,       
                              tr,               sample,           env_date,         target,           pos,             
                              abund,            qual,             round,            Nhh,             
                              momage,           momedu,           dadagri,          landown, landacre,        
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
  df <- df %>% filter(!(study=="Odagiri 2016" & sample=="W" & target=="Cow (BacCow)"), 
                      !(study=="Boehm 2016" & sample=="CH" & target=="General (GenBac3)"),
                      !(study=="Boehm 2016" & sample=="S" & target=="General (GenBac3)"),
                      !(study=="Fuhrmeister 2020" & sample=="CH" & target=="Cow (BacCow)"),
                      !(study=="Holcomb 2020" & sample=="FlyLat" & target=="Human (BacHum)"))
  
  
  
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
d_any_STH <- agg_function(c("Ascaris","Trichuris"), "Any STH")

unique(d$target)
d_any_MST <- agg_function(any_MST, "Any MST")
d_any_general_MST <- agg_function(general_MST, "Any general MST")
d_any_human_MST <- agg_function(human_MST, "Any human MST")
d_any_animal_MST <- agg_function(animal_MST, "Any animal MST")


d_any_zoonotic <- agg_function(zoonotic_pathogens, "Any zoonotic")

non_zoonotic <- c(any_pathogens[!(any_pathogens %in% zoonotic_pathogens)],"Non-zoonotic E. coli")
d_any_not_zoonotic <- agg_function(non_zoonotic, "Any non-zoonotic")
table(d_any_zoonotic$df$pos)
table(d_any_not_zoonotic$df$pos)
table(d_any_pathogen$df$pos)

#drop EC split by zoonotic origin
d <- d %>% filter(!(target %in% c("EC_not_zoo","EC_zoo")))

#Specifically, BacCow MST markers from Odagiri 2016, GenBac3 in Boehm 2016, and human Bacteroides in Holcomb 2020 had close to 100% prevalence, also leading to high positivity in aggregate targets. 


as.data.frame(d_any_protozoa$tab)

d_agg <- bind_rows(d_any_pathogen$df, d_any_virus$df, d_any_protozoa$df, d_any_bacteria$df, d_any_STH$df,
                   d_any_MST$df, d_any_general_MST$df, d_any_human_MST$df, d_any_animal_MST$df,
                   d_any_zoonotic$df, d_any_not_zoonotic$df)
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
      "Any virus",       "Any STH", "Any protozoa",
      "Any zoonotic", "Any non-zoonotic")

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
  filter(!is.na(pos)) %>%
  #Drop too-positive strata
  filter(!(study=="Odagiri 2016" & sample=="W" & target=="Cow (BacCow)"),
                      !(study=="Boehm 2016" & sample=="CH" & target=="General (GenBac3)"),
                      !(study=="Fuhrmeister 2020" & sample=="CH" & target=="Cow (BacCow)"),
                      !(study=="Holcomb 2020" & sample=="FlyLat" & target=="Human (BacHum)")) %>%
  mutate(pos=max(pos, na.rm = TRUE), sample="any sample type", animals=max(animals, na.rm=T)) %>% 
  slice(1)
dim(df)
table(df$pos)

temp <- df %>% filter(study=="Odagiri 2016")
table(temp$tr, temp$pos, temp$target)

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

table(is.na(d$clusterid))
table((d$pos))

d[is.infinite(d$pos),]
WBB[is.infinite(WBB$pos),]

# #Check covariates
# table(d$trial, (d$hhwealth))
# table(d$trial, (d$Nhh))
# 
# table(d$trial, (d$nrooms))
# table(d$trial, (d$walls))
# table(d$trial, (d$floor))
# table(d$trial, (d$elec))
table(d$trial, (d$dadagri))
table(d$trial, !is.na(d$dadagri))
table(d$trial, (d$landown))
table(d$trial, !is.na(d$landown))
table(d$trial, !is.na(d$landacre))
table(d$trial, (d$momedu))




