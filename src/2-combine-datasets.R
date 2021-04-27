
rm(list=ls())
source(here::here("0-config.R"))


#Gram vikas
gv <- readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds")) %>% 
  mutate(study="Reese et al. 2017",
         #dataid=hh_hid*10000 + dataid,
         dataid=as.numeric(factor(hh_mid))*10+round)
head(gv)



table(gv$target, gv$sample, gv$pos)

gv2 <- gv %>% filter(pos==1)
table(gv2$target, gv2$sample, gv2$round)

#Mapsan
mapsan <- readRDS(paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds")) %>% mutate(study="Holcomb et al. 2020") 

#Drop baseline measure from mapsan
mapsan <- mapsan %>% filter(round != "0m") %>% droplevels(.)

#Wash benefits
WBB <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))
WBB <- WBB %>% mutate(study=case_when(round=="" & target %in% c("sth","ascaris","trichuris") ~ "Kwong et al. 2021",
                                      round=="" ~ "Fuhrmeister et al. 2020",
                                      round=="World Bank" ~ "Boehm et al. 2016"
                                      ))

WBK <- readRDS(paste0(dropboxDir, "Data/WBK/Clean/WBK_env.RDS"))
WBK <- WBK %>% mutate(study="Steinbaum et al. 2019")

colnames(mapsan)
colnames(WBB)
colnames(WBK)

WBK <- WBK %>% rename( dataid=hhid, Nhh=num_hh, hhwealth=assetquintile, sampleid=soil_id) %>%
  mutate(round="")

WBB <- WBB %>% subset(., select = c(study, sampleid, dataid, clusterid, tr, sample, target, pos, abund, qual, round, block, Nhh, momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  floor, hhwealth)) %>%
              mutate( tr = factor(tr, levels = c("Control", "Sanitation")))
WBK <- WBK %>% subset(., select = c(study, sampleid, dataid, clusterid, tr, sample, target, pos, abund, round, block, Nhh, 
                                    #momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  
                                    floor, hhwealth))



#Odisha
odisha <- readRDS(file=paste0(dropboxDir,"Data/Odisha/GV_env_cleaned.rds")) %>% mutate(study="Odagiri et al. 2016")



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


#check duplicates
dim(d)
dim(d %>% distinct(.))
dim(d %>% distinct(study,  dataid, tr, clusterid, sample, target,  sampleid))
# df <- d %>% group_by(study,  dataid, tr, clusterid, sample, target,  sampleid) %>% mutate(N=n()) %>%
#   filter(N>1) %>% arrange(study,  dataid, tr, clusterid, sample, target,  sampleid)


  #Mark positives
table(d$pos)
d$pos <- ifelse(d$pos==2, 1, d$pos)
table(d$pos)

#create aggregate outcomes
head(d)
d %>% distinct(study, target)

#Give targets more descriptive names
unique(d$target)
d <- d %>% mutate(
  target = case_when(
    target=="ECVG" ~ "Pathogenic E. coli",
    target=="Hum" ~ "Human (HumM2)",
    target=="BC" ~ "Animal (BacCow)",
    target=="BacCow" ~ "Animal (BacCow)",
    target=="Gia" ~ "Giardia",
    target=="Noro" ~ "Norovirus",
    target=="sth" ~ "Any STH",
    target=="ascaris" ~ "Ascaris",
    target=="trichuris" ~ "Trichuris",
    target=="br" ~ "Animal (BacR)",
    target=="av" ~ "Avian (GFD)",
    target=="rv" ~ "Rotavirus",
    target=="gbc" ~ "General (GenBac3)",
    target=="GFD" ~ "Avian (GFD)", 
    target=="BacHum" ~ "Human (Bacteriodes)",
    target=="HF183" ~ "Human (Bacteriodes)",
    target=="Mnif" ~ "Human (M. smithii)",
    target=="adenovirus_40_41" ~ "Adenovirus",
    target=="ascaris_lumbricoides" ~ "Ascaris",
    target=="astrovirus" ~ "Astrovirus",
    target=="cryptosporidium_parvum" ~ "Cryptosporidium",
    target=="shigella" ~ "Shigella",
    target=="entamoeba_histolytica" ~ "Entamoeba histolytica",
    target=="giardia" ~ "Giardia",
    target=="rotavirus_A" ~ "Rotavirus",
    target=="salmonella_spp" ~ "Salmonella",
    target=="trichuris_trichiura" ~ "Trichuris",
    target=="vibrio_cholera" ~ "V. cholerae",
    target=="yersinia_spp" ~ "Yersinia",
    target=="sapovirus" ~ "Sapovirus",
    target=="campylobacter_jejuni_coli" ~ "Campylobacter jejuni/coli",
    target=="clostridium_difficile" ~ "C. difficile",
    target=="pathogenic_ecoli" ~ "Pathogenic E. coli",
    target=="norovirus_GI_GII" ~ "Norovirus",
    target=="BacUni" ~ "General (BacUni)",
    target=="Ruminant" ~ "Animal (BacR)",
    target==target ~target
  )
)

table(d$study, d$target)




#create aggregate outcomes
unique(d$target)




# targets=any_protozoa
# name="Any protozoa"
# 
# df <- d[d$target %in% any_protozoa,]
# dim(df)
# dim(df %>% distinct(sampleid, study, tr,  dataid, clusterid, round, sample))
# table(df$pos, df$target)
# table(df$pos)
# 
# df1 <- df %>% filter(is.na(sampleid))
# df2 <- df %>% filter(!is.na(sampleid))
# head(df1)
# head(df2)
# table(df1$sample)
# table(df2$sample)
# table(df1$target)
# table(df2$target)
# 
# targets=any_protozoa
# name="Any protozoa"
# 
# #Holcomb et al. 2020 has repeat sample IDs- figure out why?
# d <- d %>% filter(target %in% !!(targets), study=="Holcomb et al. 2020")
# head(d)
# table(d$target)
# df <- d %>% group_by(sampleid, sample) %>% summarise(N=length(unique(tr))) 
# table(df$N) #Why is TR getting scrambled!?!

agg_function <- function(targets, name){
  #dtemp <- d %>% filter(target %in% !!(targets)) 
  df <- d %>% group_by(study,  dataid, tr, clusterid, sample,  sampleid) %>%
    filter(target %in% !!(targets)) %>%
    select(pos, target)
  table(df$target)
  
  
  d_agg <- df %>% group_by(study,  dataid, tr, clusterid, sample,  sampleid) %>%
    summarise(
      any_pos = 1*(sum(pos==1)>0), N=n()) %>% 
    ungroup()
  table(d_agg$any_pos)
  
  d_agg <- d_agg %>% rename(pos=any_pos) %>% select(study, dataid,clusterid, sample, pos) %>% mutate(target=!!(name))
  
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

d_any_general_MST <- agg_function(general_MST, "Any general MST")
d_any_human_MST <- agg_function(human_MST, "Any human MST")
d_any_animal_MST <- agg_function(animal_MST, "Any animal MST")

as.data.frame(d_any_protozoa$tab)

d_agg <- bind_rows(d_any_pathogen$df, d_any_virus$df, d_any_protozoa$df, d_any_bacteria$df,
                   d_any_general_MST$df, d_any_human_MST$df, d_any_animal_MST$df)
table(d_agg$pos)
table(d_agg$target,d_agg$pos)
table(d_agg$study, d_agg$target)

#Save tables
save(d_any_pathogen, d_any_virus, d_any_protozoa, d_any_bacteria,
                   d_any_general_MST, d_any_human_MST, d_any_animal_MST,
     file=here("figures/agg_tables.Rdata"))

# XXXXXXXXXXXXXXXXXXXXXX
# Note:
#   Need to keep covariates in the individual data frame because of sammple-specific vars
#   and just make a cov dataframe with hh-level vars merged into the aggregate dateset
#  plus fix dataframe names
#  Note 2: I updated code to do this, but causing duplicated in the aggregate dataframe... one covariate must vary across HH... check maybe maternal?
# XXXXXXXXXXXXX

#covariates
dim(d)
#compound covariates
cov <- d %>% group_by(study, tr,  dataid, clusterid, round, sample) %>%
  #arrange(Nhh, floor, hhwealth) %>% fill(Nhh, floor, hhwealth) %>%
  slice(1) %>% ungroup() %>%
  subset(., select = -c(target, pos, abund, abund_only_detect, censored)) %>% 
  distinct(.)
dim(cov)

dim(d)
dim(d_agg)
dim(cov)
df <- left_join(d_agg, cov, by=c("study","dataid","clusterid","sample"))
dim(df)
d <- bind_rows(d, df)
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

temp <- d %>% filter(target=="Pathogenic E. coli", study=="Fuhrmeister et al. 2020")
temp %>% 
  group_by(study, dataid, target, sample) %>%
  summarise(N=n()) %>% 
  group_by(sample) %>%
  summarise(mean(N)) 

d %>% 
  group_by(study, dataid, target, sample) %>%
  summarise(N=n()) %>% 
  group_by(study,sample) %>%
  summarise(mean(N)) 


#Create rows for positivity in any sample type
dim(d)
table(d$pos)
table(is.na(d$pos))
df <- d %>% 
  group_by(study, clusterid, dataid, tr, target) %>%
  mutate(pos=max(pos, na.rm = TRUE), sample="any sample type") %>% 
  slice(1)
dim(df)
table(df$pos)
table(df$target, df$pos, df$study)
df %>% tabyl(target, pos, study, show_na = T, show_missing_levels = F)
df %>% tabyl(target, tr, study, show_na = T, show_missing_levels = F)

d <- bind_rows(d , df)


#Check furmeister e. coli
temp <- d %>% filter(target=="Pathogenic E. coli", study=="Fuhrmeister et al. 2020")
table(temp$tr,  temp$sample)
#Why are there less "any sample type" than MH... are there multiple MH per dataid? 
table(temp$tr, temp$pos, temp$sample)

table(d$target, d$pos)

saveRDS(d, file=paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
