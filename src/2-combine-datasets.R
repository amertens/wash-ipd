
rm(list=ls())
source(here::here("0-config.R"))


#Gram vikas
gv <- readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds")) %>% mutate(study="Gram Vikas")
head(gv)



table(gv$target, gv$sample, gv$pos)

gv2 <- gv %>% filter(pos==1)
table(gv2$target, gv2$sample, gv2$round)

#Mapsan
mapsan <- readRDS(paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds")) %>% mutate(study="mapsan") 

#Wash benefits
WBB <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))
WBB <- WBB %>% mutate(study="WBB")

WBK <- readRDS(paste0(dropboxDir, "Data/WBK/Clean/WBK_env.RDS"))
WBK <- WBK %>% mutate(study="WBK")

#Temp scramble treatment
table(WBB$tr, WBB$pos)
WBB$tr = sample(WBB$tr, nrow(WBB))
table(WBB$tr, WBB$pos)

WBK$tr = sample(WBK$tr, nrow(WBK))
mapsan$tr = sample(mapsan$tr, nrow(mapsan))

colnames(mapsan)
colnames(WBB)
colnames(WBK)

WBK <- WBK %>% rename( dataid=hhid, Nhh=num_hh, hhwealth=assetquintile, sampleid=soil_id) %>%
  mutate(round="")

WBB <- WBB %>% subset(., select = c(study, sampleid, dataid, clusterid, tr, sample, target, pos, abund, round, block, Nhh, momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  floor, hhwealth)) %>%
              mutate( tr = factor(tr, levels = c("Control", "Sanitation")))
WBK <- WBK %>% subset(., select = c(study, sampleid, dataid, clusterid, tr, sample, target, pos, abund, round, block, Nhh, 
                                    #momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  
                                    floor, hhwealth))

#mark seasons by date of sample collection  



WBB$sampleid<-as.character(WBB$sampleid)
WBK$sampleid<-as.character(WBK$sampleid)
gv$round<-as.character(gv$round)
#mapsan$momedu<-factor(mapsan$momedu)


#Odisha
odisha <- readRDS(file=paste0(dropboxDir,"Data/Odisha/GV_env_cleaned.rds"))




d <- bind_rows(WBB, WBK, mapsan, gv, odisha)
colnames(d)

d %>% distinct(study, sample, target)


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
    target=="Hum" ~ "HumM2",
    target=="BC" ~ "BacCow",
    target=="Gia" ~ "Giardia",
    target=="Noro" ~ "Norovirus",
    target=="sth" ~ "Any STH",
    target=="ascaris" ~ "Ascaris",
    target=="trichuris" ~ "Trichuris",
    target=="br" ~ "Ruminant",
    target=="av" ~ "Avian",
    target=="rv" ~ "Rotavirus",
    target=="gbc" ~ "GenBac3",
    target=="GFD" ~ "Avian (Helicobacter)",
    target=="HF183" ~ "Human (Bacteroides)",
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
    target=="clostridium_difficile" ~ "C. difficile",
    # target=="enteroaggregative_Ecoli" ~ "EAEC",
    # target=="enteropathogenic_Ecoli" ~ "EPEC",
    # target=="enterotoxigenic_Ecoli" ~ "ETEC",
    # target=="shiga_toxin_producing_Ecoli" ~ "STEC",
    target=="pathogenic_ecoli" ~ "Pathogenic E. coli",
    target=="norovirus_GI_GII" ~ "Norovirus",
    target==target ~target
  )
)

table(d$study, d$target)




#create aggregate outcomes
unique(d$target)


#pathogens:
any_pathogens = c("E. coli virulence gene",  "Pathogenic E. coli", "Giardia",  "C. difficile",
                  "Shigella",  "Entamoeba histolytica",  "V. cholerae", "Yersinia",       
"Norovirus",             "Any STH", "Ascaris",
"Adenovirus","Trichuris",  "Rotavirus", "Astrovirus", "Cryptosporidium", "Salmonella")   

any_virus = c("Norovirus",  "Adenovirus", "Rotavirus", "Astrovirus")   
any_bacteria = c("E. coli virulence gene", "Pathogenic E. coli", "Yersinia",  "V. cholerae", "Shigella",  "C. difficile",  "Salmonella")   
#any_helminth = c("Any STH", "Ascaris", "Trichuris")   
any_protozoa = c("Giardia", "Cryptosporidium", "Entamoeba histolytica")   





#MST's:
general_MST = c("GenBac3")

animal_MST = c( "BacCow",   
                "Ruminant",              "Avian",
                "Avian (Helicobacter)")

human_MST = c("HumM2",  "Human (Bacteroides)",   "Human (M. smithii)")




# d_agg <- d %>% group_by(study, tr,  dataid, clusterid, round, sample) %>%
#   summarise(
#     any_pathogen = 1*(sum(pos==1 & target %in% any_pathogens)>0),
#     any_virus = 1*(sum(pos==1 & target %in% any_virus)>0),                          
#     any_protozoa = 1*(sum(pos==1 & target %in% any_protozoa)>0),                          
#     any_bacteria = 1*(sum(pos==1 & target %in% any_bacteria)>0), 
#     any_general_MST = 1*(sum(pos==1 & target %in% general_MST)>0),                          
#     any_animal_MST = 1*(sum(pos==1 & target %in% animal_MST)>0),                          
#     any_human_MST = 1*(sum(pos==1 & target %in% human_MST)>0)) %>% 
#   ungroup()
# 
# d_any_pathogen <- d_agg %>% rename(pos=any_pathogen) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target="Any pathogen")
# d_any_virus <- d_agg %>% rename(pos=any_virus) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target="Any virus")
# d_any_protozoa <- d_agg %>% rename(pos=any_protozoa) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target="Any protozoa")
# d_any_bacteria <- d_agg %>% rename(pos=any_bacteria) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target="Any bacteria")
# d_any_general_MST <- d_agg %>% rename(pos=any_general_MST) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target="Any general MST")
# d_any_human_MST <- d_agg %>% rename(pos=any_human_MST) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target="Any human MST")
# d_any_animal_MST<- d_agg %>% rename(pos=any_animal_MST) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target="Any animal MST")

targets=any_protozoa
name="Any protozoa"

df <- d[d$target %in% any_protozoa,]
dim(df)
dim(df %>% distinct(sampleid, study, tr,  dataid, clusterid, round, sample))
table(df$pos, df$target)
table(df$pos)

agg_function <- function(targets, name){
  #dtemp <- d %>% filter(target %in% !!(targets)) 
  d_agg <- d %>% group_by(sampleid, study, tr,  dataid, clusterid, round, sample) %>%
    filter(target %in% !!(targets)) %>%
    summarise(
      any_pos = 1*(sum(pos==1)>0)) %>% 
    ungroup()
  table(d_agg$any_pos)
  
  d_agg <- d_agg %>% rename(pos=any_pos) %>% select(study, tr,dataid,clusterid,round, sample, pos) %>% mutate(target=!!(name))
  
  return(d_agg)
}

d_any_pathogen <- agg_function(any_pathogens, "Any pathogen")
d_any_virus <- agg_function(any_virus, "Any virus")
d_any_protozoa <- agg_function(any_protozoa, "Any protozoa")
d_any_bacteria <- agg_function(any_bacteria, "Any bacteria")

d_any_general_MST <- agg_function(general_MST, "Any general MST")
d_any_human_MST <- agg_function(human_MST, "Any human MST")
d_any_animal_MST <- agg_function(animal_MST, "Any animal MST")


d_agg <- bind_rows(d_any_pathogen, d_any_virus, d_any_protozoa, d_any_bacteria,
                   d_any_general_MST, d_any_human_MST, d_any_animal_MST)
table(d_agg$pos)
table(d_agg$target,d_agg$pos)
table(d_agg$study, d_agg$target)


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
df <- left_join(d_agg, cov, by=c("study","tr","dataid","clusterid","round","sample"))
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

#Create rows for positivity in any sample type
dim(d)
table(d$pos)
table(is.na(d$pos))
df <- d %>% mutate(abund=NA) %>% filter(!is.na(pos)) %>%
  group_by(study, tr,  dataid, clusterid, round, target) %>%
  mutate(pos=max(pos, na.rm = TRUE), sample="any sample type") %>% 
  slice(1)
dim(df)
table(df$pos)
table(df$target, df$pos)

d <- bind_rows(d , df)

table(d$target, d$pos)

saveRDS(d, file=paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
