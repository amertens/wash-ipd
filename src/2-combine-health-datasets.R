
rm(list=ls())
source(here::here("0-config.R"))

mapsan <- readRDS(paste0(dropboxDir,"Data/MapSan/mapsan_cleaned.rds"))
mapsan <- mapsan %>% mutate(study="mapsan") %>%
    #harmonize coding of sample types
    mutate(
      type = case_when(
        type=="ds" ~ "S",
        type=="fp" ~ "FP",
        type=="hw" ~ "S",
        type=="ls" ~ "LS",
        type=="wp" ~ "SW"
      )      
    )


#merge WBB datasets
WBB_env <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))
WBB_health <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_child_health.RDS"))
dim(WBB_env)
dim(WBB_health)
#do I need to split WB/followup?
WBB <- left_join(WBB_env, WBB_health, by = c("dataid"))
dim(WBB)

table(WBB$pos, WBB$diar7d)

WBB <- WBB %>% mutate(study="WBB")

#merge WBK datasets
WBK_env <- readRDS(paste0(dropboxDir, "Data/WBK/Clean/WBK_env.RDS"))
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
  mutate(round="STH round")

WBB <- WBB %>% subset(., select = c(study, sampleid, dataid, clusterid, tr, type, target, pos, abund, round, block, Nhh, momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  floor, hhwealth)) %>%
              mutate( tr = factor(tr, levels = c("Control", "Sanitation")))
WBK <- WBK %>% subset(., select = c(study, sampleid, dataid, clusterid, tr, type, target, pos, abund, round, block, Nhh, 
                                    #momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  
                                    floor, hhwealth))



WBB$sampleid<-as.character(WBB$sampleid)
WBK$sampleid<-as.character(WBK$sampleid)
#mapsan$momedu<-factor(mapsan$momedu)
d <- bind_rows(WBB, WBK, mapsan)
colnames(d)

d %>% distinct(study, type, target)


table(d$pos)
d$pos <- ifelse(d$pos==2, 1, d$pos)
table(d$pos)




#create aggregate outcomes
head(d)
d %>% distinct(study, target)


#create aggregate outcomes


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
cov <- d %>% group_by(study, tr,  dataid, clusterid, round, type) %>%
  #arrange(Nhh, floor, hhwealth) %>% fill(Nhh, floor, hhwealth) %>%
  slice(1) %>% ungroup() %>%
  subset(., select = -c(target, pos, abund, abund_only_detect, censored)) %>% 
  distinct(.)
dim(cov)

d_agg <- d %>% group_by(study, tr,  dataid, clusterid, round, type) %>%
  summarise(any_pathogen = 1*(sum(pos)>0),
         any_human_MST = 1*(sum(pos==1 & target %in% c("Hum", "HF183","Mnif","sth"))>0),
         any_animal_MST = 1*(sum(pos==1 & target %in% c("BC","br","av","GFD"))>0)
         ) %>% ungroup()
table(d_agg$any_pathogen)
table(d_agg$any_human_MST)
table(d_agg$any_animal_MST)

d_any_pathogen <- d_agg %>% rename(pos=any_pathogen) %>% select(study, tr,dataid,clusterid,round, type, pos) %>% mutate(target="any_pathogen")
d_any_human_MST <- d_agg %>% rename(pos=any_human_MST) %>% select(study, tr,dataid,clusterid,round, type, pos) %>% mutate(target="any_human_MST")
d_any_animal_MST<- d_agg %>% rename(pos=any_animal_MST) %>% select(study, tr,dataid,clusterid,round, type, pos) %>% mutate(target="any_animal_MST")

d_agg <- bind_rows(d_any_pathogen, d_any_human_MST, d_any_animal_MST)
table(d_agg$pos)

dim(d)
dim(d_agg)
dim(cov)
df <- left_join(d_agg, cov, by=c("study","tr","dataid","clusterid","round","type"))
dim(df)
d <- bind_rows(d, df)
dim(d)

table(d$target)


#mark aggregate outcomes
d <- d %>% mutate(
      aggregate_Y = case_when(
        target %in% c("sth","any_pathogen","any_human_MST","any_animal_MST") ~ 1,
        !(target %in% c("sth","any_pathogen","any_human_MST","any_animal_MST")) ~ 0
        ))

#Create rows for positivity in any sample type
dim(d)
table(d$pos)
table(is.na(d$pos))
df <- d %>% mutate(abund=NA) %>% filter(!is.na(pos)) %>%
  group_by(study, tr,  dataid, clusterid, round, target) %>%
  mutate(pos=max(pos, na.rm = TRUE), type="any sample type") %>% 
  slice(1)
dim(df)
table(df$pos)

d <- bind_rows(d , df)

saveRDS(d, file=paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
