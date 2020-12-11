
rm(list=ls())
source(here::here("0-config.R"))

mapsan <- readRDS(paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds"))
mapsan <- mapsan %>% mutate(study="mapsan")

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

#NOTE!!! Figure out what pos=2 is 
d <- d %>% filter(pos!=2)



#create aggregate outcomes
head(d)
d %>% distinct(study, target)


#create aggregate outcomes


XXXXXXXXXXXXXXXXXXXXXX
Note:
  Need to keep covariates in the individual data frame because of sammple-specific vars
  and just make a cov dataframe with hh-level vars merged into the aggregate dateset
 plus fix dataframe names
 Note 2: I updated code to do this, but causing duplicated in the aggregate dataframe... one covariate must vary across HH... check maybe maternal?
XXXXXXXXXXXXX

#covariates
cov <- d %>% subset(., select = -c(target, pos, abund, abund_only_detect, censored)) %>% distinct(.)
d_agg <- d %>% group_by(study, tr,  dataid, clusterid, round, type) %>%
  summarise(any_entero = 1*(sum(pos)>0),
         any_human_MST = 1*(sum(pos==1 & target %in% c("Hum", "HF183","Mnif","sth"))>0),
         any_animal_MST = 1*(sum(pos==1 & target %in% c("BC","br","av","GFD"))>0)
         ) %>% ungroup()
table(d_agg$any_entero)
table(d_agg$any_human_MST)
table(d_agg$any_animal_MST)

dim(d)
dim(d_ind)
dim(d_agg)
dim(cov)
df <- left_join(d_agg, cov, by=c("study","tr","dataid","clusterid","round","type"))
dim(df)
df <- bind_rows(d, df)
dim(df)



#mark aggregate outcomes
d <- d %>% mutate(
      aggregate_Y = case_when(
        target %in% c("sth","any_entero","any_human_MST","any_animal_MST") ~ 1,
        !(target %in% c("sth","any_entero","any_human_MST","any_animal_MST")) ~ 0
        ))


saveRDS(d, file=paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
