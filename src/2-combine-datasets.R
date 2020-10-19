
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

#mapsan <- mapsan %>% rename(logquant=log_conc)
WBB <- WBB %>% rename(logquant=log_conc)
WBK <- WBK %>% rename(logquant=log_conc, dataid=hhid, Nhh=num_hh, hhwealth=assetquintile, sampleid=soil_id) %>%
  mutate(round="STH round")

WBB <- WBB %>% subset(., select = c(study, sampleid, dataid, tr, type, target, pos, logquant, round, block, Nhh, momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  floor, hhwealth)) %>%
              mutate( tr = factor(tr, levels = c("Control", "Sanitation")))
WBK <- WBK %>% subset(., select = c(study, sampleid, dataid, tr, type, target, pos, logquant, round, block, Nhh, 
                                    #momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  
                                    floor, hhwealth))
mapsan <- mapsan %>% subset(., select = c(study, sampleid, dataid, tr, type, target, pos, logquant, round,  block, Nhh, momage, momheight, momedu, dadagri,landacre, hfiacat,watmin,  floor, hhwealth)) %>%
                    mutate(tr=case_when(
                      tr=="0" ~ "Control",
                      tr=="1" ~ "Sanitation"
                    ),
                    tr = factor(tr, levels = c("Control", "Sanitation"))#,
                    # case_when(
                    #   type == "" ~ ""
                    # )
                    )

  

WBB$sampleid<-as.character(WBB$sampleid)
WBK$sampleid<-as.character(WBK$sampleid)
mapsan$momedu<-factor(mapsan$momedu)
d <- bind_rows(WBB, WBK, mapsan)
colnames(d)

d %>% distinct(study, type, target)


table(d$pos)
d <- d %>% filter(pos!=2)



saveRDS(d, file=paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
