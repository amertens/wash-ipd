
source(here::here("0-config.R"))

mapsan <- readRDS(paste0(dropboxDir,"Data/MapSan/mapsan_env_cleaned.rds"))
mapsan <- mapsan %>% mutate(study="mapsan")

WBB <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBB_env.RDS"))
WBB <- WBB %>% mutate(study="WBB")


colnames(mapsan)
colnames(WBB)


WBB$sampleid<-as.character(WBB$sampleid)
mapsan$tr<-as.character(mapsan$tr)
mapsan$momedu<-factor(mapsan$momedu)
d <- bind_rows(WBB, mapsan)

saveRDS(d, file=paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
