
rm(list=ls())
source(here::here("0-config.R"))


#-----------------------------------------------------------
# Merge WBK
#-----------------------------------------------------------


anthro <- read_dta(paste0(dropboxDir, "Data/WBK/washb-kenya-anthro.dta"))
sth <- read_dta(paste0(dropboxDir,"Data/WBK/wbk_STH_soil.dta"))


table(anthro$tr)
anthro %>% group_by(tr) %>% summarize(N=n(), mean(laz,na.rm=T))

#merge to just the hhid with STH
sth <- sth %>% subset(., select=c(hhid, block, 
                                  dummy_sth,
                                  dummy_ascaris,
                                  dummy_trichuris)) %>%
  gather(dummy_sth:dummy_trichuris, key = target, value = pos) %>%
  mutate(target=gsub("dummy_","", target)) %>% filter(target!="sth", !is.na(pos)) 

head(sth)


head(anthro)

dim(anthro)
anthro_sth <- left_join(sth, anthro, by=c("hhid","block"))
dim(anthro_sth)
anthro_sth %>% group_by(tr) %>% summarize(N=n(), mean(laz,na.rm=T))


anthro_sth <- anthro_sth %>%
  mutate(tr2 = factor(tr, labels=c("Control", #save seperate treatment arm
                                   "Sanitation",
                                   "WSH")),
         #combine Sanitation and WSH arm for primary analysis),
         tr = case_when(tr==1 ~ "Control",
                        tr==3 | tr==5 ~ "Sanitation"
         ))
anthro_sth %>% group_by(tr) %>% summarize(N=n(), mean(laz,na.rm=T))
anthro_sth %>% group_by(tr2) %>% summarize(N=n(), mean(laz,na.rm=T))

