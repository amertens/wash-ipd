
source(here::here("0-config.R"))
library(DataExplorer)
library(SmartEDA)

 

#Check covariates
d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))

Wvars = c( "nrooms","walls", "floor","elec")         

# d %>% group_by(trial) %>% summarize(mean(hhwealth, na.rm=T))
# table(d$trial, is.na(d$hhwealth))
# 



table(d$trial, is.na(d$nrooms))
table(d$trial, is.na(d$walls))
table(d$trial, is.na(d$floor))
table(d$trial, is.na(d$elec))



