
# importing packages and setting directories
source(here::here("0-config.R"))


#Load WBB data
d <- readRDS(paste0(dropboxDir,"WBB/clean/WBB-longform.RDS"))

#Blind treatment assignment
d$tr <- d$tr[sample(d$tr, size=nrow(d))]


#Objective 1








