


source(here::here("0-config.R"))




#Load Wash Benefits Bangladesh primary datasets
anthro <- read.csv(paste0(dropboxDir,"WBB/raw/1-primary-outcome-datasets/washb-bangladesh-anthro.csv"))
diar <- read.csv(paste0(dropboxDir,"WBB/raw/1-primary-outcome-datasets/washb-bangladesh-diar.csv"))
enrol <- read.csv(paste0(dropboxDir,"WBB/raw/1-primary-outcome-datasets/washb-bangladesh-enrol.csv"))


#Merge and transform to single long format dataset 

#merge public IDs
pub_ids <- read.csv(paste0(dropboxDir,"WBB/raw/public-ids.csv")) 
head(pub_ids)  

#Load Wash Benefits Bangladesh environmental sample datasets
env_early <- read_dta(paste0(dropboxDir,"WBB/raw/3-env-datasets/washb-bangladesh-early-env-data-public.dta"))
env_mid_end <- read_dta(paste0(dropboxDir,"WBB/raw/3-env-datasets/washb-bangladesh-early-env-data-public.dta"))

#Merge and transform to single long format dataset


#Merge with primary dataset using public IDs



#Load Wash Benefits Bangladesh soil transmitted helminth sample datasets
sth <- read.csv(paste0(dropboxDir,"WBB/raw/2-sth-kk-outcome-datasets/Public/washb-bangladesh-sth-public.csv"))


#Merge with primary dataset using public IDs



# transform to single long format dataset




#Calculate child age at every measure




#Save dataset in dropbox directory
saveRDS(d, paste0(dropboxDir,"WBB/clean/WBB-longform.RDS"))

