

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))



#----------------------------------------------------------------------------
#Clean env dataset
#----------------------------------------------------------------------------

village <- read.csv(paste0(dropboxDir,"Data/Odisha/Data set (effect of MST and pathogen marker detection on diarrhea rates)_11-02-14.csv")) %>% filter(!is.na(villageID))
child <- read_dta(paste0(dropboxDir,"Data/Odisha/diarrhoea and weight data Odisha san trial.dta"))

#extract stata labels
makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  labs <- tibble(name = names(labels),
                 label = labels)
  labs$label<- as.character(labs$label)
  return(labs)
}

labs <- makeVlist(child)


head(village)
head(child)

colnames(village)
colnames(child)


#Transform env. data from wide to long
env <- village %>% mutate(env_date=dmy(Date_of_sampling)) %>%
  subset(., select = c(env_date,villageID, interv, Year, Rate_BacUni_Pos_HH_OPT2_2012_2013_without_outlier:Rate_BacCow_Pos_HH_OPT2_2012_2013_without_outlier, Rate_improved_source_Pos_at_least_one_patho_incl_PathoEcoli, Rate_unimproved_source_Pos_at_least_one_patho_incl_PathoEcoli:At_least_one_VC_Pos_in_improved..0.No..1.Yes.))
colnames(env)
colnames(env) <- c("env_date","clusterid","tr","round","BacUni","BacHum","BacCow","any pathogen-improved","any pathogen-unimproved","rv","av","vibrio_cholera")

env <- env %>%
  gather(BacUni:vibrio_cholera, key = target, value = pos) %>%
  filter(!(target %in% c("any pathogen-improved", "any pathogen-unimproved")))

table(env$target)
head(env)
table(is.na(env$pos))
env$rate <- env$pos
env$rate[env$target %in% c("rv","av","vibrio_cholera")] <- NA #mark positivity-only as missing
env$pos <- ifelse(env$pos>0 ,1,0) #Convert rates to positivity

#Rename target to avoid name confusion with avian MST
env$target[env$target=="av"] <- "Adenovirus"
 
env <- env %>% mutate(tr=ifelse(tr==1,"Intervention","Control"),
       tr = factor(tr, levels = c("Control", "Intervention")),
       sample="W",
       study="Odisha",
       dataid=clusterid,
       round=factor(round))

env$sampleid <- env %>% group_indices(clusterid, tr, round, sample) #%>% mutate()

saveRDS(env, file=paste0(dropboxDir,"Data/Odisha/Odisha_env_cleaned.rds"))
