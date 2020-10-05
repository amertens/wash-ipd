
source(here::here("0-config.R"))
library(DataExplorer)
library(SmartEDA)

 

#EDA - aim 1 outcomes


#----------------------------------------------------
# MapSan
#----------------------------------------------------

d <-readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
ms <- d %>% filter(study=="mapsan")
head(ms)



summary(ms$logquant)
summary(exp(ms$logquant))

#Pathogens:
# cEC: cultured E. coli
# EC23S: qPCR E. coli
# HF183: qPCR human target (Bacteroides)
# Mnif: qPCR human target (M. smithii)
# GFD: qPCR avian target (Helicobacter)

# types:
# ds=household entrance soil
# fp=food preparation surface
# hwhousehold stored water  
# ls=latrine entrance soil 
# wp=source water


#check if they


ms %>% group_by(type, target) %>%
  summarize(N=n(), npos=sum(pos), prev=round(mean(pos),3)*100, mean_log_quant=round(mean(logquant, na.rm=T),2)) %>%
  as.data.frame()

#Now tabulate by human or animal source

#Are the GPP measures just in children? -yes according to papers


#----------------------------------------------------
# WBB
#----------------------------------------------------
wbb <- d %>% filter(study=="WBB")
head(wbb)
wbb %>% group_by(type, target) %>%
  filter(!is.na(type)) %>%
  summarize(N=n(), npos=sum(pos), prev=round(mean(pos),3)*100, mean_log_quant=round(mean(logquant, na.rm=T),2)) %>%
  as.data.frame()


#----------------------------------------------------
# WBK
#----------------------------------------------------
wbk <- readRDS(paste0(dropboxDir, "Data/WBB/Clean/WBK_env.RDS"))

wbk %>% summarize_all(mean) %>% as.data.frame()


#----------------------------------------------------
# Gram Vikas
#----------------------------------------------------

gv <-readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds"))
head(gv)







