
source(here::here("0-config.R"))
library(DataExplorer)
library(SmartEDA)

 

#EDA - aim 1 outcomes

#Notes:
   #-log quant is missing for non-pos in mapsan but set to log(0.5) in washb


#----------------------------------------------------
# MapSan
#----------------------------------------------------

d <-readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
d <- d %>% subset(., select = c(dataid, study,type, target, round, pos, logquant))
head(d)

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
wbk <- d %>% filter(study=="WBK")

wbk %>% group_by(type, target) %>%
  filter(!is.na(type)) %>%
  summarize(N=n(), npos=sum(pos), prev=round(mean(pos),3)*100, mean_log_quant=round(mean(logquant, na.rm=T),2)) %>%
  as.data.frame()


#----------------------------------------------------
# Gram Vikas
#----------------------------------------------------

gv <-readRDS(paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds"))
head(gv)







