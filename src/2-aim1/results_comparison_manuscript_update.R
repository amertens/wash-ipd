
rm(list=ls())
source(here::here("0-config.R"))


unadj_RR <- readRDS(file=here("results/unadjusted_aim1_RR_pooled.Rds")) 
unadj_RD <- readRDS(file=here("results/unadjusted_aim1_RD.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR_pooled.Rds")) 
adj_RD <- readRDS(file=here("results/adjusted_aim1_RD.Rds")) 

d1 <- readRDS(file=here("results/adjusted_aim1_RR.Rds")) 
d2 <- readRDS(file=here("results/adjusted_aim1_RR_old.Rds")) 

d1 %>% filter(target=="Any STH", sample=="any sample type", study=="Capone 2021")
d2 %>% filter(target=="Any STH", sample=="any sample type", study=="Holcomb 2020")


d1 %>% filter(target=="Any STH", sample=="LS", study=="Capone 2021")
d2 %>% filter(target=="Any STH", sample=="S", study=="Holcomb 2020")


d3 <- readRDS(file=here("results/unadjusted_aim1_RR.Rds")) 
d4 <- readRDS(file=here("results/unadjusted_aim1_RR_old.Rds")) 

d3 %>% filter(target=="Any STH", sample=="LS", study=="Capone 2021")
d4 %>% filter(target=="Any STH", sample=="S", study=="Holcomb 2020")


#NOTE! Adjusted estimates have changes due to updated covariates, unadjusted have not
# Any sample type have changed based on aggregating by HHID now instead of just compound- Change? Or just change for Aim1?

#Capone STH significance has changed after separating out into 2 studies and changing adjustment covariates


#NOTE! Data is getting dropped in the adjusted analyses. Check for missing covariates. See:

adj_RR %>% filter(target=="Pathogenic E. coli", sample=="any sample type", study=="Fuhrmeister 2020")
unadj_RR %>% filter(target=="Pathogenic E. coli", sample=="any sample type", study=="Fuhrmeister 2020")

temp <- adj_RR %>% filter(target=="Pathogenic E. coli", sample=="any sample type", study=="Fuhrmeister 2020")
temp$W


adj_RR %>% filter(target=="Animal (BacCow)",study=="Fuhrmeister 2020")
unadj_RR %>% filter(target=="Animal (BacCow)",study=="Fuhrmeister 2020")


temp <- adj_RR %>% filter(a<10 | c < 10)
temp


temp<-adj_RR %>% filter(study=="Holcomb 2020")

adj_RR %>% filter(target=="Human (HF183)",  study=="Holcomb 2020")
unadj_RR %>% filter(target=="Human (HF183)",  study=="Holcomb 2020")

temp <- adj_RR %>% filter(target=="Human (HF183)",  study=="Holcomb 2020", sample=="any sample type")
temp$W