

rm(list=ls())
source(here::here("0-config.R"))
library(table1)
library(rvest)



d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))

d <- d %>% filter(!is.na(abund)) %>% droplevels(.)

tab <- d %>% group_by(study, target, sample, qual) %>%
  droplevels(.) %>%
  summarise(N=n(), med=round(median(abund),1)) %>%
  mutate(val=paste0(N, " (",med,")")) %>%
  select(study, target, sample, qual, val) %>%
  pivot_wider(id_cols= c("study","target","qual"), names_from = sample , values_from = val) %>%
  subset(., select = -c(`any sample type`)) %>%
  as.data.frame()
tab$qual[is.na(tab$qual)] <- "BLOD"

table(d$qual)
d <- d %>% group_by(study, sample, target) %>% mutate(N=n(), Nmiss=sum(is.na(qual))) %>%
  mutate(qual = case_when(
    N==Nmiss & is.na(qual) ~ "ROQ",
    N!=Nmiss & is.na(qual) ~ "BLOD",
    !is.na(qual) ~ qual))
d$qual <- factor(d$qual, levels=c("BLOD", "BLOQ",  "ROQ"))
table(d$qual)
table(paste0(d$study, " ",d$sample," ",d$target), is.na(d$qual))
table(paste0(d$study, " ",d$sample," ",d$target), (d$qual))

head(d)

#d <- d %>% filter(study=="Boehm et al. 2016", sample=="Ascaris", target=="Ascaris")

norm_fun <- function(d){
  
  #add in number of BLOQ and BLOD
  Nqual <- table(d$qual)
  prop.qual <- round(prop.table(table(d$qual)), 3) * 100
  
  d <- d %>% mutate(N=n()) %>% filter(qual=="ROQ")
  res <- d %>% summarise(N=N[1], 
                         mean=round(mean(abund),1), med=round(median(abund),1), 
                         trans.mean=round(mean(log10(abund+0.1)),1), trans.med=round(median(log10(abund+0.1)),1))
  
  res$raw.p <- res$trans.p <- NA
  try(res$raw.p <- round(shapiro.test(d$abund)$p.value, 5))
  try(res$trans.p <- round(shapiro.test(log10(d$abund))$p.value, 5))
  
  #res <- data.frame(res, as.data.frame(t(as.matrix(Nqual))), as.data.frame(t(as.matrix(prop.qual)))) 
  res$ROQ = paste0(prop.qual[3]," (",Nqual[3],")")
  res$BLOQ = paste0(prop.qual[2]," (",Nqual[2],")")
  res$BLOD = paste0(prop.qual[1]," (",Nqual[1],")")
  
  res <- res %>%
    arrange(N, ROQ, BLOQ, BLOD, mean, med, trans.mean, trans.med, trans.p, raw.p) 
  return(res)
}


abund_ROQ <- d %>% filter(!is.na(abund)) %>% droplevels()%>%
  group_by(study, sample, target) %>% do(norm_fun(.))

abund_ROQ
save(abund_ROQ, file=here("figures/abundance_table_ROQ.Rdata"))

write.csv(abund_ROQ, file=here("figures/abundance_table_ROQ.csv"))
