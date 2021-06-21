
rm(list=ls())
source(here::here("0-config.R"))
d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))

#total number targets evaluated in the shared data across the 5 trials
#aka number of specific pathogen/MST prevalence tests
df <- d %>% filter(!grepl("Any",sample),!grepl("any",sample),!grepl("Any",target), !is.na(pos))
nrow(df)
table(df$study)


#total samples in the shared data across the 5 trials
df <- d %>% filter(!grepl("Any",sample),!grepl("any",sample),!grepl("Any",target), !is.na(pos)) %>% 
  group_by(study, sample, clusterid, dataid, round, sampleid) %>% slice(1)
nrow(df)
table(df$study)

d %>% filter(!grepl("Any",sample),!grepl("any",sample),!grepl("Any",target), !is.na(pos)) %>% ungroup() %>% distinct(study, target) %>% as.data.frame()


df2 <- df[df$study=="Fuhrmeister 2020",]
df2 <- df2 %>% group_by(sampleid) %>% mutate(N=n()) %>% filter(N>1) %>% arrange(sampleid) 
df3 <- d %>% distinct(study, sampleid)
table(df3$study)

#number of samples by study
num_samples_by_study <- d %>% filter(!grepl("Any",sample),!grepl("any",sample),!grepl("Any",target), !is.na(pos)) %>% 
  group_by(study, sample, clusterid, dataid, round, sampleid) %>% slice(1) %>%
  group_by(study) %>%
  summarize(N=n()) %>% mutate(study=gsub("","",as.character(study))) %>% arrange(N)
num_samples_by_study

#Distinct samples
d %>% distinct(study, sample) %>% as.data.frame()

#number of food prep samples 
FP <- d %>% filter(sample=="FP") %>% group_by(target)
#FP %>% summarize(N=n(), sum(pos))
FP %>% ungroup() %>% distinct(sampleid) %>% summarise(N=n())

#Number in fuhrmeister journal article
720*3+360



#Load adjusted results
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR_pooled.Rds")) 
sig <- adj_RR %>% ungroup() %>% filter(ci.lb<1 & ci.ub<1 | ci.lb>1 & ci.ub>1) %>% select(sample, target, RR, ci.lb, ci.ub, study)
sig
n_per_sig <- paste0(round(nrow(sig)/nrow(adj_RR %>% filter(!is.na(coef))) * 100, 1),"% (",nrow(sig),"/",nrow(adj_RR %>% filter(!is.na(coef))),")")

#protective <- adj_RR %>% ungroup() %>% filter(!is.na(coef),round(RR,1)<=1) %>% select(sample, target, RR, ci.lb, ci.ub, study)
protective <- adj_RR %>% ungroup() %>% filter(!is.na(coef),RR<=1) %>% select(sample, target, RR, ci.lb, ci.ub, study)
n_protective <- paste0(round(nrow(protective)/nrow(adj_RR %>% filter(!is.na(coef))) * 100, 1),"% (",nrow(protective),"/",nrow(adj_RR %>% filter(!is.na(coef))),")")
n_protective

pooled_adj_RR <- adj_RR %>% filter(study=="Pooled")
adj_RR <- adj_RR %>% filter(study!="Pooled")
perc_no_pos = round(prop.table(table((adj_RR$minN==0)))*100,1)[2]
num_no_pos = paste0(as.character(sum(adj_RR$minN==0, na.rm=T)),"/",as.character(nrow(adj_RR)))
adj_RR <- adj_RR %>% filter(minN!=0)
num_sparse_pos = paste0(as.character(sum(is.na(adj_RR$coef), na.rm=T)),"/",as.character(nrow(adj_RR)))

res<- pooled_adj_RR %>% filter(target=="Any pathogen")


adj_RR %>% ungroup() %>% filter(sample=="any sample type", target=="Any pathogen")  %>% select(sample, target, RR, ci.lb, ci.ub, study)



#Load adjusted results
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR_pooled.Rds")) 
unadj_RR <- readRDS(file=here("results/unadjusted_aim1_RR_pooled.Rds")) 

d<- left_join(adj_RR, unadj_RR, by=c("Y","sample","target","study"))
head(d)

d <- d %>% 
  mutate(log.diff= coef.x-coef.y, log.abs.diff=abs(coef.x-coef.y), diff= RR.x-RR.y, abs.diff=abs(RR.x-RR.y))

summary(d$log.diff)
summary(d$log.abs.diff)
summary(d$diff)
summary(d$abs.diff)

ggplot(d, aes(x=log.diff)) + geom_density()
ggplot(d, aes(x=log.abs.diff)) + geom_density()
ggplot(d, aes(x=exp(log.abs.diff))) + geom_density()


d %>% select(RR.x, RR.y)


