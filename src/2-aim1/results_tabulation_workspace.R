
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


df2 <- df[df$study=="Fuhrmeister et al. 2020",]
df2 <- df2 %>% group_by(sampleid) %>% mutate(N=n()) %>% filter(N>1) %>% arrange(sampleid) 
df3 <- d %>% distinct(study, sampleid)
table(df3$study)

#number of samples by study
num_samples_by_study <- d %>% filter(!grepl("Any",sample),!grepl("any",sample),!grepl("Any",target), !is.na(pos)) %>% 
  group_by(study, sample, clusterid, dataid, round, sampleid) %>% slice(1) %>%
  group_by(study) %>%
  summarize(N=n()) %>% mutate(study=gsub(" et al.","",as.character(study))) %>% arrange(N)
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
pooled_adj_RR <- adj_RR %>% filter(study=="Pooled")
adj_RR <- adj_RR %>% filter(study!="Pooled")
perc_no_pos = round(prop.table(table((adj_RR$minN==0)))*100,1)[2]
num_no_pos = paste0(as.character(sum(adj_RR$minN==0, na.rm=T)),"/",as.character(nrow(adj_RR)))
adj_RR <- adj_RR %>% filter(minN!=0)
num_sparse_pos = paste0(as.character(sum(is.na(adj_RR$coef), na.rm=T)),"/",as.character(nrow(adj_RR)))

res<- pooled_adj_RR %>% filter(target=="Any pathogen")


          