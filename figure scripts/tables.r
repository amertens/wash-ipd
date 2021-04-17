

rm(list=ls())
source(here::here("0-config.R"))
library(table1)
library(rvest)



d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)
d %>% arrange(study, sample, target) %>%
  distinct(study, sample, target)

WBB_tab <- d %>% group_by(tr, sample, target) %>% filter(study=="WBB", !is.na(pos)) %>%
  summarise(N=n(), n=sum(pos, na.rm=T), 
            prev=round(mean(pos, na.rm=T)*100, 1), 
            abund=mean(log10(abund), na.rm=T))

d <- d %>% filter(!is.na(target) & !is.na(sample) & !is.na(pos))

#Drop baseline measure from mapsan
d <- d %>% filter(round != "0m") %>% droplevels(.)
table(d$study, d$round)
table(d$sample, d$target, d$study)



any_pathogens = c("Any pathogens","E. coli virulence gene",  "Pathogenic E. coli", "Giardia",  "C. difficile",
                  "Shigella",  "Entamoeba histolytica",  "V. cholerae", "Yersinia",       
                  "Norovirus",             "Any STH", "Ascaris",
                  "Adenovirus","Trichuris",  "Rotavirus", "Astrovirus", "Cryptosporidium", "Salmonella")   

any_virus = c("Any virus","Norovirus",  "Adenovirus", "Rotavirus", "Astrovirus")   
any_bacteria = c("Any bacteria","E. coli virulence gene", "Pathogenic E. coli", "Yersinia",  "V. cholerae", "Shigella",  "C. difficile",  "Salmonella")   
any_helminth = c("Any STH", "Ascaris", "Trichuris")   
any_protozoa = c("Giardia", "Cryptosporidium", "Entamoeba histolytica")   


#MST's:
general_MST = c("Any general MST","GenBac3")
animal_MST = c( "Any animal MST", "BacCow",   
                "Ruminant",              "Avian",
                "Avian (Helicobacter)")
human_MST = c("Any human MST","HumM2",  "Human (Bacteroides)",   "Human (M. smithii)")

#Add target category
d <- d %>% mutate(
  target_cat = case_when(
    target %in% any_virus ~ "V",  
    target %in% any_bacteria ~ "B",  
    target %in% any_helminth ~ "H",  
    target %in% any_protozoa ~ "P",  
    target %in% animal_MST ~ "An",  
    target %in% human_MST ~ "Hum",  
    target %in% general_MST ~ "Gen" 
  ),
  target_type = case_when(
    target %in% any_pathogens ~ "P",  
    !(target %in% any_pathogens) ~ "MST"
  ))

#target presence by sample and study
target_presence <- d %>% filter(!grepl("Any ",target), !grepl("any ",sample)) %>%
  group_by(study,sample, target, target_cat, target_type) %>% summarize(N=n(), n=sum(pos)) %>% mutate(target=paste0(target," (",target_cat,", ",n,"/",N,")")) %>%
  group_by(study, sample, target_type) %>% 
  mutate(target = paste0(target, collapse = ", ")) %>% slice(1) %>% 
  ungroup() 

target_presence_P <- target_presence %>% filter(target_type=="P") %>% select(study,sample, target) %>% spread(sample, target)
target_presence_MST <- target_presence %>% filter(target_type=="MST") %>% select(study,sample, target) %>% spread(sample, target)
target_presence_P[is.na(target_presence_P)] <- ""
target_presence_MST[is.na(target_presence_MST)] <- ""


#target presence by sample and study - longform
target_presence_long <- d %>% filter(!grepl("Any ",target), !grepl("any ",sample)) %>%
  group_by(study,sample, target, target_cat, target_type) %>% summarize(N=n(), n=sum(pos)) %>% 
  ungroup() %>%
  #mutate(target=paste0(target," (",target_cat,", ",n,"/",N,")")) %>% 
  mutate(target=paste0(target," (",n,"/",N,")")) %>% 
  arrange(target_type, study, sample, target_cat, target) %>%
  group_by(study, target_type) %>% 
  mutate(n=row_number()) %>%
  group_by(study, sample, target_type) %>% 
  mutate(n2=row_number(),
         study=ifelse(n==1,as.character(study),""),
         sample=ifelse(n2==1,as.character(sample),"-")) %>%
  ungroup()


target_presence_long_P <- target_presence_long %>% 
  filter(target_type=="P") %>%
  select(study,sample, target) 
colnames(target_presence_long_P) <- str_to_title(colnames(target_presence_long_P))
colnames(target_presence_long_P)[3] <- paste0(colnames(target_presence_long_P)[3], " (n/N)")
target_presence_long_MST <- target_presence_long %>% 
  filter(target_type=="MST") %>%
  select(study,sample, target)
colnames(target_presence_long_MST) <- str_to_title(colnames(target_presence_long_MST))
colnames(target_presence_long_MST)[3] <- paste0(colnames(target_presence_long_MST)[3], " (n/N)")




#table1
tab1 <- table1(~target+sample |study, format_number = TRUE, data=d)
#tab1 <- as.data.frame(read_html(tab1) %>% html_table(fill=TRUE))




#covariate table
Wvars = c("hhwealth", "Nhh", "momedu", "hfiacat",
          "nrooms","walls", "floor","elec") 
Wvars[!(Wvars %in% colnames(d))]
df <- d %>% subset(., select = c("study", Wvars))
tab2 <- table1(~. |study, format_number = TRUE, data=df)
#tab2 <- as.data.frame(read_html(tab2) %>% html_table(fill=TRUE))

#Prevalence and abundance of outcomes by sample sample
df <- d %>% group_by(study, target, sample) %>% filter(!is.na(pos)) %>%
  summarize(N=sum(!is.na(pos)), prev=mean(pos, na.rm=T), abund=mean(abund, na.rm=T)) %>%
  mutate(prev=round(prev,1), abund=round(abund,1))
df2 <- d %>% group_by(study,round, target, sample) %>% filter(!is.na(pos)) %>%
  summarize(N=sum(!is.na(pos)), prev=mean(pos, na.rm=T), abund=mean(abund, na.rm=T)) %>%
  mutate(prev=round(prev,1), abund=round(abund,1))

unique(df$sample)
unique(df$target)




tab_function <- function(targets){
  tabdf <- df %>% filter(target %in% targets) %>% mutate(target=factor(target, levels = targets)) %>% arrange(target)
  Ntargets <- length(unique(tabdf$sample))
  tabdf <- tabdf %>% pivot_wider(id_cols=c(study,target),names_from = c(sample), values_from = c(N, prev, abund), names_sort=T) %>% mutate_all(as.character)
  tabdf <- as.data.frame(tabdf)
  for(i in 1:Ntargets){
    tabdf[,2 + Ntargets + i] <- paste0(as.character(tabdf[,2 + Ntargets + i]),"% (", as.character(tabdf[,2+i]),")")
  }
  tabdf <- tabdf[,!grepl("N_",colnames(tabdf))]
  tabdf[is.na(tabdf)] <- ""
  tabdf[tabdf=="NaN"] <- ""
  tabdf[tabdf=="NA% (NA)"] <- ""
  
  return(tabdf)
}

any_pathogens_tab <- tab_function(any_pathogens)
any_virus_tab <- tab_function(any_virus)
any_bacteria_tab <- tab_function(any_bacteria)
any_helminth_tab <- tab_function(any_helminth)
any_protozoa_tab <- tab_function(any_protozoa)
general_MST_tab <- tab_function(general_MST)
animal_MST_tab <- tab_function(animal_MST)
human_MST_tab <- tab_function(human_MST)


tab_function2 <- function(df, targets){
  tabdf <- df %>% filter(target %in% targets) %>% mutate(target=factor(target, levels = targets)) %>% arrange(target)
  tabdf <- tabdf %>% pivot_wider(id_cols=c(study,target, round),names_from = c(sample), values_from = c(prev, abund), names_sort=T) %>% mutate_all(as.character)
  tabdf[is.na(tabdf)] <- ""
  tabdf[tabdf=="NaN"] <- ""
  
  return(tabdf)
}

any_pathogens_tab2 <- tab_function2(df=df2, any_pathogens)
any_virus_tab2 <- tab_function2(df=df2, any_virus)
any_bacteria_tab2 <- tab_function2(df=df2, any_bacteria)
any_helminth_tab2 <- tab_function2(df=df2, any_helminth)
any_protozoa_tab2 <- tab_function2(df=df2, any_protozoa)
general_MST_tab2 <- tab_function2(df=df2, general_MST)
animal_MST_tab2 <- tab_function2(df=df2, animal_MST)
human_MST_tab2 <- tab_function2(df=df2, human_MST)

#Get control-arm specific for WBB comparisons
table(d$tr)
df <- d %>% filter(tr=="Control", study=="WBB") %>%
  group_by(study, target, sample) %>%
  summarize(N=sum(!is.na(pos)), prev=mean(pos, na.rm=T)*100, abund=mean(abund, na.rm=T)) %>%
  mutate(prev=round(prev,1), abund=round(abund,1))
#tab_function(c(any_pathogens,general_MST,animal_MST,human_MST ))

save(target_presence_P, target_presence_MST, tab1, tab2,
     target_presence_long_P, target_presence_long_MST,
     any_pathogens_tab, any_virus_tab, any_bacteria_tab, any_helminth_tab, any_protozoa_tab,
     general_MST_tab, animal_MST_tab, human_MST_tab,
     any_pathogens_tab2, any_virus_tab2, any_bacteria_tab2, any_helminth_tab2, any_protozoa_tab2,
     general_MST_tab2, animal_MST_tab2, human_MST_tab2,
     file=here("figures/all_tables.Rdata"))








