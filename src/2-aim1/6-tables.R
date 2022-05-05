

rm(list=ls())
source(here::here("0-config.R"))
library(table1)
library(rvest)


#Clean results function
clean_est <- function(res, outcome="binomial"){
  if(outcome=="binomial"){
    est = round(res$RR, 2)
    ci.lb = round(res$ci.lb, 2) 
    ci.ub = round(res$ci.ub, 2)
    est.ci = paste0(est," (",ci.lb,", ",ci.ub,")")
  }else{
    est = round(res$coef, 2)
    ci.lb = round(res$ci.lb, 2) 
    ci.ub = round(res$ci.ub, 2)
    est.ci = paste0("difference= ",est," (95% CI: ",ci.lb,", ",ci.ub,")")
  }
  return(est.ci)
}



adj_RR <- readRDS(file=here("results/adjusted_aim1_RR_pooled.Rds")) 
d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- d %>% mutate(
  sample =case_when(
              sample == "any sample type" ~ "Any sample",
              sample == "SW" ~ "Source water",
              sample == "W" ~ "Stored water",
              sample == "CH" ~ "Child hand rinse",
              sample == "MH" ~ "Mother's hand rinse",
              sample == "Fly" ~ "Flies",
              sample == "LS" ~ "Latrine soil",
              sample == "S" ~ "House soil"
            ), 
              sample = factor(sample, 
                                levels=c("Any sample","Source water","Stored water",
                                         "Child hand rinse", "Mother's hand rinse", "Latrine soil",
                                         "House soil", "Flies", "Sparse data"))) %>%
            droplevels()

adj_RR <- adj_RR %>% mutate(
  sample =case_when(
    sample == "any sample type" ~ "Any sample",
    sample == "SW" ~ "Source water",
    sample == "W" ~ "Stored water",
    sample == "CH" ~ "Child hands",
    sample == "MH" ~ "Mother's hands",
    sample == "FlyKitch" ~ "Flies in kitchen",
    sample == "FlyLat" ~ "Flies in latrine",
    sample == "LS" ~ "Latrine soil",
    sample == "S" ~ "House soil"
  ), 
  sample = factor(sample, 
                  levels=c("Any sample","Source water","Stored water",
                           "Child hands", "Mother's hands", "Latrine soil",
                           "House soil", "Flies in kitchen",  "Flies in latrine", "Sparse data"))) %>%
  droplevels()


d %>% arrange(study, sample, target) %>%
  distinct(study, sample, target)

WBB_tab <- d %>% group_by(tr, sample, target) %>% filter(study=="WBB", !is.na(pos)) %>%
  summarise(N=n(), n=sum(pos, na.rm=T), 
            prev=round(mean(pos, na.rm=T)*100, 1), 
            abund=mean(log10(abund), na.rm=T))

d <- d %>% filter(!is.na(target) & !is.na(sample) & !is.na(pos))

#Drop baseline measure from mapsan
d <- d %>% filter(round != "bl") %>% droplevels(.)
table(d$study, d$round)
table(d$sample, d$target, d$study)
unique(d$target)

#Get N samples
head(d)

 d %>% distinct(study, sampleid, dataid, hhid) %>% 
  #filter(sample!="Any sample type") %>%
  group_by(study) %>%
  summarize(N=n())


# any_pathogens = c("Any pathogens","E. coli virulence gene",  "Pathogenic E. coli", "Giardia",  "C. difficile",
#                   "Shigella",  "Entamoeba histolytica",  "V. cholerae", "Yersinia",       
#                   "Norovirus",             "Any STH", "Ascaris",
#                   "Adenovirus","Trichuris",  "Rotavirus", "Astrovirus", "Cryptosporidium", "Salmonella")   
# 
# any_virus = c("Any virus","Norovirus",  "Adenovirus", "Rotavirus", "Astrovirus")   
# any_bacteria = c("Any bacteria","E. coli virulence gene", "Pathogenic E. coli", "Yersinia",  "V. cholerae", "Shigella",  "C. difficile",  "Salmonella")   
# any_helminth = c("Any STH", "Ascaris", "Trichuris")   
# any_protozoa = c("Giardia", "Cryptosporidium", "Entamoeba histolytica")   
# 
# 
# #MST's:
# general_MST = c("Any general MST","GenBac3")
# animal_MST = c( "Any animal MST", "BacCow",   
#                 "Ruminant",              "Avian",
#                 "Avian (Helicobacter)")
# human_MST = c("Any human MST","HumM2",  "Human (Bacteroides)",   "Human (M. smithii)")

#Add target category
d <- d %>% mutate(
  target_cat = case_when(
    target %in% any_virus ~ "Virus",  
    target %in% any_bacteria ~ "Bacteria",  
    target %in% any_helminth ~ "Helminth",  
    target %in% any_protozoa ~ "Protozoa",  
    target %in% animal_MST ~ "Animal",  
    target %in% human_MST ~ "Human"#,  
    #target %in% general_MST ~ "General" 
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
target_presence_long_prep <- d %>% filter(!grepl("Any ",target), !grepl("any ",sample), !grepl("Any ",sample)) %>%
  group_by(study,sample, target, target_cat, target_type) %>% summarize(N=n(), n=sum(pos), perc=round(mean(pos, na.rm=T)*100,1)) %>% 
  ungroup() %>%
  #mutate(target=paste0(target," (",target_cat,", ",n,"/",N,")")) %>% 
  mutate(
    N_perc =paste0(perc,"% (",n,"/",N,")")) %>% 
  arrange(target_type, study, sample, target_cat, target, perc) 

#merge in PRs
adj_PR <- adj_RR %>% filter(!is.na(coef))
adj_PR$est.ci <- clean_est(adj_PR)
adj_PR <- adj_PR %>% subset(., select = c("study","sample","target","est.ci"))

target_presence_long <- left_join(target_presence_long_prep, adj_PR, by = c("study","sample","target"))
target_presence_long$est.ci <- ifelse(is.na(target_presence_long$est.ci),"-",target_presence_long$est.ci)



target_presence_long <- target_presence_long%>%
  group_by(study, target_type) %>% 
  mutate(n=row_number()) %>%
  group_by(study, sample, target_type) %>% 
  mutate(n2=row_number(),
         study=ifelse(n==1,as.character(study),"-")
         ) %>%
  group_by(study, sample) %>% 
  mutate(n3=row_number(),
         sample=ifelse(n2==1,as.character(sample),"-")
  ) %>%
  ungroup()


target_presence_long_P <- target_presence_long %>% 
  filter(target_type=="P") %>%
  select(study,sample, target, N_perc, est.ci) 
colnames(target_presence_long_P) <- str_to_title(colnames(target_presence_long_P))
colnames(target_presence_long_P)[4] <- "Percent positive (n/N)"
colnames(target_presence_long_P)[5] <- "PR (95% CI)"
target_presence_long_P

target_presence_long_MST <- target_presence_long %>% 
  filter(target_type=="MST") %>%
  select(study,sample, target , N_perc, est.ci)
colnames(target_presence_long_MST) <- str_to_title(colnames(target_presence_long_MST))
colnames(target_presence_long_MST)[4] <- "Percent positive (n/N)"
colnames(target_presence_long_MST)[5] <- "PR (95% CI)"




#table1
tab1 <- table1(~target+sample |study, format_number = TRUE, data=d)
#tab1 <- as.data.frame(read_html(tab1) %>% html_table(fill=TRUE))




#covariate table
Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")         
Wvars[!(Wvars %in% colnames(d))]


#subset to HH level obs and covariates
head(d)
df <- d %>% 
  filter(!grepl("Any ",target), sample!="Any sample") %>%
  group_by(study, dataid, hhid) %>%
  slice(1) %>% ungroup() %>%
  subset(., select = c("study", Wvars)) %>% filter(study!="Odagiri 2016")


#Clean/harmonize covariates 
summary(df$walls)
summary(df$floor)
summary(df$elec)

table(df$study, df$elec)

df$walls2 <- ifelse(as.numeric(as.character(df$walls)) >= 0.5, "1","0")
df$floor2 <- ifelse(as.numeric(as.character(df$floor)) >= 0.5, "1","0")
df$elec2 <- ifelse(as.numeric(as.character(df$elec)) >= 0.5, "1","0")
df$walls2[is.na(df$walls2)] <- "Missing"
df$elec2[is.na(df$elec2)] <- "Missing"
df$floor2[is.na(df$floor2)] <- "Missing"


table(df$walls2)
table(df$elec2)
table(df$floor2)



#rename covariates
df2 <- df %>% 
  subset(., select = -c(walls, floor, elec)) %>%
  rename(
  `Household\nwealth`=hhwealth, 
  `Number of people\nin the household`=Nhh,
  `Number of rooms\nin the household`=nrooms,
  `Improved wall`=walls2, 
  `Improved floor`=floor2,
  `Improved roof`=roof,
  `Electricity`=elec2,
  `Father in\nagriculture`=dadagri,
  `Land owned`=landown, 
  `Acres of\nland owned`=landacre, 
  `Maternal\neducation`=momedu, 
  `Maternal\nage`=momage)

tab2 <- table1(~. |study, format_number = TRUE, data=df)
tab2 <- as.data.frame(read_html(tab2) %>% html_table(fill=TRUE))
#Drop overall column
tab2 <- tab2[,!grepl("Overall",colnames(tab2))]
tab2 <- tab2[-c(1:8),]
colnames(tab2) <- paste0(str_split(colnames(tab2),"\\.", simplify = T)[,1]," ", str_split(colnames(tab2),"\\.", simplify = T)[,2])
colnames(tab2)[1] <- "."
colnames(tab2)[8] <- "Capone 2022 in prep."      

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
#general_MST_tab <- tab_function(general_MST)
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
#general_MST_tab2 <- tab_function2(df=df2, general_MST)
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
     animal_MST_tab, human_MST_tab,
     any_pathogens_tab2, any_virus_tab2, any_bacteria_tab2, any_helminth_tab2, any_protozoa_tab2,
     animal_MST_tab2, human_MST_tab2,
     file=here("figures/all_tables.Rdata"))









