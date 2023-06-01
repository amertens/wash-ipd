


rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data.rds"))

table(d$study, d$ch_pos_cdiff)

#drop food samples
d <- d %>% filter(sample!="FP") %>% droplevels()
unique(d$target)

#vector of covariates
Wvars = c("sex","age","hfiacat","momage","hhwealth_cont", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth_cont", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         

all_Wvars <- unique(c(Wvars, Wvars_anthro))




#----------------------------------
# Clean covariates
#----------------------------------

table(d$study, d$hhwealth)
table(d$study, d$Nhh)
table(d$study, d$nrooms)

d <- d %>% group_by(trial) %>% 
  mutate(#hhwealth=factor(ntile(hhwealth,4), levels=c("1","2","3","4")),
         hhwealth=fct_explicit_na(hhwealth, na_level = "Missing"),
         # Nhh=factor(case_when(
         #   Nhh<5 ~ "<5",
         #   Nhh>=5 & Nhh <=8 ~ "5-8",
         #   Nhh>8 ~ ">8"
         # ), levels=c("5-8","<5",">8")),
         Nhh=fct_explicit_na(Nhh, na_level = "Missing"),
         # nrooms=as.numeric(nrooms),
         # nrooms=factor(case_when(
         #   nrooms<3 ~ "1-2",
         #   nrooms>2  ~ ">3",
         # ), levels=c("1-2",">3")),
         nrooms=fct_explicit_na(nrooms, na_level = "Missing"),
         landown=fct_explicit_na(landown, na_level = "Missing"),
         dadagri=factor(dadagri),
         dadagri=fct_explicit_na(dadagri, na_level = "Missing"),
         momedu=factor(momedu),
         momedu=fct_explicit_na(momedu, na_level = "Missing"),
         hfiacat=factor(hfiacat),
         hfiacat=fct_explicit_na(hfiacat, na_level = "Missing"),
         walls=factor(walls),
         walls=fct_explicit_na(walls, na_level = "Missing"),
         roof=factor(roof),
         roof=fct_explicit_na(roof, na_level = "Missing"),
         floor=factor(floor),
         floor=fct_explicit_na(floor, na_level = "Missing"),
         elec=factor(elec),
         elec=fct_explicit_na(elec, na_level = "Missing")
  )

table(d$study, d$floor)


# 1.	Child birth order/parity -aim 2 only

table(d$study, d$sex) 
table(d$study, is.na(d$sex)) 

d$sex <- factor(d$sex)

# 2.	Asset-based wealth index 
table(d$hhwealth) 
table(d$study, d$hhwealth) #check Reese and Holcomb missing
table(d$study, is.na(d$hhwealth))

# 3.	Number of individuals and children in household
table(d$Nhh) 
d$Nhh <- factor(d$Nhh, levels=c("<5","5-8",">8", "Missing"))
table(d$study, d$Nhh) #check Reese and Holcomb missing
table(d$study, is.na(d$Nhh))

# 4.	Household food security -aim 2 only
levels(d$hfiacat) 
table(d$study, d$hfiacat) 
table(d$study, is.na(d$hfiacat)) #add to reese and steinbaum


# 5.	Household electrification and construction, including wall/roof material
table(d$study, d$elec) #check missing in Holcomb
table(d$study, is.na(d$elec))
table(d$study, d$walls) #check missing in Holcomb
table(d$study, is.na(d$walls))
table(d$study, d$floor) #check missing in Holcomb
table(d$study, is.na(d$floor))

table(d$study, d$nrooms) #check missing in WBB
table(d$study, is.na(d$nrooms))

# 6.	Parental age -aim 2 only
table(d$study, d$momage)
table(d$study, is.na(d$momage))
# 7.	Parental education 
levels(d$momedu)
d$momedu <- factor(d$momedu, levels=c("No education", "Incomplete Primary", "Primary",  "Secondary", "More than secondary", "Missing"))
table(d$study, d$momedu)
table(d$study, is.na(d$momedu)) #add to Steinbaum and check Holcomb
# 8.	Parental employment  - check all
# a.	Indicator for works in agriculture 
table(d$study, d$dadagri)
table(d$study, is.na(d$dadagri)) #add missingness check Steinbaum

# 9.	Land ownership 
table(d$study, is.na(d$landown)) #check Steinbaum, reese

# Land acreage 
table(d$study, is.na(d$landacre)) #check Steinbaum, reese



#----------------------------------
# Check covariates missingness
#----------------------------------

#Check covariate missingness
d_diar <- d %>% filter(!is.na(diar7d))
d_anthro <- d %>% filter(!is.na(haz)|!is.na(waz))


for(i in Wvars){
  cat(i,":\n")
  if(sum(!is.na(d_diar[[i]]))!=nrow(d_diar)){
    print(table(d_diar$study, !is.na(d_diar[[i]])))
  }
}

for(i in Wvars_anthro){
  cat(i,":\n")
  if(sum(!is.na(d_anthro[[i]]))!=nrow(d_anthro)){
    print(table(d_anthro$study, !is.na(d_anthro[[i]])))
  }
}


#----------------------------------
#median/mode impute missing data
#----------------------------------

d$merge_id <- 1:nrow(d)

dW <- d %>% subset(., select = c("study","merge_id",all_Wvars))
df <- d[ , !colnames(d) %in% c(all_Wvars)]

df2 <- dW %>% group_by(study) %>%
  do(res=impute_missing_values(.))

df3 <- NULL
for(i in 1:length(df2$res)){
  df3 <- bind_rows(df3, df2$res[[i]]$data)
}

d <- full_join(df, df3, by=c("study","merge_id"))



#----------------------------------
# Check covariates again
#----------------------------------

#Check covariate missingness
d_diar <- d %>% filter(!is.na(diar7d))
d_anthro <- d %>% filter(!is.na(haz)|!is.na(waz))


for(i in Wvars){
  cat(i,":\n")
  if(sum(!is.na(d_diar[[i]]))!=nrow(d_diar)){
    print(table(d_diar$study, !is.na(d_diar[[i]])))
  }
}

for(i in Wvars_anthro){
  cat(i,":\n")
  if(sum(!is.na(d_anthro[[i]]))!=nrow(d_anthro)){
    print(table(d_anthro$study, !is.na(d_anthro[[i]])))
  }
}


saveRDS(d, file=paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
