# importing packages and setting directories
source(here::here("0-config.R"))

# Load Wash Benefits Bangladesh primary datasets
anthro <- read.csv(paste0(dropboxDir,"WBB/raw/1-primary-outcome-datasets/washb-bangladesh-anthro.csv"))
diar <- read.csv(paste0(dropboxDir,"WBB/raw/1-primary-outcome-datasets/washb-bangladesh-diar.csv"))
enrol <- read.csv(paste0(dropboxDir,"WBB/raw/1-primary-outcome-datasets/washb-bangladesh-enrol.csv"))

# Harmonizing anthro variable names with diar
anthro<-anthro%>%
          rename(agedays=aged,
                 ageyrs=agey,
                 svydate=anthrodate)
anthro<-anthro%>%
          mutate(svyyear=year(dmy(svydate)),
                 svyweek=week(dmy(svydate)))


# binding anthro and diarrhea datasets
anthro_diar<-bind_rows(anthro,diar)

# reading in public IDs
pub_ids <- read.csv(paste0(dropboxDir,"WBB/raw/public-ids.csv")) 

# merge in public IDs
anthro_diar_pub <- inner_join(anthro_diar,pub_ids,by=c("block","clusterid","dataid"))

# removing non-public ID vars
non_pub_vars<-c("block","clusterid","dataid")
anthro_diar_pub <- anthro_diar_pub%>%select(-non_pub_vars)

# Load Wash Benefits Bangladesh environmental sample datasets
env_early <- read_dta(paste0(dropboxDir,"WBB/raw/3-env-datasets/washb-bangladesh-early-env-data-public.dta"))
env_mid_end <- read_dta(paste0(dropboxDir,"WBB/raw/3-env-datasets/washb-bangladesh-midend-env-data-public.dta"))

# Trimming vars that overlap with enrol.csv (which will be merged later)
intersect_env_early_enrol<-intersect(names(env_early),names(enrol))
intersect_env_mid_end_enrol<-intersect(names(env_mid_end),names(enrol))
env_early <- env_early%>%select(-intersect_env_early_enrol)
env_mid_end <- env_mid_end%>%select(-intersect_env_mid_end_enrol)

# Environmental data apply to target children only - adding childid and tchild to reflect this
env_early$childid<-"T1"
env_early$tchild<-"Target child"
env_mid_end$childid<-"T1"
env_mid_end$tchild<-"Target child"

# Harmonizing variable types between anthro/diar/enrol and env_early

# storing var names of intersect(anthro/diar/enrol,env_early) vars in vectors by type of var
ints<-c()
nums<-c()
factors<-c()
intersect_1<-intersect(names(anthro_diar_pub),names(env_early))
for (i in 1:length(intersect_1)){
  if (class(anthro_diar_pub[,intersect_1[i]])=="integer"){
    ints<-c(ints,intersect_1[i])
  }
  if (class(anthro_diar_pub[,intersect_1[i]])=="numeric"){
    nums<-c(nums,intersect_1[i])
  }
  if (class(anthro_diar_pub[,intersect_1[i]])=="factor"){
    factors<-c(factors,intersect_1[i])
  }
}

# coercing env_early vars to the type of the matching vars in anthro/diar/enrol

for (i in 1:length(names(env_early))){
  if (names(env_early)[i] %in% ints){
    env_early[,i]<-as.integer(unlist(env_early[,i]))
  }
  if (names(env_early)[i] %in% nums){
    env_early[,i]<-as.numeric(unlist(env_early[,i]))
  }
  if (names(env_early)[i] %in% factors){
    env_early[,i]<-as.factor(unlist(env_early[,i]))
  }
}

# binding anthro/diar with env_early
anthro_diar_env_early<-bind_rows(anthro_diar_pub,env_early)

# Harmonizing variable types between anthro/diar/enrol and env_early
# storing var names of intersect(anthro/diar/enrol/env_early,env_mid_end) vars in vectors by type of var
ints<-c()
nums<-c()
factors<-c()
chars<-c()
intersect_2<-intersect(names(anthro_diar_env_early),names(env_mid_end))
for (i in 1:length(intersect_2)){
  if (class(anthro_diar_env_early[,intersect_2[i]])=="integer"){
    ints<-c(ints,intersect_2[i])
  }
  if (class(anthro_diar_env_early[,intersect_2[i]])=="numeric"){
    nums<-c(nums,intersect_2[i])
  }
  if(class(anthro_diar_env_early[,intersect_2[i]])=="factor"){
    factors<-c(factors,intersect_2[i])
  }
  if(class(anthro_diar_env_early[,intersect_2[i]])=="character"){
    chars<-c(chars,intersect_2[i])
  }
}

# coercing env_mid_end vars to the type of the matching vars in anthro/diar/enrol/env_early

for (i in 1:length(names(env_mid_end))){
  if (names(env_mid_end)[i] %in% ints){
    env_mid_end[,i]<-as.integer(unlist(env_mid_end[,i]))
  }
  if (names(env_mid_end)[i] %in% nums){
    env_mid_end[,i]<-as.numeric(unlist(env_mid_end[,i]))
  }
  if (names(env_mid_end)[i] %in% factors){
    env_mid_end[,i]<-as.factor(unlist(env_mid_end[,i]))
  }
  if (names(env_mid_end)[i] %in% chars){
    env_mid_end[,i]<-as.character(unlist(env_mid_end[,i]))
  }
}

# binding anthro/diar/env_early with env_mid_end
anthro_diar_env<-bind_rows(anthro_diar_env_early,env_mid_end)

# Load Wash Benefits Bangladesh soil transmitted helminth sample datasets
sth <- read.csv(paste0(dropboxDir,"WBB/raw/2-sth-kk-outcome-datasets/Public/washb-bangladesh-sth-public.csv"))

# Harmonizing sth variable names to facilitate bind with anthro/diar/env
sth<-sth%>%rename(dataid_r=dataid,
                  clusterid_r=clusterid,
                  block_r=block,
                  agedays=aged,
                  ageyrs=agey,
                  childid=personid)

# Trimming vars that overlap with enrol.csv (which will be merged later)
intersect_sth_enrol<-intersect(names(sth),names(enrol))
sth<-sth%>%select(-intersect_sth_enrol)

# storing var names of intersect(anthro/diar/env,sth) vars in vectors by type of var
ints<-c()
nums<-c()
factors<-c()
chars<-c()
intersect_3<-intersect(names(anthro_diar_env),names(sth))
for (i in 1:length(intersect_3)){
  if (class(anthro_diar_env[,intersect_3[i]])=="integer"){
    ints<-c(ints,intersect_3[i])
  }
  if (class(anthro_diar_env[,intersect_3[i]])=="numeric"){
    nums<-c(nums,intersect_3[i])
  }
  if(class(anthro_diar_env[,intersect_3[i]])=="factor"){
    factors<-c(factors,intersect_3[i])
  }
  if(class(anthro_diar_env[,intersect_3[i]])=="character"){
    chars<-c(chars,intersect_3[i])
  }
}

# coercing sth vars to the type of the matching vars in anthro/diar/env

for (i in 1:length(names(sth))){
  if (names(sth)[i] %in% ints){
    sth[,i]<-as.integer(unlist(sth[,i]))
  }
  if (names(sth)[i] %in% nums){
    sth[,i]<-as.numeric(unlist(sth[,i]))
  }
  if (names(sth)[i] %in% factors){
    sth[,i]<-as.factor(unlist(sth[,i]))
  }
  if (names(sth)[i] %in% chars){
    sth[,i]<-as.character(unlist(sth[,i]))
  }
}

# binding anthro/diar/env and sth, and sorting by child and age
anthro_diar_env_sth<-bind_rows(anthro_diar_env,sth)

# Merging in enrol (baseline covariates)
# creating enrol-specific svydate var
enrol<-enrol%>%rename(svydate.enrol=svydate)

# merging in public IDs to enrol
enrol_pub <- inner_join(enrol,pub_ids,by=c("block","clusterid","dataid"))

# removing non-public ID vars
non_pub_vars<-c("block","clusterid","dataid")
enrol_pub <- enrol_pub%>%select(-non_pub_vars)

# merging anthro/diar and enrol datasets
anthro_diar_env_sth_enrol<-left_join(anthro_diar_env_sth,enrol_pub,by=c("dataid_r","clusterid_r","block_r"))

# sorting by child- and time-indexing variables
anthro_diar_env_sth_enrol <- anthro_diar_env_sth_enrol%>%arrange(dataid_r,childid,agedays)

# reorganizing columns
dataid_col<-grep("dataid_r", colnames(anthro_diar_env_sth_enrol))
childid_col<-grep("childid", colnames(anthro_diar_env_sth_enrol))
agedays_col<-grep("agedays", colnames(anthro_diar_env_sth_enrol))
clusterid_col<-grep("clusterid_r", colnames(anthro_diar_env_sth_enrol))
block_col<-grep("block_r", colnames(anthro_diar_env_sth_enrol))
tr_col<-grep("tr", colnames(anthro_diar_env_sth_enrol))
a_cols<-c(dataid_col,childid_col,agedays_col,clusterid_col,block_col,tr_col)
b_cols<-seq(1,length(colnames(anthro_diar_env_sth_enrol)))
b_cols<-b_cols[! b_cols %in% a_cols]
anthro_diar_env_sth_enrol<-anthro_diar_env_sth_enrol[,c(a_cols,b_cols)]
anthro_diar_env_sth_enrol$motherid <- anthro_diar_env_sth_enrol$childid

#Save dataset in dropbox directory
saveRDS(d, paste0(dropboxDir,"WBB/clean/WBB-longform.RDS"))

# Sanity checks
sum(is.na(anthro_diar_env_sth_enrol$dataid_r))
sum(is.na(anthro_diar_env_sth_enrol$childid))
sum(is.na(anthro_diar_env_sth_enrol$agedays))
# Note: 2824 individuals are missing "agedays" in sth dataset


#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#

# Checking that overlapping vars between enrol, env_early, env_mid_end, and sth have the same values
# comparing enrol and env_early
enrol_pub<-full_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
intersect_enrol_enve<-intersect(names(enrol_pub),names(env_early))
env_early2<-env_early[,c(intersect_enrol_enve)]
enrol2<-enrol_pub[,c(intersect_enrol_enve)]
enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(env_early2$dataid_r),unique(enrol2$dataid_r)))
# identifying and printing dataid's in env dataset that are NOT in enrol
mis<-outersect(unique(env_early2$dataid_r),unique(enrol2$dataid_r))
print(mis)
# this dataID had NAs for all the enrol-type covariates, so no info is lost by exlcuding it
env_early2<-env_early2%>%filter(dataid_r!=mis)
enrol2$momedu<-as.numeric(enrol2$momedu)
enrol2$hfiacat<-as.numeric(enrol2$hfiacat)
env_early2$momedu<-as.numeric(env_early2$momedu)
env_early2$hfiacat<-as.numeric(env_early2$hfiacat)
#if the line below produces "1", all values match between the datasets
print(all.equal(enrol2,env_early2))


# comparing enrol and env_mid_end, timepoint=1
enrol_pub<-full_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
env_mid_end_t1<-env_mid_end%>%filter(timepoint==1)
intersect_enrol_enve<-intersect(names(enrol_pub),names(env_mid_end_t1))
env_mid_end2<-env_mid_end_t1[,c(intersect_enrol_enve)]
enrol2<-enrol_pub[,c(intersect_enrol_enve)]
enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r)))
# identifying and printing dataid's in env dataset that are NOT in enrol
mis<-outersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r))
print(mis)
# this dataID had NAs for all the enrol-type covariates, so no info is lost by exlcuding it
env_mid_end2<-env_mid_end2%>%filter(dataid_r!=mis)
enrol2<-data.frame(lapply(enrol2,function(x) as.integer(x)))
momedu<-env_mid_end2$momedu<-as.numeric(as_factor(env_mid_end2$momedu))
env_mid_end2<-data.frame(lapply(env_mid_end2,function(x) as.integer(as.character(x))))
env_mid_end2$momedu<-momedu
enrol2<-enrol2%>%arrange(dataid_r)
env_mid_end2<-env_mid_end2%>%arrange(dataid_r)
#if the line below produces "TRUE", all values match between the datasets
print(all.equal(enrol2,env_mid_end2))

# comparing enrol and env_mid_end, timepoint=2
enrol_pub<-full_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
env_mid_end_t2<-env_mid_end%>%filter(timepoint==2)
intersect_enrol_enve<-intersect(names(enrol_pub),names(env_mid_end_t2))
env_mid_end2<-env_mid_end_t2[,c(intersect_enrol_enve)]
enrol2<-enrol_pub[,c(intersect_enrol_enve)]
enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r)))
# identifying and printing dataid's in env dataset that are NOT in enrol
mis<-outersect(unique(env_mid_end2$dataid_r),unique(enrol2$dataid_r))
print(mis)
# this dataID had NAs for all the enrol-type covariates, so no info is lost by exlcuding it
env_mid_end2<-env_mid_end2%>%filter(dataid_r!=mis)
enrol2<-data.frame(lapply(enrol2,function(x) as.integer(x)))
momedu<-env_mid_end2$momedu<-as.numeric(as_factor(env_mid_end2$momedu))
env_mid_end2<-data.frame(lapply(env_mid_end2,function(x) as.integer(as.character(x))))
env_mid_end2$momedu<-momedu
enrol2<-enrol2%>%arrange(dataid_r)
env_mid_end2<-env_mid_end2%>%arrange(dataid_r)
#if the line below produces "TRUE", all values match between the datasets
print(all.equal(enrol2,env_mid_end2))


# comapring enrol and sth
enrol_pub<-full_join(enrol,pub_ids,by=c("block","clusterid","dataid"))
sth_uni<-sth[!duplicated(sth$dataid_r),]
intersect_enrol_enve<-intersect(names(enrol_pub),names(sth_uni))
sth2<-sth_uni[,c(intersect_enrol_enve)]
enrol2<-enrol_pub[,c(intersect_enrol_enve)]
enrol2<-enrol2%>%filter(dataid_r %in% intersect(unique(sth2$dataid_r),unique(enrol2$dataid_r)))
# identifying and printing dataid's in env dataset that are NOT in enrol
mis<-outersect(unique(sth2$dataid_r),unique(enrol2$dataid_r))
print(mis)
# this dataID had NAs for all the enrol-type covariates, so no info is lost by exlcuding it
sth2<-sth2%>%filter(dataid_r!=mis)
sth2$momedu<-as.factor(sth2$momedu)
sth2$hfiacat<-as.factor(sth2$hfiacat)
enrol2<-data.frame(lapply(enrol2,function(x) as.numeric(x)))
sth2<-data.frame(lapply(sth2,function(x) as.numeric(x)))
#recoding sth vars to match enrol for comparison
sth_change<-c("asset_radio",
              "asset_refrig",
              "asset_bike",
              "asset_moto",
              "asset_sewmach",
              "asset_tv",
              "asset_wardrobe",
              "asset_table",
              "asset_chair",
              "asset_khat",
              "asset_chouki",
              "asset_mobile",
              "roof",
              "walls",
              "floor",
              "elec")
# binary changes: 2(sth)->0(enrol)
# non-binary changes #(sth)->#(enrol): "asset_phone", 1->9, 2->1, 3->0; "asset_clock", 1->NA,3->0,2->1
# no change: asset_tvbw,asset_tvcol
for (i in 1:length(names(sth2))){
  if (names(sth2)[i] %in% change){
    sth2[,i]<-ifelse(sth2[,i]==2,0,1)
  }
  if(names(sth2)[i]=="asset_phone"){
    sth2[,i]<-ifelse(sth2[,i]==1,9,ifelse(sth2[,i]==2,1,ifelse(sth2[,i]==3,0,NA)))
  }
  if(names(sth2)[i]=="asset_clock"){
    sth2[,i]<-ifelse(sth2[,i]==1,NA,ifelse(sth2[,i]==3,0,ifelse(sth2[,i]==2,1,NA)))
  }
}
enrol2<-enrol2%>%arrange(dataid_r)
sth2<-sth2%>%arrange(dataid_r)
#if the line below produces "1", all values match between the datasets
print(all.equal(enrol2,sth2))