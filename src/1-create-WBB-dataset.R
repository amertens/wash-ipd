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

# creating enrol-specific svydate var
enrol<-enrol%>%rename(svydate.enrol=svydate)

# merging anthro/diar and enrol datasets
anthro_diar_enrol<-left_join(anthro_diar,enrol,by=c("dataid","clusterid","block"))

# reading in public IDs
pub_ids <- read.csv(paste0(dropboxDir,"WBB/raw/public-ids.csv")) 

# merge in public IDs
anthro_diar_enrol_pub <- inner_join(anthro_diar_enrol,pub_ids,by=c("block","clusterid","dataid"))


# removing non-public ID vars
non_pub_vars<-c("block","clusterid","dataid")
anthro_diar_enrol_pub <- anthro_diar_enrol_pub%>%select(-non_pub_vars)

# reorganizing columns to bring ID variables to the left
id_cols<-c(134,135,136)
other_cols<-seq(from=1,to=133,by=1)
anthro_diar_enrol_pub<-anthro_diar_enrol_pub[,c(id_cols,other_cols)]

# Load Wash Benefits Bangladesh environmental sample datasets
env_early <- read_dta(paste0(dropboxDir,"WBB/raw/3-env-datasets/washb-bangladesh-early-env-data-public.dta"))
env_mid_end <- read_dta(paste0(dropboxDir,"WBB/raw/3-env-datasets/washb-bangladesh-midend-env-data-public.dta"))

# Harmonizing variable types between anthro/diar/enrol and env_early

# storing var names of intersect(anthro/diar/enrol,env_early) vars in vectors by type of var
ints<-c()
nums<-c()
factors<-c()
intersect_1<-intersect(names(anthro_diar_enrol_pub),names(env_early))
for (i in 1:length(intersect_1)){
  if (class(anthro_diar_enrol_pub[,intersect_1[i]])=="integer"){
    ints<-c(ints,intersect_1[i])
  }
  if (class(anthro_diar_enrol_pub[,intersect_1[i]])=="numeric"){
    nums<-c(nums,intersect_1[i])
  }
  if (class(anthro_diar_enrol_pub[,intersect_1[i]])=="factor"){
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

# binding anthro/diar/enrol with env_early
anthro_diar_enrol_env_early<-bind_rows(anthro_diar_enrol_pub,env_early)


# Harmonizing variable types between anthro/diar/enrol and env_early

# storing var names of intersect(anthro/diar/enrol/env_early,env_mid_end) vars in vectors by type of var
ints<-c()
nums<-c()
factors<-c()
chars<-c()
intersect_2<-intersect(names(anthro_diar_enrol_env_early),names(env_mid_end))
for (i in 1:length(intersect_2)){
  if (class(anthro_diar_enrol_env_early[,intersect_2[i]])=="integer"){
    ints<-c(ints,intersect_2[i])
  }
  if (class(anthro_diar_enrol_env_early[,intersect_2[i]])=="numeric"){
    nums<-c(nums,intersect_2[i])
  }
  if(class(anthro_diar_enrol_env_early[,intersect_2[i]])=="factor"){
    factors<-c(factors,intersect_2[i])
  }
  if(class(anthro_diar_enrol_env_early[,intersect_2[i]])=="character"){
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

anthro_diar_enrol_env<-bind_rows(anthro_diar_enrol_env_early,env_mid_end)

# Load Wash Benefits Bangladesh soil transmitted helminth sample datasets
sth <- read.csv(paste0(dropboxDir,"WBB/raw/2-sth-kk-outcome-datasets/Public/washb-bangladesh-sth-public.csv"))

# Harmonizing sth variable names to facilitate bind with anthro/diar/enrol/env
sth<-sth%>%rename(dataid_r=dataid,
                  clusterid_r=clusterid,
                  block_r=block,
                  agedays=aged,
                  ageyrs=agey,
                  childid=personid)

# Harmonizing variable types between anthro/diar/enrol/env and sth

# storing var names of intersect(anthro/diar/enrol/env,sth) vars in vectors by type of var
ints<-c()
nums<-c()
factors<-c()
chars<-c()
intersect_3<-intersect(names(anthro_diar_enrol_env),names(sth))
for (i in 1:length(intersect_3)){
  if (class(anthro_diar_enrol_env[,intersect_3[i]])=="integer"){
    ints<-c(ints,intersect_3[i])
  }
  if (class(anthro_diar_enrol_env[,intersect_3[i]])=="numeric"){
    nums<-c(nums,intersect_3[i])
  }
  if(class(anthro_diar_enrol_env[,intersect_3[i]])=="factor"){
    factors<-c(factors,intersect_3[i])
  }
  if(class(anthro_diar_enrol_env[,intersect_3[i]])=="character"){
    chars<-c(chars,intersect_3[i])
  }
}

# coercing sth vars to the type of the matching vars in anthro/diar/enrol/env

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

# binding anthro/diar/enrol/env and sth
anthro_diar_enrol_env_sth<-bind_rows(anthro_diar_enrol_env,sth)%>%arrange(dataid,childid,agedays)


#Calculate child age at every measure


#Save dataset in dropbox directory
saveRDS(d, paste0(dropboxDir,"WBB/clean/WBB-longform.RDS"))

