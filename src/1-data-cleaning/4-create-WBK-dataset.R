

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))



#----------------------------------------------------------------------------
#Make env dataset
#----------------------------------------------------------------------------
env <- read_dta(paste0(dropboxDir,"Data/WBK/wbk_STH_soil.dta"))
enrol <- read.csv(paste0(dropboxDir,"Data/WBK/washb-kenya-enrol.csv"))

head(env)

# labs <- makeVlist(env) %>% mutate(label=as.character(label)) %>% as.data.frame()
# labs$label[labs$name=="a2_1 "]
# write.csv(labs, paste0(dropboxDir,"Data/WBK/wbk_STH_soil_codebook.csv"))

#mark any animcals
colnames(env)
table(env$cow)
table(env$poultry)

env <- env %>% 
  mutate(cow=ifelse(is.na(cow),0,cow),
         goat=ifelse(is.na(goat),0,goat),
         dog=ifelse(is.na(dog),0,dog),
         poultry=ifelse(is.na(poultry),0,poultry),
    animals = 1*(cow+goat+dog+poultry>0))
table(is.na(env$dog))
table((env$animals))

#Make positive and log_quant longform datasets
#(Don't use viable because PCR can't detect the difference)
pos <- env %>% subset(., select=c(hhid, block, soil_id,
                                   dummy_sth,
                                   dummy_ascaris,
                                   dummy_trichuris)) %>%
  gather(dummy_sth:dummy_trichuris, key = target, value = pos) %>%
  mutate(target=gsub("dummy_","", target))

quant <- env %>% subset(., select=c(hhid, block, soil_id,
                                    total_eggs,
                                    ascaris,
                                    trichuris)) %>%
  gather(total_eggs:trichuris , key = target, value = abund) %>%
  mutate(target=ifelse(target=="total_eggs","sth",target))


table(pos$target)
table(quant$target)


dim(pos)
dim(quant)
env2 <- full_join(pos, quant, by=c("hhid","block","soil_id","target"))
dim(env2)
head(env2)



#Subset and clean covariates
cov <- env %>% subset(., select=c(hhid, vlgid, compoundid, clusterid, block_dc, soil_id,  tr,
                                  num_hh, 
                                  roof,
                                  elec,
                                  animals,
                                  walls,floor,
                                  assetquintile,
                                  cow,goat,
                                  dog,poultry,
                                  soil_feces,
                                  soil_sunny,
                                  soil_wet)) %>%
  mutate(tr2 = factor(tr, labels=c("Control", #save seperate treatment arm
                                  "Sanitation",
                                  "WSH")),
          #combine Sanitation and WSH arm for primary analysis),
          tr = case_when(tr==1 ~ "Control",
                         tr==3 | tr==5 ~ "Sanitation"
                         ),
         sample = "S",
         round="env round") 


dim(env2)
env2 <- left_join(env2, cov, by=c("hhid","soil_id"))
dim(env2)


#Get other enrollment covariates needed for env. analysis
colnames(env2)
colnames(enrol)
enrol2 <- enrol %>% 
  mutate(env_date=dmy(ms_el_up_date)) %>%
  subset(., select=c(hhid, env_date, momedu, momage, Ncomp, HHS)) %>%
  rename(hfiacat=HHS) %>%
  filter(!is.na(env_date)) %>% 
  distinct()

dim(env2)
env2 <- left_join(env2, enrol2, by=c("hhid"))
dim(env2)
table(is.na(env2$env_date))


enrol2 <- enrol %>% 
  subset(., select=c(hhid, momedu, Ncomp)) %>% 
  rename(Nhh=Ncomp)


saveRDS(env2, paste0(dropboxDir, "Data/WBK/Clean/WBK_env.RDS"))



#----------------------------------------------------------------------------
#Make child health dataset
#----------------------------------------------------------------------------


#Load child parasite datasets
STH <- read_dta(paste0(dropboxDir,"Data/WBK/parasites_kenya_public_ca20171215.dta"))
head(STH)

#Load covariates and treatment arms
tr <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-real-tr.csv"))


#Subset to the environmental vars (drop covariates)
colnames(STH)
df <- STH %>% subset(., select=c(childidr2, hhidr2, tr, target_child, 
                                 asca_epg,          asca_intensity,    asca_intensity_cat,tric_epg,         
                                 tric_intensity,    tric_intensity_cat,hook_epg,          hook_intensity,   
                                 hook_intensity_cat,ascaris_yn,        trichuris_yn,      hook_yn, giardia_yn)) %>%
  rename(childid=childidr2,
         hhid=hhidr2) %>%
  gather(asca_epg:giardia_yn, key = target, value = Y)
head(df)

table(df$target)

df$measure <- str_split(df$target,"_", simplify = T)[,2]
df$target <- str_split(df$target,"_", simplify = T)[,1]

table(df$measure)
df_epg <-df %>% filter(measure=="epg") %>% rename(epg=Y) %>% subset(., select = -c(measure))
df_intensity <-df %>% filter(measure=="intensity") %>% rename(intensity=Y) %>% subset(., select = -c(measure))
df_pos <-df %>% filter(measure=="yn") %>% rename(pos=Y) %>% subset(., select = -c(measure))

dim(df_epg)
child_sth <- left_join(df_pos, df_epg, by=c("childid","hhid","tr", 'target_child', 'target'))
child_sth <- left_join(child_sth, df_intensity, by=c("childid","hhid","tr", 'target_child', 'target'))
dim(child_sth)

#Why is dataset increasing in size?
