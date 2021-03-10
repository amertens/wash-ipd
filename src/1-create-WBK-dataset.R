

# importing packages and setting directories
source(here::here("0-config.R"))

library(dplyr)


#----------------------------------------------------------------------------
#Make env dataset
#----------------------------------------------------------------------------
env <- read_dta(paste0(dropboxDir,"Data/WBK/wbk_STH_soil.dta"))

head(env)

# labs <- makeVlist(env) %>% mutate(label=as.character(label)) %>% as.data.frame()
# labs$label[labs$name=="a2_1 "]
# write.csv(labs, paste0(dropboxDir,"Data/WBK/wbk_STH_soil_codebook.csv"))


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
                                  walls,floor,
                                  assetquintile,
                                  cow,goat,
                                  dog,poultry,
                                  soil_feces,
                                  soil_sunny,
                                  soil_wet)) %>%
  mutate(tr = factor(tr, labels=c("Control",
                                  "Sanitation",
                                  "WSH")),
         sample = "S",
         round="env round") 


env2 <- left_join(env2, cov, by=c("hhid","soil_id"))


saveRDS(env2, paste0(dropboxDir, "Data/WBK/Clean/WBK_env.RDS"))



#----------------------------------------------------------------------------
#Make child health dataset
#----------------------------------------------------------------------------


#Load child parasite datasets
STH <- read_dta(paste0(dropboxDir,"Data/WBK/parasites_kenya_public_ca20171215.dta"))
head(STH)

#Load covariates and treatment arms
enrol <- read.csv(paste0(dropboxDir,"Data/WBB/washb-bangladesh-enrol.csv"))
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
