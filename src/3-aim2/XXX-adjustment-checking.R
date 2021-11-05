
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 
mapsan <- readRDS(paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))


table(d$study)
table(d$study, d$hhwealth)

df <- d %>% filter(study=="Capone 2021")
res <- glm(diar7d~hhwealth, data=df, family="binomial")
summary(res)


table(d$study)
table(d$target)
table(d$sample)
df <- d %>% filter(study=="Holcomb 2020", target=="Any MST",sample=="S", !is.na(pos), !is.na(diar7d))
fun_res<-adj_RR %>% filter(study=="Holcomb 2020", target=="Any MST",sample=="S", Y=="diar7d")
fun_res$W

head(df)
table(df$hhwealth, df$pos, df$diar7d)
dim(df)
res <- glm(diar7d ~ pos + age + hhwealth + tr, family=poisson(link="log"),  data=df)
summary(res)
exp(-1.26288)





df <- d %>% filter(study=="Holcomb 2020", target=="Any MST",sample=="LS", !is.na(pos), !is.na(haz))
mapsan_df <- d %>% filter(study=="Holcomb 2020", target=="Any MST",sample=="LS", !is.na(pos), !is.na(haz))
fun_res<-adj_RR %>% filter(study=="Holcomb 2020", target=="Any MST",sample=="LS", Y=="haz")
fun_res
fun_res$W

head(df)
table(df$hhwealth, df$pos, df$diar7d)
dim(df)
res <- glm(haz ~ pos + sex + age + hhwealth , family="gaussian",  data=df)
summary(res)

res <- glm(haz ~ pos, family="gaussian",  data=df)
summary(res)

res <- glm(haz ~ pos , family="gaussian",  data=mapsan_df)
summary(res)

res <- glm(haz ~ hhwealth , family="gaussian",  data=df)
summary(res)


df <- d %>% filter(trial=="MapSan", target=="Any MST",sample=="LS", !is.na(pos), !is.na(haz))
res <- glm(haz ~ hhwealth , family="gaussian",  data=df)
summary(res)



