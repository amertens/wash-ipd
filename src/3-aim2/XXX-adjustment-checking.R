
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 
mapsan <- readRDS(paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))
child <- read.csv(paste0(dropboxDir,"Data/MapSan/triPhase_database_20200824 1412_IPD.csv")) %>%
  filter(actualPhase==1)

child <- child %>% distinct(ï..totchildID, age_months , anthro_hfa_2, povNormal)
dim(child)
head(child)
child$hhwealth=factor(quantcut(child$povNormal, na.rm=T), labels=c("1","2","3","4"))
dim(child)
res <- glm(anthro_hfa_2 ~ povNormal, data=child)
summary(res)
res <- glm(anthro_hfa_2 ~ hhwealth, data=child)
summary(res)
res <- glm(diarrhea ~ hhwealth, family="binomial", data=child)
summary(res)

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


Wvars = c("sex","age","hfiacat","momage","hhwealth_cont", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
df <- d %>% filter(study=="Fuhrmeister 2020", target=="Any MST",sample=="MH", !is.na(haz))
res_fun<- aim2_glm(df, Ws = Wvars, forcedW=c("age", "hhwealth_cont"), outcome="diar7d", exposure="pos", study=df$study[1], sample=df$sample[1], target=df$target[1], family="binomial") 
res_fun
res_adj <- glm(diar7d ~ pos + sex+ dadagri+ Nhh + tr+ age+ hhwealth_cont , family=poisson(link="log"),  data=df)
summary(res_adj)
exp(coef(res_adj)[2])

res_unadj <- glm(diar7d ~ pos, family=poisson(link="log"),  data=df)
summary(res_unadj)
exp(coef(res_unadj)[2])




Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
res_fun<- aim2_glm(df, Ws = Wvars_anthro, forcedW=c("age", "hhwealth"), outcome="haz", exposure="pos", study=df$study[1], sample=df$sample[1], target=df$target[1], family="gaussian") 
res_fun
res_adj <- glm(haz ~ pos +  momedu+ landacre+ elec+ hfiacat+ floor+ tr+ age+ hhwealth , family="gaussian",  data=df)
summary(res_adj)
(coef(res_adj)[2])

res_unadj <- glm(haz ~ pos, family="gaussian",  data=df)
summary(res_unadj)
(coef(res_unadj)[2])
