
rm(list=ls())
source(here::here("0-config.R"))

# #targets in CH data

ch_pathogens <- c(
  "Giardia",
  "Ascaris",
  "Trichuris",
  "Pathogenic E. coli",
  "Cryptosporidium",
  "Entamoeba histolytica",
  "Adenovirus",
  "Norovirus",
  "Rotavirus",
  "Shigella",
  "Yersinia",
  "C. difficile",
  "Campylobacter",
  "Salmonella",
  "V. cholerae"
)




d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
table(is.na(d$child_date_pathogen), d$ch_pos_path_ecoli, d$study)

# #drop aggregate values:
d <- d %>% filter(!(grepl("Any ",target) & target != "Any STH"),
                  !(target %in% c("Zoonotic E. coli","Non-zoonotic E. coli")))

# #drop dates out of range.
# dim(d)
# d <- d %>% filter(!is.na(pathogen_date))
# dim(d)
# d <- d %>% filter(pathogen_date - env_date >= 0 & pathogen_date - env_date <= 124)
# dim(d)



df <- d %>% filter(target=="Ascaris") %>% droplevels()
table(df$study, df$ch_pos_ascaris, df$sample)
table(df$study, df$ch_qpcr_pos_ascaris, df$sample)

df <- d %>% filter(target=="Trichuris") %>% droplevels()
table(df$study, df$ch_pos_trichuris, df$sample)
table(df$study, df$ch_qpcr_pos_trichuris, df$sample)

#make list of pairs - binary
paired_pathogens <- as.data.frame(t(data.frame(
           c("ch_pos_giardia",   "Giardia"),     
           c("ch_pos_entamoeba", "Entamoeba histolytica"),     
           c("ch_pos_crypto", "Cryptosporidium"),       
           c("ch_qpcr_pos_ascaris",   "Ascaris"),
           c("ch_qpcr_pos_trichuris", "Trichuris"),
           c("ch_pos_ascaris",   "Ascaris"),
          c("ch_pos_trichuris",  "Trichuris"),
          c("ch_pos_giardia_EE",   "Giardia"),     
          c("ch_pos_entamoeba_EE", "Entamoeba histolytica"),     
          c("ch_pos_crypto_EE", "Cryptosporidium"),       
          c("ch_pos_ascaris_EE",   "Ascaris"),
          c("ch_pos_trichuris_EE",  "Trichuris"),
         c( "ch_pos_adenovirus", "Adenovirus"),   
         c("ch_pos_norovirus",   "Norovirus"),   
         c("ch_pos_rotavirus",  "Rotavirus"),    
         c("ch_pos_cdiff",  "C. difficile"),       
         c( "ch_pos_campylobacter",  "Campylobacter"),
         c( "ch_pos_salmonella",  "Salmonella"),   
         c( "ch_pos_shigella",   "Shigella"),   
         c( "ch_pos_cholera", "V. cholerae"),       
         c("ch_pos_yersinia",   "Yersinia"),     
         c("ch_pos_path_ecoli", "Pathogenic E. coli"))))

rownames(paired_pathogens) <- NULL
colnames(paired_pathogens) <- c("outcome","exposure")

#make list of pairs - continious

paired_pathogens_cont <- as.data.frame(t(data.frame(
  exposure=c("ch_abund_giardia",  "Giardia"),     
            c("ch_abund_entamoeba", "Entamoeba histolytica"),
            c("ch_abund_ascaris",    "Ascaris"), 
            c("ch_abund_trichuris", "Trichuris"),
            c("ch_abund_crypto",     "Cryptosporidium"),
            c("ch_abund_giardia_EE",   "Giardia"),     
            c("ch_abund_entamoeba_EE", "Entamoeba histolytica"),     
            c("ch_abund_crypto_EE", "Cryptosporidium"),       
            c("ch_abund_ascaris_EE",   "Ascaris"),
            c("ch_abund_trichuris_EE",  "Trichuris"),
            c("ch_qpcr_abund_ascaris", "Ascaris"), 
            c("ch_qpcr_abund_trichuris", "Trichuris"))))
rownames(paired_pathogens_cont) <- NULL
colnames(paired_pathogens_cont) <- c("outcome","exposure")


#NOTE: Need to get right covariates including age... make age pathogens above

#Covariate list
Wvars = c("sex","age","hfiacat","momage","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


outcome="ch_pos_rotavirus"
exposure="pos"
study="Kwong 2021"
sample="S"
target= "Ascaris"
family="binomial"
forcedW=c("age", "hhwealth")
Ws=Wvars

#
table(is.na(d$child_date_pathogen), d$ch_pos_path_ecoli, d$study)

df <- d %>% filter(study=={{study}}) %>% droplevels(.)
table(df$target)

table(d$study)

df <- d %>% filter(study=={{study}} ,    target=={{target}}) %>% droplevels(.)
dim(df)

table(df$child_date_pathogen - df$env_date)

table(is.na(df$child_date_pathogen))
table(is.na(df$child_date_pathogen), df$ch_pos_path_ecoli)


      #need to figure out how to use the EE date or QPCR data

#Make function that checks the sampling timing
#and sets outcome to NA in not >=0 and <124
# aim2_path_glm <- function(d, Wvars, forcedW, outcome, exposure, study, sample, target, family){
#   
#   df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
#   
#   
#  Wvars, outcome=paired_pathogens$outcome[1], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[1], family="binomial")) 
#   
#   
#   res <- aim2_glm(d=df, Ws=Wvars, forcedW, outcome=outcome, exposure=exposure, study=study, sample=sample, target=target, family=family)
#   return(res)
# }


res_full <- NULL
#Don't forc W for Giardia because of convergence errors
res_adj1 <- d %>% group_by(study, sample) %>%
  do(aim2_glm(., Ws = Wvars, forcedW=NULL, outcome=paired_pathogens$outcome[1], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[1], family="binomial", minN_thres=5)) 
res_full <- bind_rows(res_full, res_adj1)
for(i in 2:nrow(paired_pathogens)){
  res_adj <- d %>% group_by(study, sample) %>%
    do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome=paired_pathogens$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens$exposure[i], family="binomial", minN_thres=5)) 
  res_full <- bind_rows(res_full, res_adj)
}
res_full <- res_full %>% filter(!is.na(RR))
#LS pathogenic E-coli failed to converge. Rerun here
res_adj2 <- d %>% group_by(study, sample) %>%
  do(aim2_glm(., Ws = Wvars[Wvars!="sex"], forcedW=NULL, outcome="ch_pos_path_ecoli", exposure="pos", study="Capone 2021", sample=.$sample[1], target="Pathogenic E. coli", family="binomial", minN_thres=5)) 
res_adj2 <- res_adj2 %>% filter(!is.na(RR),sample=="LS")

#LS Trichuris failed to converge. Rerun here
res_adj3 <- d %>% group_by(study, sample) %>%
  do(aim2_glm(., Ws = Wvars[Wvars!="walls"], forcedW=NULL, outcome="ch_pos_trichuris", exposure="pos", study="Capone 2021", sample=.$sample[1], target="Trichuris", family="binomial", minN_thres=5)) 
res_adj3 <- res_adj3 %>% filter(!is.na(RR),sample=="LS")
res_adj3

# df <- d %>% filter(sample=="LS", study=="Capone 2021", target=="Trichuris")
# Ws = Wvars
# forcedW=NULL
# outcome="ch_pos_trichuris"
# exposure="pos"
# study="Capone 2021"
# sample="LS"
# target="Trichuris"
# family="binomial"
# minN_thres=5
# table(df$tr)


res_full <- res_full %>% 
  filter(!(Y=="ch_pos_path_ecoli" & sample=="LS")) %>% 
  filter(!(Y=="ch_pos_trichuris" & sample=="LS"))
res_full <- bind_rows(res_full, res_adj2, res_adj3)

res_full_cont <- NULL
for(i in 1:nrow(paired_pathogens_cont)){
  res_adj_cont <- d %>% group_by(study, sample) %>%
    do(aim2_glm(., Ws = Wvars, forcedW=c("age", "hhwealth"), outcome=paired_pathogens_cont$outcome[i], exposure="pos", study=.$study[1], sample=.$sample[1], target=paired_pathogens_cont$exposure[i], family="gaussian", minN_thres=5)) 
  res_full_cont <- bind_rows(res_full_cont, res_adj_cont)
}

res_full_cont <- res_full_cont %>% filter(!is.na(coef))
res_full_cont



saveRDS(res_full, file=here("results/pathogen_specific_aim2_res.Rds"))
saveRDS(res_full_cont, file=here("results/pathogen_specific_aim2_cont_res.Rds"))




