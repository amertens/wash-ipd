


rm(list=ls())
source(here::here("0-config.R"))

wbb <- readRDS(paste0(dropboxDir,"Data/WBB_env_CH_data.rds"))
wbk <- readRDS(paste0(dropboxDir,"Data/WBK_env_CH_data.rds"))
mapsan <- readRDS(paste0(dropboxDir,"Data/mapsan_env_CH_data.rds"))
gv <- readRDS(paste0(dropboxDir,"Data/gv_env_CH_data.rds"))
odisha <- readRDS(paste0(dropboxDir,"Data/odisha_env_CH_data.rds"))

table(wbb$diar7d_full)
table(wbk$diar7d_full)
table(mapsan$diar7d_full)
table(gv$diar7d_full)
table(odisha$diar7d_full)


prop.table(table(wbb$diar7d))
prop.table(table(wbk$diar7d))
prop.table(table(mapsan$diar7d))
prop.table(table(gv$diar7d))
prop.table(table(odisha$diar7d))

table(mapsan$animals)


#Clean pathogen-specific infections

wbk <- wbk %>%  subset(., select = -c(ch_sth, 
                                      ch_sth_coinf, ch_sth_giar_coinf, ch_hook, ch_hook_intensity, 
                                      qpcr_Necator, qpcr_Ancylostoma, qpcr_Strongyloides)) %>%
  rename(
    ch_pos_giardia=ch_giardia,
    ch_pos_ascaris=ch_ascaris,
    ch_pos_trichuris=ch_trichuris,
    ch_abund_giardia=ch_giardia,
    ch_abund_ascaris=ch_asca_intensity,
    ch_abund_trichuris=ch_tric_intensity,
    ch_qpcr_pos_trichuris=qpcr_Trichuris,
    ch_qpcr_pos_ascaris=qpcr_Ascaris)

mapsan <- mapsan %>% rename(
  ch_pos_giardia=gpp_giar,
  ch_pos_adenovirus=gpp_aden,
  ch_pos_ascaris=Kkasc,
  ch_pos_trichuris=KKtrc,
  ch_pos_norovirus=gpp_noro,
  ch_pos_rotavirus=gpp_rota,
  ch_pos_shigella=gpp_shig,
  ch_pos_yersinia=gpp_yers,
  ch_pos_cdiff=gpp_cdif,
  ch_pos_campylobacter=gpp_camp,
  ch_pos_entamoeba=gpp_enta,
  ch_pos_crypto=gpp_cryp,
  ch_pos_salmonella=gpp_salm,
  ch_pos_cholera=gpp_chol) %>%
  mutate(ch_pos_path_ecoli = 1*(gpp_o157 + gpp_etec + gpp_stec > 0)) %>%
  subset(., select = -c(KKhkw, gpp_o157, gpp_etec, gpp_stec))
wbk <- zap_labels(wbk)


#d <- bind_rows(wbb, wbk, mapsan, gv, odisha)
d <- data.table::rbindlist(list( wbb, wbk, mapsan, gv, odisha), fill=T)

table(d$study, is.na(d$diar7d_full))
colnames(d)

unique(d$target)

#-----------------------------------------------------------
# Calculate stunting/wasting/underweight
#-----------------------------------------------------------

d <- d %>% mutate(
  stunt = 1*(haz < -2),
  wast = 1*(whz < -2),
  underwt = 1*(waz < -2),
)


#-----------------------------------------------------------
# Check covariates
#-----------------------------------------------------------
table(d$study, d$hfiacat)
table(d$study, is.na(d$hfiacat))
table(d$study, d$hhwealth)
table(d$study, is.na(d$hhwealth))
table(d$study, d$momedu)
table(d$study, is.na(d$momedu)) 
table(d$study, is.na(d$momage)) 

table(d$study, d$sex)
table(d$study, is.na(d$sex))
d$age[is.na(d$age)] <- d$agedays[is.na(d$age)]
d$age_anthro[is.na(d$age_anthro)] <- d$agedays_anthro[is.na(d$age_anthro)]
table(d$study, is.na(d$age))
table(d$study[!is.na(d$haz)], is.na(d$age_anthro[!is.na(d$haz)]))


#-----------------------------------------------------------
# Save
#-----------------------------------------------------------
saveRDS(d, file=paste0(dropboxDir,"Data/merged_env_CH_data.rds"))

table(is.na(d$child_date_pathogen), d$ch_pos_path_ecoli, d$study)


d %>% group_by(study) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_pos_diar=sum(diar7d==1, na.rm=T), N_haz=sum(!is.na(haz)), N_waz=sum(!is.na(waz)))

tab <- d %>% group_by(study, sample, target) %>%
  summarize(N=n(), N_pos=sum(pos), N_diar=sum(!is.na(diar7d)), N_pos_diar=sum(diar7d==1, na.rm=T), N_pos_env_diar=sum(pos==1 & diar7d==1, na.rm=T), N_haz=sum(!is.na(haz)))
tab %>% filter(sample=="any sample type", target=="Any pathogen")
tab %>% filter(sample=="any sample type", target=="Any MST")
