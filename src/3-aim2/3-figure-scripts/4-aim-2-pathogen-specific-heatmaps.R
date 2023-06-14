
rm(list=ls())
source(here::here("0-config.R"))
library(RColorBrewer)

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 

table(adj_RR$target)
adj_RR %>% filter(target=="Pathogenic E. coli", Y=="diar7d", sample=="any sample type")

unique(adj_RR$study)

#count number of covariates
unadj_RR$N_W <- ""
adj_RR$N_W <- str_count(adj_RR$W,",") + 1
adj_RR$N_W[adj_RR$W=="unadjusted"] <- 0


table(adj_RR$minN)
table(adj_RR$a)
table(adj_RR$c)
table(adj_RR$a+adj_RR$c)

summary(adj_RR$RR)
adj_RR$ci.lb[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- NA
adj_RR$ci.ub[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- NA
adj_RR$sparse[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- "yes"
adj_RR$sample_cat[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- "Sparse data"
adj_RR$RR[adj_RR$RR < 0.05 & !is.na(adj_RR$RR)] <- 1

adj_RR <- adj_RR %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)


unadj_RR <- unadj_RR %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)

adj_RR$pooled <- ifelse(grepl("Pooled",(adj_RR$study)),1,0)
adj_RR$study <- gsub("Pooled","POOLED",adj_RR$study)


#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

#unadj_RR<-unadj_RR %>% filter(!is.na(se))
sample_cats = levels(unadj_RR$sample_cat)[levels(unadj_RR$sample_cat)!="Any sample"]

levels(adj_RR$sample_cat)
unique(adj_RR$study)
adj_RR <- adj_RR %>% mutate(study = factor(study, levels = c("POOLED","Boehm 2016","Capone 2021","Capone 2022 in prep", "Fuhrmeister 2020",
                                                             "Holcomb 2021","Kwong 2021","Odagiri 2016","Reese 2017","Steinbaum 2019"  )))
adj_RR <- adj_RR %>% arrange(sample_cat, study)
adj_RR$X <- paste0(adj_RR$sample_cat,": ", adj_RR$study)
adj_RR$X <- factor(adj_RR$X, levels=rev(unique(adj_RR$X)))
levels(adj_RR$X)

adj_RR$Y <- factor(adj_RR$Y, levels=c("diar7d", "haz","waz","whz",  "underwt","stunt","wast"))
adj_RR$Y <- recode_factor(adj_RR$Y, 
                     "diar7d"="Diarrhoea", 
                     "haz"="HAZ",
                     "waz"="WAZ",
                     "whz"="WHZ",  
                     "underwt"="Underweight",
                     "stunt"="Stunting",
                     "wast"="Wasting")

adj_RR$sign <- NA
unique(adj_RR$Y)
adj_RR$point.diff <- ifelse(adj_RR$Y %in% c("Diarrhoea","Stunting","Wasting","Underweight"), adj_RR$RR, adj_RR$coef)
adj_RR$point.diff[adj_RR$Y %in% c("Diarrhoea","Stunting","Wasting","Underweight")]

adj_RR$sign <- ifelse(adj_RR$Y %in% c("Diarrhoea","Stunting","Wasting","Underweight"),ifelse(adj_RR$point.diff > 1, -1, 1), sign(adj_RR$point.diff))
table(adj_RR$sign)

adj_RR$pval_cat <- cut(adj_RR$pval, breaks = c(-1, 0.01, 0.05, 0.1, 2), 
                  labels = c("<0.01", "<0.05", "0.05-0.1", "0.1-1"))
adj_RR$pval_cat <- ifelse(adj_RR$sign == -1, paste0(adj_RR$pval_cat, " (increased risk)"), 
                     paste0(adj_RR$pval_cat, " (decreased risk)"))
adj_RR$pval_cat[adj_RR$pval_cat %in% c("0.1-1 (decreased risk)", "0.1-1 (increased risk)")] <- "0.1-1"
table(adj_RR$pval_cat)
adj_RR$pval_cat <- factor(adj_RR$pval_cat, levels = c("<0.01 (decreased risk)", 
                                            "<0.05 (decreased risk)", "0.05-0.1 (decreased risk)", 
                                            "0.1-1", "0.05-0.1 (increased risk)", 
                                            "<0.05 (increased risk)", "<0.01 (increased risk)"))
adj_RR$pval_cat <- addNA(adj_RR$pval_cat)
levels(adj_RR$pval_cat) = c(levels(adj_RR$pval_cat), "Sparse")
adj_RR$pval_cat[is.na(adj_RR$pval_cat)] <- "Sparse"
table(adj_RR$pval_cat)
table(is.na(adj_RR$pval_cat))
adj_RR$est = ""
adj_RR$est = paste0(format(round(adj_RR$point.diff, 2), nsmall = 2), " (", format(round(adj_RR$ci.lb, 2), nsmall = 2), ", ", format(round(adj_RR$ci.ub, 2), nsmall = 2), ")")



adj_RR$target_f <- gsub("\n"," ",adj_RR$target_f)

adj_RR <- adj_RR %>% 
  mutate(sample=factor(sample, levels=c("Fly","CH","MH","LS","S",
                                        "SW","W","any sample type"))) 

panel_spacing = 0.75
textcol = "grey20"
cols = (brewer.pal(n = 9, name = "Spectral"))
colours <- c(`<0.01 (increased risk)` = cols[1], `<0.05 (increased risk)` = cols[2], 
             `0.05-0.1 (increased risk)` = cols[3],
             `0.1-1` = cols[5],  
             `0.05-0.1 (decreased risk)` = cols[7], `<0.05 (decreased risk)` = cols[8], 
             `<0.01 (decreased risk)` = cols[9], `Not estimated` = "grey80")



#--------------------------------------------------------------------------------
# Aggregate targets
#--------------------------------------------------------------------------------
head(adj_RR)
table(adj_RR$target)

#temp<-adj_RR %>% filter(Y=="HAZ", study=="POOLED", sample=="any sample type", target=="Any pathogen")

d<-adj_RR %>% filter(!is.na(pval), !is.na(target)) %>%
  filter(sparse!="yes",
         grepl("Any ", target),
         !grepl("zoonotic", target)
         #sample!="any sample type"
         ) %>%
  arrange(sample, study, target_f)
dim(d)



# dfull <- expand_grid(unique(d$Y), unique(d$X))
# colnames(dfull) <- c("Y", "X")
# d <- left_join(dfull, d, by = c("Y", "X"))
# #d <- distinct(d)
dim(d)
dfull <- expand_grid(unique(d$Y), unique(d$X), unique(d$target))
dim(dfull)
#dfull <- expand_grid(unique(dfull$target), unique(dfull$X))
colnames(dfull) <- c("Y", "X","target")
d <- left_join(dfull, d, by = c("target", "X","Y"))
table(d$X)
d <- d %>% filter(!is.na(est) | X=="Any sample: POOLED", !is.na(Y))
d$pval_cat <- addNA(d$pval_cat)
levels(d$pval_cat) = c(levels(d$pval_cat), "Not estimated")
d$pval_cat[is.na(d$pval_cat)] <- "Not estimated"


head(d)
d <- d %>% group_by(study, Y, target) %>%
  mutate(Nsamp=n()) %>%
  filter(!(Nsamp==2 & grepl("any",sample) & study!="POOLED"))
table(d$Nsamp)
table(d$sample)

d$est = gsub("NA \\(NA, NA\\)", "", d$est)

d <- d %>% filter(!is.na(target)) %>% droplevels()


unique(d$target)
d$target[d$target=="Any MST"] <- "Any human\nor animal MST"
hm_df_agg <- d
hm_df_agg <- hm_df_agg %>% mutate(target = factor(target,  
                  levels = c(
                    "Any pathogen","Any bacteria","Any virus","Any protozoa",
                    "Any STH","Any human\nor animal MST", "Any human MST","Any animal MST")))

#Drop empty "any sample: POOLED"
hm_df_agg <- hm_df_agg %>%  group_by(X,target) %>% filter(!(X=="Any sample: POOLED"& n()==sum(is.na(pval))))

#plot:
hm_agg <- heatmap_plot(hm_df_agg, colours=colours)
hm_agg

# temp <- hm_df_agg %>% filter(X=="Any sample: POOLED", Y=='HAZ')
# 
# hm_agg <- heatmap_plot(temp, colours=colours)
# hm_agg

ggsave(hm_agg, file = paste0(here::here(),"/figures/pngs/p_aggregate_heatmap.png"), width = 10, height = 26)


#--------------------------------------------------------------------------------
# Specific pathogens
#--------------------------------------------------------------------------------


head(adj_RR)
d<-adj_RR %>% filter(!is.na(pval), !is.na(target)) %>%
   filter(sparse!="yes", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  arrange(sample, study, target_f)
dim(d)


dfull <- expand_grid(unique(d$Y), unique(d$X), unique(d$target))
colnames(dfull) <- c("Y", "X","target")
d <- left_join(dfull, d, by = c("target", "X","Y"))
table(d$X)
d <- d %>% filter(!is.na(est) | X=="Any sample: POOLED", !is.na(Y))
d$pval_cat <- addNA(d$pval_cat)
levels(d$pval_cat) = c(levels(d$pval_cat), "Not estimated")
d$pval_cat[is.na(d$pval_cat)] <- "Not estimated"



d <- d %>% group_by(study, Y, target) %>%
  mutate(Nsamp=n()) %>%
  filter(!(Nsamp==2 & grepl("any",sample)))
d$est = gsub("NA \\(NA, NA\\)", "", d$est)

d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_path <- d

#Drop empty "any sample: POOLED"
hm_df_path <- hm_df_path %>%  group_by(X,target) %>% filter(!(X=="Any sample: POOLED"& n()==sum(is.na(pval))))

#plot:
hm <- heatmap_plot(hm_df_path, colours=colours)

ggsave(hm, file = paste0(here::here(),"/figures/pngs/p_path_heatmap.png"), width = 10, height = 26)


#--------------------------------------------------------------------------------
# Specific MST
#--------------------------------------------------------------------------------


head(adj_RR)
d<-adj_RR %>% filter(!is.na(pval), !is.na(target)) %>%
  filter(sparse!="yes", target %in% any_MST, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  arrange(sample, study, target_f)
dim(d)



dfull <- expand_grid(unique(d$Y), unique(d$X), unique(d$target))
colnames(dfull) <- c("Y", "X","target")
d <- left_join(dfull, d, by = c("target", "X","Y"))
table(d$X)
d <- d %>% filter(!is.na(est) | X=="Any sample: POOLED", !is.na(Y))
d$pval_cat <- addNA(d$pval_cat)
levels(d$pval_cat) = c(levels(d$pval_cat), "Not estimated")
d$pval_cat[is.na(d$pval_cat)] <- "Not estimated"



#only keep aggregate if it is different than main
#d <- d %>% distinct(study, Y, est, target, .keep_all = T)
d <- d %>% group_by(study, Y, target) %>%
  mutate(Nsamp=n()) %>%
  filter(!(Nsamp==2 & grepl("any",sample)))
d$est = gsub("NA \\(NA, NA\\)", "", d$est)

d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_mst <- d

df <- hm_df_mst %>% filter(target=="Human (HumM2)"|target=="Avian (GFD)", Y=="Diarrhoea")
df

#Drop empty "any sample: POOLED"
hm_df_mst <- hm_df_mst %>%  group_by(X,target) %>% filter(!(X=="Any sample: POOLED"& n()==sum(is.na(pval))))

#plot:
hm_mst <- heatmap_plot(hm_df_mst, colours=colours)

ggsave(hm_mst, file = paste0(here::here(),"/figures/pngs/p_MST_heatmap.png"), width = 10, height = 26)

hm_df <-d

save(hm_df_agg, hm_df_path, hm_df_mst, hm_agg, hm, hm_mst, file=here("figures/aim2_path_heatmap.Rdata"))

