
rm(list=ls())
source(here::here("0-config.R"))
library(RColorBrewer)

adj_diff <- readRDS(here("results/adjusted_aim2_abund_res.Rds"))



unique(adj_diff$target)

#count number of covariates
adj_diff$N_W <- str_count(adj_diff$W,",") + 1
adj_diff$N_W[adj_diff$W=="unadjusted"] <- 0


table(adj_diff$Y)
table(adj_diff$minN)
table(adj_diff$a)
table(adj_diff$c)
table(adj_diff$a+adj_diff$c)

summary(adj_diff$RR)
adj_diff$ci.lb[adj_diff$RR < 0.05 & !is.na(adj_diff$RR)] <- NA
adj_diff$ci.ub[adj_diff$RR < 0.05 & !is.na(adj_diff$RR)] <- NA
adj_diff$sparse[adj_diff$RR < 0.05 & !is.na(adj_diff$RR)] <- "yes"
adj_diff$sample_cat[adj_diff$RR < 0.05 & !is.na(adj_diff$RR)] <- "Sparse data"
adj_diff$RR[adj_diff$RR < 0.05 & !is.na(adj_diff$RR)] <- 1

adj_diff <- adj_diff %>% mutate(
  sig_cat = case_when(
    pval<0.001 ~"***",
    pval<0.01 ~"**",
    pval<0.05 ~"*",
    pval>=0.05 ~""
  )
)

adj_diff$pooled <- ifelse(grepl("Pooled",(adj_diff$study)),1,0)
adj_diff$study <- gsub("Pooled","POOLED",adj_diff$study)

#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

#adj_diff<-adj_diff %>% filter(!is.na(se))
sample_cats = levels(adj_diff$sample_cat)[levels(adj_diff$sample_cat)!="Any sample"]
adj_diff <- clean_res(adj_diff)

#----------------------------------------------------
# Specific pathogens
#----------------------------------------------------

#Drop carraige returns
adj_diff$target_f <- gsub("\n"," ",adj_diff$target_f)
legend_labels=sample_cats
drop_full_sparse=F
my_colors = c("grey20",carto_pal(12, "Prism"))

colours <- c("Any sample" = my_colors[1],
             "Source water" = my_colors[3],
             "Stored water"  = my_colors[4],
             "Child hands"  = my_colors[7],
             "Mother's hands" = my_colors[8],
             "Latrine soil" = my_colors[5],
             "House soil" = my_colors[6],
             "Flies" = my_colors[9],
             "Sparse data" = "grey50")


adj_diff <- adj_diff %>% 
  mutate(sample=factor(sample, levels=c("Fly","CH","MH","LS","S",
                                        "SW","W","any sample type"))) 



adj_diff$X <- paste0(adj_diff$sample_cat,": ", adj_diff$study)
adj_diff$X <- factor(adj_diff$X, levels=unique(adj_diff$X))
adj_diff$Y <- factor(adj_diff$Y, levels=c("diar7d", "haz","waz","whz",  "underwt","stunt","wast"))
adj_diff$Y <- recode_factor(adj_diff$Y, 
                     "diar7d"="Diarrhea", 
                     "haz"="HAZ",
                     "waz"="WAZ",
                     "whz"="WHZ",  
                     "underwt"="Underweight",
                     "stunt"="Stunting",
                     "wast"="Wasting")


adj_diff$sign <- NA
unique(adj_diff$Y)
adj_diff$point.diff <- ifelse(adj_diff$Y %in% c("Diarrhea","Stunting","Wasting","Underweight"), adj_diff$RR, adj_diff$coef)
adj_diff$point.diff[adj_diff$Y %in% c("Diarrhea","Stunting","Wasting","Underweight")]

adj_diff$sign <- ifelse(adj_diff$Y %in% c("Diarrhea","Stunting","Wasting","Underweight"),ifelse(adj_diff$point.diff > 1, -1, 1), sign(adj_diff$point.diff))
table(adj_diff$sign)


adj_diff$pval_cat <- cut(adj_diff$pval, breaks = c(-1, 0.01, 0.05, 0.2, 2), 
                  labels = c("<0.01", "<0.05", "0.05-0.2", "0.2-1"))
adj_diff$pval_cat <- ifelse(adj_diff$sign == -1, paste0(adj_diff$pval_cat, " increase risk"), 
                     paste0(adj_diff$pval_cat, " decrease risk"))
adj_diff$pval_cat[adj_diff$pval_cat %in% c("0.2-1 decrease risk", "0.2-1 increase risk")] <- "0.2-1"
table(adj_diff$pval_cat)
adj_diff$pval_cat <- factor(adj_diff$pval_cat, levels = c("<0.01 decrease risk", 
                                            "<0.05 decrease risk", "0.05-0.2 decrease risk", 
                                            "0.2-1", "0.05-0.2 increase risk", 
                                            "<0.05 increase risk", "<0.01 increase risk"))

adj_diff$pval_cat <- addNA(adj_diff$pval_cat)
levels(adj_diff$pval_cat) = c(levels(adj_diff$pval_cat), "Sparse")
adj_diff$pval_cat[is.na(adj_diff$pval_cat)] <- "Sparse"
table(adj_diff$pval_cat)
table(is.na(adj_diff$pval_cat))
adj_diff$est = ""
adj_diff$est = paste0(round(adj_diff$point.diff, 2), " (", round(adj_diff$ci.lb, 2), ", ", round(adj_diff$ci.ub, 2), ")")


#only keep aggregate if it is different than main
#d <- d %>% distinct(study, Y, est, .keep_all = T)
adj_diff <- adj_diff %>% group_by(study, Y, target) %>%
  mutate(Nsamp=n()) %>%
  filter(!(Nsamp==2 & grepl("any",sample)))



adj_diff$est = gsub("NA \\(NA, NA\\)", "", adj_diff$est)
textcol = "grey20"
cols = (brewer.pal(n = 9, name = "Spectral"))
colours <- c(`<0.01 increase risk` = cols[1], `<0.05 increase risk` = cols[2], 
             `0.05-0.2 increase risk` = cols[3],
             `0.2-1` = cols[5],  
             `0.05-0.2 decrease risk` = cols[7], `<0.05 decrease risk` = cols[8], 
             `<0.01 decrease risk` = cols[9], `Not estimated` = "gray80")

panel_spacing = 0.75


#--------------------------------------------------------------------------------
# Specific pathogens
#--------------------------------------------------------------------------------


head(adj_diff)
d<-adj_diff %>% filter(!is.na(pval), !is.na(target)) %>%
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

d$est = gsub("NA \\(NA, NA\\)", "", d$est)



d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_path_abund <- d

hm <- heatmap_plot(hm_df_path_abund, colours=colours)
hm

#--------------------------------------------------------------------------------
# Specific MST
#--------------------------------------------------------------------------------


head(adj_diff)
d<-adj_diff %>% filter(!is.na(pval), !is.na(target)) %>%
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


d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_mst_abund <- d

hm_mst <- heatmap_plot(hm_df_mst_abund, colours=colours)
hm_mst


save(hm_df_path_abund, hm_df_mst_abund, file=here("figures/aim2_abund_heatmap.Rdata"))

