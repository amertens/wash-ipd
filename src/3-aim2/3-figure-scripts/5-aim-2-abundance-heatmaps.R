
rm(list=ls())
source(here::here("0-config.R"))

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



#--------------------------------------------------------------------------------
# Specific pathogens
#--------------------------------------------------------------------------------


head(adj_diff)
d<-adj_diff %>% filter(!is.na(pval), !is.na(target)) %>%
   filter(sparse!="yes", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  arrange(sample, study, target_f)
dim(d)

length(unique(d$target))
length(unique(d$sample))
length(unique(d$study))
length(unique(d$Y))

unique(d$sample)

d$X <- paste0(d$sample_cat,": ", d$study)
d$X <- factor(d$X, levels=unique(d$X))
d$Y <- factor(d$Y, levels=c("diar7d", "haz","waz","whz",  "underwt","stunt","wast"))
d$Y <- recode_factor(d$Y, 
                     "diar7d"="Diarrhea", 
                     "haz"="HAZ",
                     "waz"="WAZ",
                     "whz"="WHZ",  
                     "underwt"="Underweight",
                     "stunt"="Stunting",
                     "wast"="Wasting")

require(RColorBrewer)
dfull <- expand_grid(unique(d$Y), unique(d$X))
colnames(dfull) <- c("Y", "X")
d <- left_join(dfull, d, by = c("Y", "X"))
d <- distinct(d)

head(d)
d$sign <- NA
unique(d$Y)
d$point.diff <- ifelse(d$Y %in% c("Diarrhea","Stunting","Wasting","Underweight"), d$RR, d$coef)
d$point.diff[d$Y %in% c("Diarrhea","Stunting","Wasting","Underweight")]

d$sign <- ifelse(d$Y %in% c("Diarrhea","Stunting","Wasting","Underweight"),ifelse(d$point.diff > 1, -1, 1), sign(d$point.diff))
table(d$sign)


d$pval_cat <- cut(d$pval, breaks = c(-1, 0.01, 0.05, 0.2, 2), 
                  labels = c("<0.01", "<0.05", "0.05-0.2", "0.2-1"))
d$pval_cat <- ifelse(d$sign == -1, paste0(d$pval_cat, " increase risk"), 
                     paste0(d$pval_cat, " decrease risk"))
d$pval_cat[d$pval_cat %in% c("0.2-1 decrease risk", "0.2-1 increase risk")] <- "0.2-1"
table(d$pval_cat)
d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease risk", 
                                            "<0.05 decrease risk", "0.05-0.2 decrease risk", 
                                            "0.2-1", "0.05-0.2 increase risk", 
                                            "<0.05 increase risk", "<0.01 increase risk"))

d$pval_cat <- addNA(d$pval_cat)
levels(d$pval_cat) = c(levels(d$pval_cat), "Sparse")
d$pval_cat[is.na(d$pval_cat)] <- "Sparse"
table(d$pval_cat)
table(is.na(d$pval_cat))
d$est = ""
d$est = paste0(round(d$point.diff, 2), " (", round(d$ci.lb, 2), ", ", round(d$ci.ub, 2), ")")


#only keep aggregate if it is different than main
d <- d %>% distinct(study, Y, est, .keep_all = T)



d$est = gsub("NA \\(NA, NA\\)", "", d$est)
textcol = "grey20"
cols = (brewer.pal(n = 9, name = "Spectral"))
colours <- c(`<0.01 increase risk` = cols[1], `<0.05 increase risk` = cols[2], 
             `0.05-0.2 increase risk` = cols[3],
             `0.2-1` = cols[5],  
             `0.05-0.2 decrease risk` = cols[7], `<0.05 decrease risk` = cols[8], 
             `<0.01 decrease risk` = cols[9], `Not estimated` = "gray80")



d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_path_abund <- d

hm <- ggplot(hm_df_path_abund, aes(x = Y, y = X, fill = pval_cat )) + 
  geom_tile(colour = "grey80", size = 0.25) + scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme_minimal(base_size = 8) +
  facet_grid(target~., scales = "free", space="free") +
  scale_fill_manual(values = colours, drop = FALSE) +
  geom_text(aes(label = est), size=2.25) +
  theme(legend.title = element_text(color = textcol,
        size = 8), legend.margin = margin(grid::unit(0.1, "cm")), legend.text = element_text(colour = textcol,
        size = 7, face = "bold"), legend.key.height = grid::unit(0.2,   "cm"), legend.key.width = grid::unit(1, "cm"),
        legend.position = "bottom", axis.text.x = element_text(size = 8,  colour = textcol), 
        axis.text.y = element_text(size = 8, vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4),
        plot.title = element_text(colour = textcol, hjust = 0, size = 12, face = "bold"), strip.text.x = element_text(size = 10),
        strip.text.y = element_text(angle = 0, size = 10),
        plot.background = element_blank(), panel.border = element_blank(),
        strip.background = element_blank(), panel.background = element_rect(fill = "grey80", colour = "grey80"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
        guides(fill = guide_legend("P-value strength", nrow = 2)) + labs(x = "", y = "", title = "")

hm

#--------------------------------------------------------------------------------
# Specific MST
#--------------------------------------------------------------------------------


head(adj_diff)
d<-adj_diff %>% filter(!is.na(pval), !is.na(target)) %>%
  filter(sparse!="yes", target %in% any_MST, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  arrange(sample, study, target_f)
dim(d)

length(unique(d$target))
length(unique(d$sample))
length(unique(d$study))
length(unique(d$Y))

d$X <- paste0(d$sample_cat,": ", d$study)
d$X <- factor(d$X, levels=unique(d$X))
d$Y <- factor(d$Y, levels=c("diar7d", "haz","waz","whz",  "stunt","underwt","wast"   ))
d$Y <- recode_factor(d$Y, 
                     "diar7d"="Diarrhea", 
                     "haz"="HAZ",
                     "waz"="WAZ",
                     "whz"="WHZ",  
                     "underwt"="Underweight",
                     "stunt"="Stunting",
                     "wast"="Wasting")

require(RColorBrewer)
dfull <- expand_grid(unique(d$Y), unique(d$X))
colnames(dfull) <- c("Y", "X")
d <- left_join(dfull, d, by = c("Y", "X"))
d <- distinct(d)

head(d)
d$sign <- NA
unique(d$Y)
d$point.diff <- ifelse(d$Y %in% c("Diarrhea","Stunting","Wasting","Underweight"), d$RR, d$coef)
d$sign <- ifelse(d$Y %in% c("Diarrhea","Stunting","Wasting","Underweight"),ifelse(d$point.diff > 1, -1, 1), sign(d$point.diff))
table(d$sign)

d$pval_cat <- cut(d$pval, breaks = c(-1, 0.01, 0.05, 0.2, 2), 
                  labels = c("<0.01", "<0.05", "0.05-0.2", "0.2-1"))
d$pval_cat <- ifelse(d$sign == -1, paste0(d$pval_cat, " increase risk"), 
                     paste0(d$pval_cat, " decrease risk"))
d$pval_cat[d$pval_cat %in% c("0.2-1 decrease risk", "0.2-1 increase risk")] <- "0.2-1"
table(d$pval_cat)
d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease risk", 
                                            "<0.05 decrease risk", "0.05-0.2 decrease risk", 
                                            "0.2-1", "0.05-0.2 increase risk", 
                                            "<0.05 increase risk", "<0.01 increase risk"))
d$pval_cat <- addNA(d$pval_cat)
levels(d$pval_cat) = c(levels(d$pval_cat), "Sparse")
d$pval_cat[is.na(d$pval_cat)] <- "Sparse"
table(d$pval_cat)
table(is.na(d$pval_cat))
d$est = ""
d$est = paste0(round(d$point.diff, 2), " (", round(d$ci.lb, 2), ", ", round(d$ci.ub, 2), ")")



#only keep aggregate if it is different than main
d <- d %>% distinct(study, Y, est, .keep_all = T)



d$est = gsub("NA \\(NA, NA\\)", "", d$est)
textcol = "grey20"
cols = (brewer.pal(n = 9, name = "Spectral"))
colours <- c(`<0.01 decrease risk` = cols[1], `<0.05 decrease risk` = cols[2], 
             `0.05-0.2 decrease risk` = cols[3], `0.2-0.5 decrease risk` = cols[4], 
             `0.5-1` = cols[5], `0.2-0.5 increase risk` = cols[6], 
             `0.05-0.2 increase risk` = cols[7], `<0.05 increase risk` = cols[8], 
             `<0.01 increase risk` = cols[9], `Not estimated` = "gray80")

d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_mst_abund <- d


hm_mst <- ggplot(hm_df_mst_abund, aes(x = Y, y = X, fill = pval_cat )) + 
  geom_tile(colour = "grey80", size = 0.25) + scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme_minimal(base_size = 8) +
  facet_grid(target~., scales = "free", space="free") +
  scale_fill_manual(values = colours, drop = FALSE) +
  geom_text(aes(label = est), size=2.25) +
  theme(legend.title = element_text(color = textcol,
                                    size = 8), legend.margin = margin(grid::unit(0.1, "cm")), legend.text = element_text(colour = textcol,
                                                                                                                         size = 7, face = "bold"), legend.key.height = grid::unit(0.2,   "cm"), legend.key.width = grid::unit(1, "cm"),
        legend.position = "bottom", axis.text.x = element_text(size = 8,  colour = textcol), 
        axis.text.y = element_text(size = 8, vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4),
        plot.title = element_text(colour = textcol, hjust = 0, size = 12, face = "bold"), strip.text.x = element_text(size = 10),
        strip.text.y = element_text(angle = 0, size = 10),
        plot.background = element_blank(), panel.border = element_blank(),
        strip.background = element_blank(), panel.background = element_rect(fill = "grey80", colour = "grey80"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend("P-value strength", nrow = 2)) + labs(x = "", y = "", title = "")



save(hm_df_path_abund, hm_df_mst_abund, file=here("figures/aim2_abund_heatmap.Rdata"))

