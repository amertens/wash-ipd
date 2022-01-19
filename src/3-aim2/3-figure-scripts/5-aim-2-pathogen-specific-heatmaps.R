
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 
d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data.rds"))

temp<-adj_RR%>%filter(Y=="diar7d", target=="Any MST")
table(temp$sample_cat)

unique(adj_RR$target)

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
# adj_RR$sparse[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- "yes"
# adj_RR$RR[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- NA
# adj_RR$ci.lb[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- NA
# adj_RR$ci.ub[adj_RR$Y=="diar7d" & adj_RR$n < 20] <- NA

res <- adj_RR %>% filter(target %in% c("Any MST"), 
                         Y=="diar7d", study=="Holcomb 2020",
                         sample=="any sample type")
res$N_W
res$W

#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

#unadj_RR<-unadj_RR %>% filter(!is.na(se))
sample_cats = levels(unadj_RR$sample_cat)[levels(unadj_RR$sample_cat)!="Any sample"]


#----------------------------------------------------
# Specific pathogens
#----------------------------------------------------
# diar_adj_path_df <- adj_RR %>% 
#   filter( Y=="diar7d", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) #%>%
#   #base_plot(drop_full_sparse=T, facet_lab_size = 8)

#Drop sparse and any repeat of the any sample type when only one sample type
# diar_adj_path_df <- diar_adj_path_df %>% group_by(target) %>%
#   filter(n()!=sum(sparse=="yes")) %>% ungroup()

#Drop carraige returns
adj_RR$target_f <- gsub("\n"," ",adj_RR$target_f)

# diar_adj_path_df <- adj_RR %>% 
#   filter(sparse=="no", Y=="diar7d", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
#   group_by(target, study) %>%
#   mutate(N=n()) %>%
#   filter(!(N==2 & sample=="any sample type")) %>% ungroup()
# table(diar_adj_path_df$N)
# table(diar_adj_path_df$N, diar_adj_path_df$sample)
# 
# table(d$study)
# table(d$target)
# temp <- diar_adj_path_df %>% filter(study=="Capone 2021 in prep", target=="Pathogenic E. coli")
# head(temp)

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


adj_RR <- adj_RR %>% 
  mutate(sample=factor(sample, levels=c("Fly","CH","MH","LS","S",
                                        "SW","W","any sample type"))) 



#--------------------------------------------------------------------------------
# Aggregate targets
#--------------------------------------------------------------------------------
head(adj_RR)
table(adj_RR$target)

d<-adj_RR %>% filter(!is.na(pval), !is.na(target)) %>%
  filter(sparse=="no",
         grepl("Any ", target),
         !grepl("zoonotic", target)
         #sample!="any sample type"
         ) %>%
  arrange(sample, target_f, study)
dim(d)

unique(d$sample)

length(unique(d$target))
length(unique(d$sample))
length(unique(d$study))
length(unique(d$Y))

unique(d$sample)
unique(d$target)

d <- d %>% 
  mutate(target=factor(target, levels=c("Any pathogen", "Any bacteria", "Any virus", "Any protozoa","Any STH",
      "Any MST","Any animal MST", "Any human MST","Any general MST"))) 

d %>% filter(target=="Any MST", study=="Fuhrmeister 2020", Y=="waz")
d %>% filter(target=="Any MST", study=="Fuhrmeister 2020", Y=="whz")


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

d$pval_cat <- cut(d$pval, breaks = c(-1, 0.01, 0.05, 0.2, 0.5, 2), 
                  labels = c("<0.01", "<0.05", "0.05-0.2","0.2-0.5", "0.5-1"))
d$pval_cat <- ifelse(d$sign == -1, paste0(d$pval_cat, " increase risk"), 
                     paste0(d$pval_cat, " decrease risk"))
d$pval_cat[d$pval_cat %in% c("0.5-1 decrease risk", "0.5-1 increase risk")] <- "0.5-1"
table(d$pval_cat)
d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease risk", 
                                            "<0.05 decrease risk", "0.05-0.2 decrease risk", "0.2-0.5 decrease risk", 
                                            "0.5-1", "0.05-0.2 increase risk", "0.2-0.5 increase risk", 
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
             `0.05-0.2 increase risk` = cols[3], `0.2-0.5 increase risk` = cols[4], 
             `0.5-1` = cols[5], `0.2-0.5 decrease risk` = cols[6], 
             `0.05-0.2 decrease risk` = cols[7], `<0.05 decrease risk` = cols[8], 
             `<0.01 decrease risk` = cols[9], `Not estimated` = "gray80")


d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_agg <- d

hm_agg <- ggplot(hm_df_agg, aes(x = Y, y = X, fill = pval_cat )) + 
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
hm_agg

ggsave(hm_agg, file = paste0(here::here(),"/figures/pngs/p_aggregate_heatmap.png"), width = 10, height = 26)


#--------------------------------------------------------------------------------
# Specific pathogens
#--------------------------------------------------------------------------------


head(adj_RR)
d<-adj_RR %>% filter(!is.na(pval), !is.na(target)) %>%
   filter(sparse=="no", target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  arrange(target_f, sample, study)
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

d$pval_cat <- cut(d$pval, breaks = c(-1, 0.01, 0.05, 0.2, 0.5, 2), 
                  labels = c("<0.01", "<0.05", "0.05-0.2","0.2-0.5", "0.5-1"))
d$pval_cat <- ifelse(d$sign == -1, paste0(d$pval_cat, " increase risk"), 
                     paste0(d$pval_cat, " decrease risk"))
d$pval_cat[d$pval_cat %in% c("0.5-1 decrease risk", "0.5-1 increase risk")] <- "0.5-1"
table(d$pval_cat)
d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease risk", 
                                            "<0.05 decrease risk", "0.05-0.2 decrease risk", "0.2-0.5 decrease risk", 
                                            "0.5-1", "0.05-0.2 increase risk", "0.2-0.5 increase risk", 
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
             `0.05-0.2 increase risk` = cols[3], `0.2-0.5 increase risk` = cols[4], 
             `0.5-1` = cols[5], `0.2-0.5 decrease risk` = cols[6], 
             `0.05-0.2 decrease risk` = cols[7], `<0.05 decrease risk` = cols[8], 
             `<0.01 decrease risk` = cols[9], `Not estimated` = "gray80")


d <- d %>% filter(!is.na(target)) %>% droplevels()
hm_df_path <- d

hm <- ggplot(hm_df_path, aes(x = Y, y = X, fill = pval_cat )) + 
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

ggsave(hm, file = paste0(here::here(),"/figures/pngs/p_path_heatmap.png"), width = 10, height = 26)


#--------------------------------------------------------------------------------
# Specific MST
#--------------------------------------------------------------------------------


head(adj_RR)
d<-adj_RR %>% filter(!is.na(pval), !is.na(target)) %>%
  filter(sparse=="no", target %in% any_MST, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  arrange(target_f, sample, study)
dim(d)

length(unique(d$target))
length(unique(d$sample))
length(unique(d$study))
length(unique(d$Y))

d$X <- paste0(d$sample_cat,": ", d$study)
d$X <- factor(d$X, levels=unique(d$X))
d$Y <- factor(d$Y, levels=c("diar7d", "haz","waz","whz",  "underwt","stunt","wast"   ))
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

d$pval_cat <- cut(d$pval, breaks = c(-1, 0.01, 0.05, 0.2, 0.5, 2), 
                  labels = c("<0.01", "<0.05", "0.05-0.2","0.2-0.5", "0.5-1"))
d$pval_cat <- ifelse(d$sign == -1, paste0(d$pval_cat, " increase risk"), 
                     paste0(d$pval_cat, " decrease risk"))
d$pval_cat[d$pval_cat %in% c("0.5-1 decrease risk", "0.5-1 increase risk")] <- "0.5-1"
table(d$pval_cat)
d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease risk", 
                                            "<0.05 decrease risk", "0.05-0.2 decrease risk", "0.2-0.5 decrease risk", 
                                            "0.5-1", "0.05-0.2 increase risk", "0.2-0.5 increase risk", 
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
hm_df_mst <- d


hm_mst <- ggplot(hm_df_mst, aes(x = Y, y = X, fill = pval_cat )) + 
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

ggsave(hm_mst, file = paste0(here::here(),"/figures/pngs/p_MST_heatmap.png"), width = 10, height = 26)

hm_df <-d

save(hm_df_agg, hm_df_path, hm_df_mst, hm_agg, hm, hm_mst, file=here("figures/aim2_path_heatmap.Rdata"))


# p_diar_adj_path <- ggplot(data = diar_adj_path_df, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
#   geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),  position = position_dodge(0.5),
#                 width = 0.3, size = 1) +
#   geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
#   geom_text(aes(label=sig_cat), color="black", position = position_dodge(0.5), vjust = -0.1) +
#   scale_color_manual(breaks = legend_labels,
#                      values = colours, drop = FALSE) +
#   scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
#   geom_hline(yintercept = 1, linetype="dashed") +
#   facet_wrap(~target_f,  scales="free", ncol=3) +
#   scale_y_continuous(
#     breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
#     trans='log10', 
#     labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
#   ) +
#   coord_flip() +
#   labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
#   theme_ki() + 
#   theme(axis.ticks.x=element_blank(),
#         legend.position = c(0.6,0.1),
#         strip.text.x = element_text(size=10, face = "bold"),
#         strip.text.y = element_text(size=11, angle = 270, face = "bold"),          
#         panel.spacing = unit(0, "lines")) + 
#   guides(colour = guide_legend(nrow = 2))


