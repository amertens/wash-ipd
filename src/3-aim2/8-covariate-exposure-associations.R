
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
head(d)
d <- droplevels(d)
table(d$study, d$pos)
table(d$study)
table(d$study, d$hhwealth)
for(i in unique(d$study)){
  print(summary(d$hhwealth_cont[d$study==i]))
}
table(d$study, d$sample, d$Nhh)

d <- d %>% filter(sample!="FP") %>% droplevels()


#subset data to single obs per sample (ignoring multiple children)
d <- d %>% distinct(study, sample, target, clusterid, hhid, pos, .keep_all = T)

# 1.	Child birth order/parity 
# 2.	Asset-based wealth index 
# 3.	Number of individuals and children in household
# 4.	Household food security 
# 5.	Household electrification and construction, including wall/roof material 
# 6.	Parental age 
# 7.	Parental education 
# 8.	Parental employment 
# a.	Indicator for works in agriculture 
# 9.	Land ownership 

Wvars = c("sex","age","hfiacat","momage","hhwealth_cont", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#-----------------------------------
#washb_prescreen by study and outcome for all covariates
#-----------------------------------

prescreen_pval <- function (Y, W, family = "binomial", pval = 0.2, print = TRUE){
  require(lmtest)
  if (pval > 1 | pval < 0) {
    stop("P-value threshold not set between 0 and 1.")
  }
  Ws <- as.data.frame(W)
  dat <- bind_cols(Ws, data.frame(Y=Y))
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  LRp <- data.frame(var= names(Ws), pval=NA)

  for (i in 1:nW) {
    dat$W <- dat[, i]
    df <- data.frame(Y=dat$Y, W=dat$W)
    df <- df[complete.cases(df), ]
    
    if (class(dat$W) == "factor" & dim(table(dat$W)) == 
        1) {
      fit1 <- fit0 <- glm(Y ~ 1, data = df, family = family)
    }
    else {
      fit1 <- glm(Y ~ W, data = df, family = family)
      fit0 <- glm(Y ~ 1, data = df, family = family)
    }
    LRp$pval[i] <- lrtest(fit1, fit0)[2, 5]
  }

    return(LRp)
}


calc_cov_p <- function(d, Ws, study, sample, target){
  df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
  cat(levels(df$study),", ", sample,", ", target,"\n")
  cat("N before dropping missing: ", nrow(df),"\n")
  
  
  df <- df %>% filter(!is.na(pos))
  Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
  
  res=NULL
  try(res <- prescreen_pval(Y=df$pos, W=Wdf, family="binomial", print=F))
  if(is.null(res)){
    res <- data.frame(var="failed", pval=NA)
  }
  return(res)
}


head(d)
res <- d %>% group_by(study, sample, target) %>%
    do(calc_cov_p(., Ws=Wvars, study=.$study[1], sample=.$sample[1], target=.$target[1]))

unique(res$var)
res$var <- recode(res$var,  
                  nrooms="Number of rooms",
                  hhwealth_cont="HH wealth",
                  landacre="Acres owned",
                  momedu="Mom edu.",
                  hfiacat="HH food security",
                  floor="Improved floor",
                  elec="Electricity",
                  walls="Improved walls",
                  roof="Improved roof",
                  momage="Mom age",
                  age="Child age",
                  tr="Treatment",
                  dadagri="In agriculture",
                  sex="Sex",
                  Nhh="HH size",
                  landown="Owns land")


saveRDS(res, file=here("results/covariate_associations.Rds"))

#-----------------------------------
#heatmap per study and sample by target combination and covariate p-value 
#-----------------------------------

require(RColorBrewer)
res$pval[res$pval==1] <- NA
res <- res %>% filter(var!="failed", !is.na(pval))
textcol=tableau11[1]

# dfull <- expand_grid(unique(d$Y), unique(d$X))
# colnames(dfull) <- c("Y", "X")
# d <- left_join(dfull, d, by = c("Y", "X"))
# d <- distinct(d)
res$pval_cat <- cut(res$pval, breaks = c(-1, 0.001, 0.01, 0.1, 0.05, 0.2, 
                                     0.5, 2), labels = c("<0.001","<0.01", "<0.05", "0.05-0.1", 
                                                         "0.1-0.2", "0.2-0.5", "0.5-1"))

table(res$pval_cat)
res$pval_cat <- factor(res$pval_cat, levels = c("<0.001","<0.01", "<0.05", "0.05-0.1", 
                                                "0.1-0.2","0.2-0.5", "0.5-1"))
# res$pval_cat <- addNA(res$pval_cat)
# levels(res$pval_cat) = c(levels(res$pval_cat), "Not available")
# res$pval_cat[is.na(res$pval_cat)] <- "Not available"
table(res$pval_cat)
table(is.na(res$pval_cat))

#cols = rev(brewer.pal(n = 7, name = "viridis"))
cols=rev(viridis::viridis(n=7))
colours <- c(`<0.001` = cols[1], `<0.01` = cols[2], `<0.05` = cols[3], 
             `0.05-0.1` = cols[4], `0.1-0.2` = cols[5], `0.2-0.5` = cols[6], 
             `0.5-1` = cols[7])
res <- droplevels(res)
res <- res %>% group_by(var) %>%
  mutate(aveP=mean(pval)) %>% ungroup() %>%
  arrange(aveP) %>%
  mutate(var=factor(var, levels=unique(var)),
         target=factor(target, levels=rev(unique(target))))

#-----------------------------------
#heatmap with all targets
#-----------------------------------


plot_heatmap_all <- function(res, study){
  plot_df <- res %>% filter(study=={{study}})
  
  p <-  ggplot(plot_df, aes(x=var, y=target, fill=pval_cat)) +
      geom_tile(colour = "grey80", size = 0.25) + 
      facet_wrap(~sample, scales = "free", ncol=2) +
      scale_x_discrete(expand = c(0, 0)) + 
      scale_y_discrete(expand = c(0, 0)) + 
      theme_minimal(base_size = 10) + 
      scale_fill_manual(values = colours, drop = FALSE) + 
      #geom_text(aes(label = est)) + 
      theme(aspect.ratio = 1, legend.title = element_text(color = textcol, 
             size = 8), legend.margin = margin(grid::unit(0.1,"cm")), 
            legend.text = element_text(colour = textcol, size = 7, face = "bold"), legend.key.height = grid::unit(0.2, 
           "cm"), legend.key.width = grid::unit(1, "cm"), 
            legend.position = "right", axis.text.y = element_text(size = 8, 
            vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4), 
            plot.title = element_text(colour = textcol, hjust = 0, 
            size = 12, face = "bold"), strip.text.x = element_text(size = 10), 
            axis.text.x = element_text(size = 8, angle = -45, hjust = 0, colour = textcol),
            strip.text.y = element_text(angle = 0, size = 10), 
            plot.background = element_blank(), panel.border = element_blank(), 
            strip.background = element_blank(), panel.background = element_rect(fill = "grey80", 
            colour = "grey80"), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + guides(fill = guide_legend("P-value strength", 
            ncol = 1)) + labs(x = "Covariate", y = "Target", title = study)
  
  return(p)
}

unique(res$study)
p_fuhr <- plot_heatmap_all(res, study="Fuhrmeister 2020")
p_boehm <- plot_heatmap_all(res, study="Boehm 2016")
p_kwong <- plot_heatmap_all(res, study="Kwong 2021")
p_stein <- plot_heatmap_all(res, study="Steinbaum 2019")
p_holc <- plot_heatmap_all(res, study="Holcomb 2020")
p_cap1 <- plot_heatmap_all(res, study="Capone 2021")
p_cap2 <- plot_heatmap_all(res, study="Capone 2021 in prep")
p_reese <- plot_heatmap_all(res, study="Reese 2017")
p_odag <- plot_heatmap_all(res, study="Odagiri 2016")


save(list=ls(pattern="p_"), file=here("figures/aim2_covar_sig_figures.Rdata"))


# #-----------------------------------
# #heatmap with all targets
# #-----------------------------------
# 
# 
# 
# plot_df <- res %>% filter(target=="Any pathogen") %>% 
#   filter(!is.na(pval)) %>%
#   droplevels()
# p <-  ggplot(plot_df, aes(x=sample, y=var, fill=pval_cat)) +
#   geom_tile(colour = "grey80", size = 0.25) + 
#   facet_wrap(~study, scales = "free") +
#   scale_x_discrete(expand = c(0, 0)) + 
#   scale_y_discrete(expand = c(0, 0)) + 
#   theme_minimal(base_size = 10) + 
#   scale_fill_manual(values = colours, drop = FALSE) + 
#   #geom_text(aes(label = est)) + 
#   theme(aspect.ratio = 1, legend.title = element_text(color = textcol, 
#                                                       size = 8), legend.margin = margin(grid::unit(0.1,"cm")), 
#         legend.text = element_text(colour = textcol, size = 7, face = "bold"), legend.key.height = grid::unit(0.2, 
#                                                         "cm"), legend.key.width = grid::unit(1, "cm"), 
#         legend.position = "right", axis.text.x = element_text(size = 8, 
#                                                               colour = textcol), axis.text.y = element_text(size = 8, 
#                                                               vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4), 
#         plot.title = element_text(colour = textcol, hjust = 0, 
#                                   size = 12, face = "bold"), strip.text.x = element_text(size = 10), 
#         strip.text.y = element_text(angle = 0, size = 10), 
#         plot.background = element_blank(), panel.border = element_blank(), 
#         strip.background = element_blank(), panel.background = element_rect(fill = "grey80", 
#                                                                             colour = "grey80"), panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) + guides(fill = guide_legend("P-value strength", 
#                                                                          ncol = 1)) #+ labs(x = Exposure, y = Outcome, title = title)
# 
# p
# 
# 
# 
# 
# plot_df2 <- res %>% filter(sample=="any sample type") %>% 
#   filter(!is.na(pval)) %>%
#   droplevels()
# 
# p <-  ggplot(plot_df2, aes(x=target, y=var, fill=pval_cat)) +
#   geom_tile(colour = "grey80", size = 0.25) + 
#   facet_wrap(~study, scales = "free") +
#   scale_x_discrete(expand = c(0, 0)) + 
#   scale_y_discrete(expand = c(0, 0)) + 
#   theme_minimal(base_size = 10) + 
#   scale_fill_manual(values = colours, drop = FALSE) + 
#   #geom_text(aes(label = est)) + 
#   theme(aspect.ratio = 1, legend.title = element_text(color = textcol, 
#                                                       size = 8), legend.margin = margin(grid::unit(0.1,"cm")), 
#         legend.text = element_text(colour = textcol, size = 7, face = "bold"), legend.key.height = grid::unit(0.2, 
#                                                                                                               "cm"), legend.key.width = grid::unit(1, "cm"), 
#         legend.position = "right", axis.text.x = element_text(size = 8, 
#                                                               colour = textcol), axis.text.y = element_text(size = 8, 
#                                                                                                             vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4), 
#         plot.title = element_text(colour = textcol, hjust = 0, 
#                                   size = 12, face = "bold"), strip.text.x = element_text(size = 10), 
#         strip.text.y = element_text(angle = 0, size = 10), 
#         plot.background = element_blank(), panel.border = element_blank(), 
#         strip.background = element_blank(), panel.background = element_rect(fill = "grey80", 
#                                                                             colour = "grey80"), panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) + guides(fill = guide_legend("P-value strength", 
#                                                                          ncol = 1)) #+ labs(x = Exposure, y = Outcome, title = title)
# 
# p
