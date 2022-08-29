
rm(list=ls())
source(here::here("0-config.R"))
library(scales)

unadj_RR <- readRDS(file=here("results/unadjusted_aim1_RR_pooled.Rds")) 
unadj_RD <- readRDS(file=here("results/unadjusted_aim1_RD.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR_pooled.Rds")) 
adj_RD <- readRDS(file=here("results/adjusted_aim1_RD.Rds")) 


unadj_diff <- readRDS(file=here("results/unadjusted_aim2_abund_res.Rds"))
adj_diff <- readRDS(file=here("results/adjusted_aim2_abund_res.Rds"))

unadj_diff <- clean_res(unadj_diff) 
adj_diff <- clean_res(adj_diff) 

adj_RR$sample_cat[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022 in prep" & adj_RR$target=="Any virus" ] <- "Sparse data"
adj_RR$RR[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022 in prep" & adj_RR$target=="Any virus" ] <-  1
adj_RR$ci.lb[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022 in prep" & adj_RR$target=="Any virus" ] <- NA
adj_RR$ci.ub[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022 in prep" & adj_RR$target=="Any virus" ] <- NA
adj_RR$sparse[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022 in prep" & adj_RR$target=="Any virus" ] <- "yes"


#---------------------------------------------------------------
#function to clean results/order factors
#---------------------------------------------------------------

unique(unadj_RD$target)
unique(unadj_RD$sample)

d <- unadj_RR
table(d$sparse)
table(d$sample_cat )
table(d$sample)
target_lev=target_levels



#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

#unadj_RR <- clean_res(unadj_RR)
unadj_RD <- clean_res(unadj_RD)
adj_RD <- clean_res(adj_RD)


#see if any levels are missing
unadj_RR$target[is.na(unadj_RR$target_f)]

sample_cats = levels(unadj_RR$sample_cat)[levels(unadj_RR$sample_cat)!="Any sample"]


adj_RR$est <- paste0(sprintf("%.2f",adj_RR$RR)," (",
                     sprintf("%.2f",adj_RR$ci.lb),", ",
                     sprintf("%.2f",adj_RR$ci.ub),")")
adj_RR$est[adj_RR$study!="Pooled"] <- ""

adj_diff$est <- paste0(sprintf("%.2f",adj_diff$coef)," (",
                       sprintf("%.2f",adj_diff$ci.lb),", ",
                       sprintf("%.2f",adj_diff$ci.ub),")")
adj_diff$est[adj_diff$study!="Pooled"] <- ""

unadj_RR$est <- paste0(sprintf("%.2f",unadj_RR$RR)," (",
                       sprintf("%.2f",unadj_RR$ci.lb),", ",
                       sprintf("%.2f",unadj_RR$ci.ub),")")
unadj_RR$est[unadj_RR$study!="Pooled"] <- ""

unadj_diff$est <- paste0(sprintf("%.2f",unadj_diff$coef)," (",
                         sprintf("%.2f",unadj_diff$ci.lb),", ",
                         sprintf("%.2f",unadj_diff$ci.ub),")")
unadj_diff$est[adj_diff$study!="Pooled"] <- ""

#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------

mydf <- adj_RR %>% 
  filter(target %in% c("Any pathogen"), sample=="any sample type") %>% droplevels()
            
  
  drop_full_sparse=T
  Y_range=c(0.5,2)
legend_labels=sample_cats
 lab_width=10

  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies" = my_colors[9],
               # "Flies in kitchen" = my_colors[9],
               # "Flies in latrine" = my_colors[10],
               "Sparse data" = "grey50")
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% mutate(sparse=factor(sparse, levels=c("no","yes","pooled"))) %>%
    droplevels(.)
  
  shapes <- c("no" = 16,
              "yes" = 13,
              "pooled"  = 18)
  
  axis_range <- range(Y_range)
  axis_range[1] <- axis_range[1]/1.1
  axis_range[2] <- axis_range[2]*1.1
  
p_pres_1 <-  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=sparse))) + 
    geom_point(size=2, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.2, size = 0.75) +
    scale_color_manual(breaks = legend_labels,
                       values = colours, drop = FALSE, guide="none") +
    scale_shape_manual(values=shapes, guide="none",  drop = TRUE) + 
    geom_hline(yintercept = 1, linetype="dashed") +
    #facet_grid(target_f~sample_type,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = lab_width, multi_line = TRUE)) +
    scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
      breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
      trans='log10', 
      labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
    ) + coord_flip(ylim=axis_range)+
    labs(color="Sample type") + xlab("") + ylab("Prevalence ratio (Intervention vs. control)") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 


p_pres_1


ggsave(p_pres_1, file = paste0(here::here(),"/figures/pngs/pres_aim1_main_adj.png"), width = 4.5, height = 2)
