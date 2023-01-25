
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

adj_RR$sample_cat[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022" & adj_RR$target=="Any virus" ] <- "Sparse data"
adj_RR$RR[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022" & adj_RR$target=="Any virus" ] <-  1
adj_RR$ci.lb[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022" & adj_RR$target=="Any virus" ] <- NA
adj_RR$ci.ub[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022" & adj_RR$target=="Any virus" ] <- NA
adj_RR$sparse[adj_RR$sample=="Fly" & adj_RR$study=="Capone 2022" & adj_RR$target=="Any virus" ] <- "yes"


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
  filter(target %in% c("Any pathogen"))
drop_full_sparse=T
legend_labels=sample_cats




base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F,
                      lab_width=10,
                      # Y_breaks=c(.25, .5,1, 2, 4, 8),
                      # Y_labs=c("1/4", "1/2","1", "2", "4", "8")){
                      Y_range=c(.25, 4)){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  # colours <- c("Any sample" = my_colors[1],
  #              "Source water" = my_colors[3],
  #              "Stored water"  = my_colors[4],
  #              "Child hand rinse"  = my_colors[7],
  #              "Mother hand rinse" = my_colors[8],
  #              "Latrine soil" = my_colors[5],
  #              "House soil" = my_colors[6],
  #              "Flies" = my_colors[9],
  #              # "Flies in kitchen" = my_colors[9],
  #              # "Flies in latrine" = my_colors[10],
  #              "Sparse data" = "grey50")
  # 
  # shapes <- c("no" = 16,
  #             "yes" = 13,
  #             "pooled"  = 18)
  # 
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% mutate(sparse=factor(sparse, levels=c("no","yes","pooled")), sample_cat_sparse=paste0(sample_cat,"-",sparse)) %>%
    arrange(sample_cat,sparse) %>% mutate(sample_cat_sparse=factor(sample_cat_sparse, levels=c("Any sample-no",
                                                                                               "Any sample-pooled",
                                                                                               "Source water-no",
                                                                                               "Source water-pooled",
                                                                                               "Stored water-no",
                                                                                               "Stored water-pooled",
                                                                                               "Child hand rinse-no",
                                                                                               "Child hand rinse-pooled",
                                                                                               "Mother hand rinse-no",
                                                                                               "Mother hand rinse-pooled",
                                                                                               "Latrine soil-no",
                                                                                               "Latrine soil-pooled",
                                                                                               "House soil-no",
                                                                                               "House soil-pooled",
                                                                                               "Flies-no",
                                                                                               "Flies-pooled",
                                                                                               "Sparse data-yes"))) %>%
    droplevels(.)
  
  unique(mydf$sample_cat_sparse)
  
  
  colours <- c("Any sample-no" = my_colors[1],
               "Any sample-pooled" = my_colors[1],
               "Source water-no" = my_colors[3],
               "Source water-pooled" = my_colors[3],
               "Stored water-no"  = my_colors[4],
               "Stored water-pooled"  = my_colors[4],
               "Child hand rinse-no"  = my_colors[7],
               "Child hand rinse-pooled"  = my_colors[7],
               "Mother hand rinse-no" = my_colors[8],
               "Mother hand rinse-pooled" = my_colors[8],
               "Latrine soil-no" = my_colors[5],
               "Latrine soil-pooled" = my_colors[5],
               "House soil-no" = my_colors[6],
               "House soil-pooled" = my_colors[6],
               "Flies-no" = my_colors[9],
               "Flies-pooled" = my_colors[9],
               "Sparse data-yes" = "grey50")
  
  shapes <- c("Any sample-no" = 16,
               "Any sample-pooled" = 18,
               "Source water-no" = 16,
               "Source water-pooled" = 18,
               "Stored water-no"  = 16,
               "Stored water-pooled"  = 18,
               "Child hand rinse-no"  = 16,
               "Child hand rinse-pooled" = 18,
               "Mother hand rinse-no" = 16,
               "Mother hand rinse-pooled" = 18,
               "Latrine soil-no" = 16,
               "Latrine soil-pooled" = 18,
               "House soil-no" = 16,
               "House soil-pooled" = 18,
               "Flies-no" = 16,
               "Flies-pooled" = 18,
               "Sparse data-yes" = 13)

  
  axis_range <- range(Y_range)
  axis_range[1] <- axis_range[1]/1.1
  axis_range[2] <- axis_range[2]*1.1
  
  sample_labels <- c("Any sample", "Any sample",
               "Source water", "Source water",
               "Stored water",  "Stored water",
               "Child hand rinse",  "Child hand rinse",
               "Mother hand rinse", "Mother hand rinse",
               "Latrine soil", "Latrine soil",
               "House soil", "House soil",
               "Flies", "Flies",
               "Sparse data" )

  
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat_sparse, color=sample_cat_sparse, shape=sample_cat_sparse))) + 
  geom_point(size=2, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.2, size = 0.75) +
    # scale_color_manual(breaks = legend_labels, guide = guide_legend(),
    #   values = colours, drop = FALSE) +
    # scale_shape_manual(values=shapes, 
    #                    guide = guide_legend(),
    #                    #guide="none", 
    #                    drop = FALSE) + 
    scale_colour_manual(name = "Sample type",
                        labels =sample_labels,
                        values = colours, drop = FALSE) +   
    scale_shape_manual(name = "Sample type",
                       labels = sample_labels,
                       values = shapes, drop = FALSE) +
    geom_text(aes(y=RR, label=est), color="black", vjust = -0.8, hjust = -0.1, size=1.5) +
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_type,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = lab_width, multi_line = TRUE)) +
    scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
      breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
                       trans='log10', 
                       labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
                       ) + coord_flip(ylim=axis_range)+
    # guides(color=guide_legend(title="Sample type", nrow=2,byrow=TRUE), 
    #        shape=guide_legend(title="Sample type", nrow=2,byrow=TRUE)
    #        ) +
    xlab("") + ylab("Prevalence ratio (Intervention vs. control)") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
}


#---------------------------------------------------------------
# Primary figure
#---------------------------------------------------------------

#Primary figure
p_adj_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any bacteria", "Any protozoa", "Any STH", "Any virus")) %>%
  base_plot(drop_full_sparse=T, Y_range=c(0.25,4))
p_adj_1


ggsave(filename=here("figures/aim1-fig-2-temp.pdf"),
       plot = p_adj_1,device='pdf',width=7.2,height=8)

