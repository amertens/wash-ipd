
rm(list=ls())
source(here::here("0-config.R"))
library(scales)

unadj_RR <- readRDS(file=here("results/unadjusted_aim1_RR_pooled.Rds")) 
unadj_RD <- readRDS(file=here("results/unadjusted_aim1_RD.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR_pooled.Rds")) 
adj_RD <- readRDS(file=here("results/adjusted_aim1_RD.Rds")) 

unadj_RR %>% distinct(study, target, sample) %>% as.data.frame()

#---------------------------------------------------------------
#function to clean results/order factors
#---------------------------------------------------------------

unique(unadj_RD$target)
unique(unadj_RD$sample)

d <- unadj_RR
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


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------
  
mydf <- unadj_RR %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) 
drop_full_sparse=T
legend_labels=sample_cats


base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F,
                      # Y_breaks=c(.25, .5,1, 2, 4, 8),
                      # Y_labs=c("1/4", "1/2","1", "2", "4", "8")){
                      Y_range=c(.25, 4)){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hands"  = my_colors[7],
               "Mother's hands" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies in kitchen" = my_colors[9],
               "Flies in latrine" = my_colors[10],
               "Sparse data" = "grey50")
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% droplevels(.)
  
  axis_range <- range(Y_range)
  axis_range[1] <- axis_range[1]/1.1
  axis_range[2] <- axis_range[2]*1.1
  
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_point(size=2, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 0.75) +
    scale_color_manual(breaks = legend_labels,
      values = colours, drop = FALSE) +
    scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_type,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
      breaks=c(0.0625, 0.125,.25, .5,1, 2, 4, 8, 16), 
                       trans='log10', 
                       labels = c("1/16","1/8","1/4", "1/2","1", "2", "4", "8", "16")
                       ) + coord_flip(ylim=axis_range)+
    labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
  
d <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST")) %>% filter(study=="Holcomb 2020")

#Primary figure
p_adj_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST")) %>%
  base_plot(drop_full_sparse=T, Y_range=c(0.25,4))
p_adj_1

p_adj_2 <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST")) %>%
  base_plot
p_adj_2


p_adj_3 <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus")) %>%
  base_plot
p_adj_3


#-	Fig S2. Prevalence of pathogen with human hosts, and pathogen with human/animal hosts 
   #(same as fig 3)
# -	Fig S3. Prevalence of specific pathogens 
unique(adj_RR$target_f)
p_adj_s1 <- adj_RR %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  base_plot(drop_full_sparse=T,
            Y_range=c(0.125,8))
p_adj_s1



# -	Fig S4. Prevalence of specific MST markers 
p_adj_s2 <- adj_RR %>% 
  filter(target %in% any_MST) %>%
  base_plot
p_adj_s2


#Primary figure
p_unadj_1 <- unadj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST")) %>%
  base_plot(drop_full_sparse=T)
p_unadj_1

p_unadj_2 <- unadj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST")) %>%
  base_plot
p_unadj_2

p_unadj_3 <- unadj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus")) %>%
  base_plot
p_unadj_3


#-	Fig S2. Prevalence of pathogen with human hosts, and pathogen with human/animal hosts 
#(same as fig 3)
# -	Fig S3. Prevalence of specific pathogens 
unique(unadj_RR$target_f)
p_unadj_s1 <- unadj_RR %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  base_plot(drop_full_sparse=T, Y_range=c(0.125,8))
p_unadj_s1



# -	Fig S4. Prevalence of specific MST markers 
p_unadj_s2 <- unadj_RR %>% 
  filter(target %in% any_MST) %>%
  base_plot
p_unadj_s2





# -	Fig S8-S10. Repeat of Fig 1, broken down by rural/urban, season, animal ownership 
#rural/urban, 
#season, 
#animal ownership 




#save figures
save(list=ls(pattern="p_"), file=here("figures/all_figures.Rdata"))

#save groups
save(any_pathogens, 
     any_virus,  
     any_bacteria, 
     any_protozoa, 
     general_MST, 
     animal_MST, 
     human_MST, 
     any_MST , file=here("figures/outcome_groups.Rdata"))

