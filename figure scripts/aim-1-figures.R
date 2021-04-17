
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim1_RR.Rds")) %>% filter(!is.na(coef))
unadj_RD <- readRDS(file=here("results/unadjusted_aim1_RD.Rds")) %>% filter(!is.na(coef))
unadj_diff <- readRDS(file=here("results/unadjusted_aim1_diff.Rds")) %>% filter(!is.na(coef)) 
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR.Rds")) %>% filter(!is.na(coef))
adj_RD <- readRDS(file=here("results/adjusted_aim1_RD.Rds")) %>% filter(!is.na(coef))
adj_diff <- readRDS(file=here("results/adjusted_aim1_diff.Rds")) %>% filter(!is.na(coef)) 

unadj_RR %>% distinct(study, target, sample) %>% as.data.frame()

unadj_RR %>% filter(study=="WBB", target=="Any STH")

#---------------------------------------------------------------
#function to clean results/order factors
#---------------------------------------------------------------

unique(unadj_RD$target)
unique(unadj_RD$sample)

d <- unadj_RR
target_lev=target_levels

clean_res <- function(d, target_lev=target_levels){
  
  target_lev <- gsub("Any STH ","Any Helminth",target_lev)
  d$target_f <- gsub("Any STH ","Any Helminth",d$target)  
  target_lev <- gsub("Any ","Any\n",target_lev)
  d$target_f <- gsub("Any ","Any\n",d$target)
  target_lev <- gsub("Entamoeba histolytica","Entamoeba\nhistolytica",target_lev)
  d$target_f <- gsub("Entamoeba histolytica","Entamoeba\nhistolytica",d$target_f)
  target_lev <- gsub("Pathogenic E. coli","Pathogenic\nE. coli",target_lev)
  d$target_f <- gsub("Pathogenic E. coli","Pathogenic\nE. coli",d$target_f)
  
  d$target_f <- factor(d$target_f, levels = c(target_lev, unique(d$target_f)[!(unique(d$target_f) %in% target_lev)]) ) 

  d <- d %>% mutate(
    sample_type =case_when(
      sample == "any sample type" ~ "Any sample\ntype",
      sample == "SW" ~ "Water",
      sample == "W" ~ "Water",
      sample == "CH" ~ "Hands",
      sample == "MH" ~ "Hands",
      sample == "LS" ~ "Soil",
      sample == "S" ~ "Soil"
    ),
    sample_type = factor(sample_type, levels=c("Any sample\ntype", "Water", "Hands","Soil")),
    sample_cat =case_when(
      sample == "any sample type" ~ "Any sample",
      sample == "SW" ~ "Source water",
      sample == "W" ~ "Stored water",
      sample == "CH" ~ "Child hands",
      sample == "MH" ~ "Mother's hands",
      sample == "LS" ~ "Latrine soil",
      sample == "S" ~ "House soil"
    ), 
    sample_cat = factor(sample_cat, 
        levels=c("Any sample","Source water","Stored water",
                 "Child hands", "Mother's hands", "Latrine soil","House soil"))
    )
  
  return(d)
}



#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

unadj_RR <- clean_res(unadj_RR)
unadj_RD <- clean_res(unadj_RD)
unadj_diff <- clean_res(unadj_diff)
adj_RR <- clean_res(adj_RR)
adj_RD <- clean_res(adj_RD)
adj_diff <- clean_res(adj_diff)


#see if any levels are missing
unadj_RR$target[is.na(unadj_RR$target_f)]
 
sample_cats = levels(unadj_RR$sample_cat)[levels(unadj_RR$sample_cat)!="Any sample"]


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------
  
mydf <- unadj_RR %>%
  filter(target %in% c("Any human MST","Any animal MST","Any pathogen","Any general MST"))
legend_labels=sample_cats

base_plot <- function(mydf, legend_labels=sample_cats){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hands"  = my_colors[7],
               "Mother's hands" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6])
  
  mydf <- mydf %>% droplevels(.)
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat))) + 
  geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = legend_labels,
      values = colours, drop = FALSE) +
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_type,  scales="free_y", space = "free_x") +
    scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN)+ coord_flip()+
    labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=11, face = "bold"),
          strip.text.y = element_text(size=11, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
  
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
  
#Primary figure
p_1 <- unadj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any pathogen","Any general MST")) %>%
  base_plot
p_1


p_s1 <- unadj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus")) %>%
  base_plot
p_s1


#-	Fig S2. Prevalence of pathogen with human hosts, and pathogen with human/animal hosts 
   #(same as fig 3)
# -	Fig S3. Prevalence of specific pathogens 
unique(unadj_RR$target_f)
p_s3 <- unadj_RR %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved"))) %>%
  base_plot
p_s3



# -	Fig S4. Prevalence of specific MST markers 
p_s4 <- unadj_RR %>% 
  filter(target %in% any_MST) %>%
  base_plot
p_s4

# -	Fig S5. Abundance of specific pathogens
p_s5 <- unadj_diff %>% 
  filter(target %in% any_pathogens) %>%
  droplevels(.) %>%
  #mutate(study=paste0(study,"-",round)) %>%
  ggplot(., (aes(x=study, y=RR))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 1, linesample="dashed") +
  facet_wrap(target_f~sample,  scales="free") +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  ylab("IRR") +
  coord_flip()+
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) + theme_ki()
p_s5

# -	Fig S6. Abundance of specific MST markers 
p_s6 <- unadj_diff %>% 
  filter(target %in% any_MST) %>%
  droplevels(.) %>%
  #mutate(study=paste0(study,"-",round)) %>%
  ggplot(., (aes(x=study, y=RR))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 1, linesample="dashed") +
  facet_grid(target_f~sample,  scales="free") +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  ylab("IRR") +
  coord_flip()+
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) + theme_ki()
p_s6

# -	Fig S7. Repeat of Fig 1, adjusted  
p_s7 <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any pathogen","Any general MST")) %>%
  base_plot
p_s7

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


