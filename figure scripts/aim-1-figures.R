
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim1_RR.Rds")) %>% filter(!is.na(coef))
unadj_RD <- readRDS(file=here("results/unadjusted_aim1_RD.Rds")) %>% filter(!is.na(coef))
unadj_diff <- readRDS(file=here("results/unadjusted_aim1_diff.Rds")) %>% filter(!is.na(coef)) 
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR.Rds")) %>% filter(!is.na(coef))
adj_RD <- readRDS(file=here("results/adjusted_aim1_RD.Rds")) %>% filter(!is.na(coef))
adj_diff <- readRDS(file=here("results/adjusted_aim1_diff.Rds")) %>% filter(!is.na(coef)) 

unadj_RR %>% distinct(study, target, sample) %>% as.data.frame()

#---------------------------------------------------------------
#function to clean results/order factors
#---------------------------------------------------------------

unique(unadj_RD$target)
unique(unadj_RD$sample)
clean_res <- function(d){
  d$target_f <- factor(d$target, levels =c(
    "Any general MST",       "Any human MST",        "Any animal MST",  
    "Any pathogen",    "Any bacteria",                       
    "Any virus",       "Any STH", "Any protozoa",
    "Human (Bacteroides)",  
    "Human (M. smithii)",                       
    "Ascaris",               "BacCow",               
    "E. coli virulence gene","Giardia",               "HumM2",                
    "Norovirus",             "Trichuris",             "Avian",                
    "Rotavirus",             "Ruminant"   
  ))
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
    sample_cat =case_when(
      sample == "any sample type" ~ "",
      sample == "SW" ~ "Source water",
      sample == "W" ~ "Household water",
      sample == "CH" ~ "Child hands",
      sample == "MH" ~ "Mother's hands",
      sample == "LS" ~ "",
      sample == "S" ~ ""
    )
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
 
#---------------------------------------------------------------
# Outcome groups:
#---------------------------------------------------------------

  #pathogens:
  any_pathogens = c("E. coli virulence gene","Giardia",  "C. difficile",
                    "Shigella",  "Entamoeba histolytica",  "V. cholerae", "Yersinia",       
                    "Norovirus",     "Ascaris",
                    "Adenovirus","Trichuris",  "Rotavirus", "Astrovirus", "Cryptosporidium", "Salmonella")   
  
  any_virus = c("Norovirus",  "Adenovirus", "Rotavirus", "Astrovirus")   
  any_bacteria = c("E. coli virulence gene", "Yersinia",  "V. cholerae", "Shigella",  "C. difficile",  "Salmonella")   
  #any_helminth = c("Any STH", "Ascaris", "Trichuris")   
  any_protozoa = c("Giardia", "Cryptosporidium", "Entamoeba histolytica")   
  
  #MST's:
  general_MST = c("GenBac3")
  
  animal_MST = c( "BacCow",   
                  "Ruminant",              "Avian",
                  "Avian (Helicobacter)")
  
  human_MST = c("HumM2",  "Human (Bacteroides)",   "Human (M. smithii)")
  any_MST = c(general_MST, animal_MST, human_MST)



#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------
  
base_plot <- function(mydf) {
  mydf <- mydf %>% droplevels(.)
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat))) + 
  geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = c("Source water", "Household water", "Child hands", "Mother's hands"),
                       values = tableau11) +
    geom_hline(yintercept = 1, linesample="dashed") +
    facet_grid(target_f~sample_type,  scales="free") +
    scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN)+ coord_flip()+
    labs(color="Sample type") +
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
  
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
  
#Primary figure
p_1 <- unadj_RR %>% 
  filter(target_f %in% c("Any human MST","Any animal MST","Any pathogen","Any general MST")) %>%
  base_plot
p_1


p_s1 <- unadj_RR %>% 
  filter(target_f %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus")) %>%
  base_plot
p_s1


#-	Fig S2. Prevalence of pathogen with human hosts, and pathogen with human/animal hosts 
   #(same as fig 3)
# -	Fig S3. Prevalence of specific pathogens 
unique(unadj_RR$target_f)
p_s3 <- unadj_RR %>% 
  filter(target_f %in% any_pathogens) %>%
  base_plot
p_s3



# -	Fig S4. Prevalence of specific MST markers 
p_s4 <- unadj_RR %>% 
  filter(target_f %in% any_MST) %>%
  base_plot
p_s4

# -	Fig S5. Abundance of specific pathogens
p_s5 <- unadj_diff %>% 
  filter(target_f %in% any_pathogens) %>%
  droplevels(.) %>%
  #mutate(study=paste0(study,"-",round)) %>%
  ggplot(., (aes(x=study, y=RR))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 1, linesample="dashed") +
  facet_wrap(target_f~sample,  scales="free") +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  ylab("Count difference") +
  coord_flip()+
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) + theme_ki()
p_s5

# -	Fig S6. Abundance of specific MST markers 
p_s6 <- unadj_diff %>% 
  filter(target_f %in% any_MST) %>%
  droplevels(.) %>%
  #mutate(study=paste0(study,"-",round)) %>%
  ggplot(., (aes(x=study, y=RR))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 1, linesample="dashed") +
  facet_grid(target_f~sample,  scales="free") +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  ylab("Count difference") +
  coord_flip()+
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) + theme_ki()
p_s6

# -	Fig S7. Repeat of Fig 1, adjusted  
p_s7 <- adj_RR %>% 
  filter(target_f %in% c("Any human MST","Any animal MST","Any pathogen","Any general MST")) %>%
  base_plot
p_s7

# -	Fig S8-S10. Repeat of Fig 1, broken down by rural/urban, season, animal ownership 
#rural/urban, 
#season, 
#animal ownership 


#save figures
save(list=ls(pattern="p_"), file=here("figures/all_figures.Rdata"))



