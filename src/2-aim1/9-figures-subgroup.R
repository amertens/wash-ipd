
rm(list=ls())
source(here::here("0-config.R"))
library(scales)

adj_RR <- readRDS(file=here("results/adjusted_aim1_emm.Rds")) 
target_lev=target_levels

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------
adj_RR <- adj_RR %>% filter(!is.na(coef))
adj_RR$sparse <- "no"
adj_RR <- clean_res(adj_RR)
adj_RR <- adj_RR %>% mutate(
  int.p =case_when(
    Vlevel==0 ~ NA_character_,
    int.p<0.001~"***",
    int.p<0.01~"**",
    int.p<0.05~"*",
    int.p>=0.05~""
  ),
  Vlevel = factor(case_when(
    V=="wet" & Vlevel==0 ~ "Dry season",
    V=="wet" & Vlevel==1 ~ "Wet season",
    V=="animals" & Vlevel==0 ~ "No animals",
    V=="animals" & Vlevel==1 ~ "Animals in\ncompound"
  ))
)

table(adj_RR$int.p)
table(adj_RR$Vlevel, adj_RR$int.p)

#see if any levels are missing
adj_RR$target[is.na(adj_RR$target_f)]
sample_cats = levels(adj_RR$sample_cat)[levels(adj_RR$sample_cat)!="Any sample"]


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------
  
mydf <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), V=="wet")
drop_full_sparse=T
legend_labels=sample_cats


base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, ylimits=c(0.25,5)){
  
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
  
  Y_breaks=c(.25, .5,1, 2, 4, 8)
  Y_breaks2=c("1/4", "1/2","1", "2", "4", "8")
  
  ggplot(data = mydf, (aes(x=study, y=RR, group=Vlevel, color=Vlevel))) + 
  geom_point(size=3, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    #Mark significant interactions
    geom_text(aes(label=int.p), color="black") + 
    scale_color_manual(#breaks = legend_labels,
      values = cbbPalette[2:3], drop = FALSE) +
    #scale_shape_manual(values=c(16, 13,18), guide=FALSE) + 
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
    scale_y_continuous(#breaks=scales::breaks_pretty(c(0.25, 0.5,1, 2, 4, 8)),
      breaks=Y_breaks, 
                       trans='log10', 
                       labels = Y_breaks2
                       ) + 
    coord_flip(ylim=ylimits)+
    labs(color="Subgroup") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
  
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
p_wet_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), V=="wet") %>%
  base_plot(drop_full_sparse=F, ylimits=c(0.5,3))
p_wet_1

p_animals_1 <- adj_RR %>% 
  filter(target %in% c("Any pathogen","Any MST"), V=="animals") %>%
  base_plot(drop_full_sparse=T, ylimits=c(0.25,5))


p_wet_2 <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), V=="wet") %>%
  base_plot

p_animals_2 <- adj_RR %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), V=="animals") %>%
  base_plot

p_wet_3 <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), V=="wet") %>%
  base_plot

p_animals_3 <- adj_RR %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), V=="animals") %>%
  base_plot



#-	Fig S2. Prevalence of pathogen with human hosts, and pathogen with human/animal hosts 
   #(same as fig 3)
# -	Fig S3. Prevalence of specific pathogens 
p_wet_s1 <- adj_RR %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved")), V=="wet") %>%
  base_plot(drop_full_sparse=T)

p_animals_s1 <- adj_RR %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved")), V=="animals") %>%
  base_plot(drop_full_sparse=T)



# -	Fig S4. Prevalence of specific MST markers 
p_wet_s2 <- adj_RR %>% 
  filter(target %in% any_MST, V=="wet") %>%
  base_plot

p_animals_s2 <- adj_RR %>% 
  filter(target %in% any_MST, V=="animals") %>%
  base_plot




#save figures
save(list=ls(pattern="p_"), file=here("figures/subgroup_figures.Rdata"))



