
rm(list=ls())
source(here::here("0-config.R"))
library(scales)


adj_RD <- readRDS(file=here("results/adjusted_aim1_emm_pooled_RD.Rds")) 

adj_zoo <- readRDS(file=here("results/adjusted_zoonotic_animals.Rds")) %>% filter(!is.na(coef))


Ns <- adj_RD %>% group_by(study, sample, target, aggregate_Y, V) %>% summarise(N=n())
table(Ns$N)
Ns[Ns$N>2,]
Ns[Ns$N==1,]
adj_RD

adj_RD$PD <- adj_RD$coef

adj_RD$PD[is.na(adj_RD$PD)] <- NA
target_lev=target_levels


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------
#adj_RD <- adj_RD %>% filter(!is.na(coef))
adj_RD$sparse <- ifelse(is.na(adj_RD$coef),"yes","no")
adj_RD$PD[adj_RD$sparse=="yes"] <- 1
adj_RD <- clean_res_subgroup(adj_RD)
adj_RD <- adj_RD %>% mutate(
  int.p =case_when(
    Vlevel==0 ~ NA_character_,
    int.p<0.001~"***",
    int.p<0.01~"**",
    int.p<0.05~"*",
    int.p>=0.05~""
  ),
  Vlevel = factor(case_when(
    sparse=="yes" ~ "Sparse data",
    V=="wet" & Vlevel==0 ~ "Dry season",
    V=="wet" & Vlevel==1 ~ "Wet season",
    V=="animals" & Vlevel==0 ~ "No animals",
    V=="animals" & Vlevel==1 ~ "Animals in\ncompound"
  ), levels = c("Dry season", "Wet season","No animals", "Animals in\ncompound","Sparse data"))
)

table(adj_RD$int.p)
table(adj_RD$Vlevel, adj_RD$int.p)
table(adj_RD$Vlevel)

#see if any levels are missing
adj_RD$target[is.na(adj_RD$target_f)]
sample_cats = levels(adj_RD$sample_cat)[levels(adj_RD$sample_cat)!="Any sample"]

#Clean results
adj_zoo <- clean_res(adj_zoo)



#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------
  
mydf <- adj_RD %>% 
  filter(target %in% c("Any pathogen","Any MST"), V=="wet")
drop_full_sparse=T
legend_labels=sample_cats
ylimits=c(-1,1)

mydf <- adj_zoo 
drop_full_sparse=F


base_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F, ylimits=c(-1,1), Nbreaks=3){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies in kitchen" = my_colors[9],
               "Flies in latrine" = my_colors[10]#,
               #"Sparse data" = "grey50"
                 )
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
    mydf <- mydf %>% group_by(sample) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% filter(!is.na(coef)) %>% droplevels(.)
  #Y_breaks=c(-1,-0.8, -0.6 -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  Y_breaks=c( -0.3, 0, 0.3)
  # Y_breaks2=c("1/4", "1/2","1", "2", "4", "8")
  
  ggplot(data = mydf, (aes(x=study, y=coef, group=Vlevel, color=Vlevel, shape=Vlevel))) + 
  geom_point(size=2, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5),
                  width = 0.3, size = 1) +
    #Mark significant interactions
    geom_text(aes(label=int.p), color="black", position = position_dodge(0.5), hjust = -0.01, vjust = -0.05, size=4) + 
    scale_color_manual(#breaks = legend_labels,
      values = c(cbbPalette[2:3],"grey50"), drop = FALSE) +
    scale_shape_manual(values=c(16, 16,16), guide=FALSE) +  
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_grid(target_f~sample_cat,  scales="free_y", space = "free_x", labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
     scale_y_continuous(#breaks=scales::breaks_pretty(c(-1, -0.5, 0, 0.5, 1)),
       #breaks=pretty_breaks(n=Nbreaks)
       breaks=Y_breaks#,
       #labels = Y_breaks2
       ) + 
    coord_flip(ylim=ylimits) +
    labs(color="Subgroup") + xlab("") + ylab("Prevalence difference") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines")) 
  
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
p_wet_1_PD <- adj_RD %>% 
  filter(target %in% c("Any pathogen","Any MST"), V=="wet") %>%
  base_plot(drop_full_sparse=T,  ylimits=c(-0.55,0.4))
p_wet_1_PD

p_animals_1_PD <- adj_RD %>% 
  filter(target %in% c("Any pathogen","Any MST"), V=="animals") %>%
  base_plot(drop_full_sparse=T,  ylimits=c(-0.5,0.5))
p_animals_1_PD

p_wet_2_PD <- adj_RD %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), V=="wet") %>%
  base_plot

p_animals_2_PD <- adj_RD %>% 
  filter(target %in% c("Any human MST","Any animal MST","Any general MST"), V=="animals") %>%
  base_plot

p_wet_3_PD <- adj_RD %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), V=="wet") %>%
  base_plot

p_animals_3_PD <- adj_RD %>% 
  filter(target %in% c("Any bacteria", "Any protozoa", "Any STH", "Any virus"), V=="animals") %>%
  base_plot



#-	Fig S2. Prevalence of pathogen with human hosts, and pathogen with human/animal hosts 
   #(same as fig 3)
# -	Fig S3. Prevalence of specific pathogens 
p_wet_s1_PD <- adj_RD %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved")), V=="wet") %>%
  base_plot(drop_full_sparse=T)

p_animals_s1_PD <- adj_RD %>% 
  filter(target %in% any_pathogens, !c(target %in% c("Any STH","any pathogen-improved","any pathogen-unimproved")), V=="animals") %>%
  base_plot(drop_full_sparse=T)



# -	Fig S4. Prevalence of specific MST markers 
p_wet_s2_PD <- adj_RD %>% 
  filter(target %in% any_MST, V=="wet") %>%
  base_plot

p_animals_s2_PD <- adj_RD %>% 
  filter(target %in% any_MST, V=="animals") %>%
  base_plot



adj_zoo <- adj_zoo %>%
  mutate(Vlevel = factor(Vlevel)) 
adj_zoo <- clean_res_subgroup(adj_zoo)

adj_zoo$target_f
adj_zoo$sample_cat

adj_zoo <- adj_zoo %>% mutate(
  int.p =case_when(
    Vlevel==0 ~ NA_character_,
    int.p<0.001~"***",
    int.p<0.01~"**",
    int.p<0.05~"*",
    int.p>=0.05~""
  ),
  Vlevel = factor(case_when(
    sparse=="yes" ~ "Sparse data",
    Vlevel==0 ~ "Non-zoonotic",
    Vlevel==1 ~ "Zoonotic",
  ), levels = c("Non-zoonotic", "Zoonotic","Sparse data"))
)

unique(adj_zoo$study)
#adj_zoo$study <- factor(adj_zoo$study, levels = rev(c("Steinbaum 2019","Fuhrmeister 2020", "Kwong 2021", "Pooled")))

p_zoo_PD <- adj_zoo %>%
  # filter(target %in% c("Any zoonotic","Any non-zoonotic"),
  #        sample_cat!="Sparse data") %>%
  # group_by(study) %>% 
  # mutate(Vlevel=target, target_f="Any pathogen", int.p="") %>%
  base_plot(., drop_full_sparse = F,  ylimits=c(-0.75,0.25))
p_zoo_PD



#save figures
save(list=ls(pattern="p_"), file=here("figures/subgroup_figures_PD.Rdata"))



