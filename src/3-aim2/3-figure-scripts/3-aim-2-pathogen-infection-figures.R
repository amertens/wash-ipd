
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/pathogen_specific_aim2_res_unadj.Rds")) 
adj_RR <- readRDS(file=here("results/pathogen_specific_aim2_res.Rds")) %>% arrange(minN)

#adj_RR_old <- readRDS(file="C:/Users/andre/Downloads/pathogen_specific_aim2_res.Rds") 

#TEMP!
adj_RR <- adj_RR %>% filter(RR<128 & RR > 1/128) %>% filter(n!=0, RR!=1)
unadj_RR <- unadj_RR %>% filter(RR<128 & RR > 1/128) %>% filter(n!=0, RR!=1)

adj_RR <- adj_RR %>% filter(minN>=5)
unadj_RR <- unadj_RR %>% filter(minN>=5)


# adj_RR <- adj_RR %>% filter(minN>=10)
# unadj_RR <- unadj_RR %>% filter(minN<10)
# adj_RR <- bind_rows(adj_RR, unadj_RR)
#adj_RR <- unadj_RR


#adj_RR <- adj_RR %>% filter(n>1)
#adj_RR <- adj_RR %>% filter(minN>=5)                                                           
                                                            
adj_RR$N_W <- str_count(adj_RR$W,",") + 1
adj_RR$N_W[adj_RR$W=="unadjusted"] <- 0

adj_RR %>% filter(target=="Ascaris", sample=="S")
adj_RR %>% filter(target=="Trichuris", sample=="S")
temp<-adj_RR %>% filter(target=="Trichuris")
unique(adj_RR$Y)

#Drop non-QPCR sth outcomes from plot to avoid overplotting
#adj_RR <- adj_RR %>% filter(!grepl("qpcr",Y))
adj_RR <- adj_RR %>% filter(!(study=="Kwong 2021" & (Y=="ch_pos_ascaris" | Y=="ch_pos_trichuris" | Y=="ch_pos_trichuris_EE" | Y=="ch_pos_ascaris_EE")))
adj_RR <- adj_RR %>% filter(sample!="any sample type")

# # #drop sparse
#adj_RR <- adj_RR %>% filter(n>=10, minN>=1)

unique(adj_RR$study)
adj_RR <- adj_RR %>% mutate(study=factor(study, levels = rev(c("Steinbaum 2019","Fuhrmeister 2020","Kwong 2021","Capone 2021","Capone 2022 in prep","Boehm 2016"))))

#---------------------------------------------------------------
# Clean results
#---------------------------------------------------------------

adj_RR$sparse <- "no"
adj_RR <- clean_res(adj_RR)

adj_RR <- adj_RR %>% mutate(
  sig_cat = case_when(
    pval>=0.05 ~"",
    pval<0.05 ~"*",
    pval<0.01 ~"**",
    pval<0.001 ~"***"
  )
)

sample_cats = levels(adj_RR$sample_cat)[levels(adj_RR$sample_cat)!="Any sample"]

adj_RR <- adj_RR %>% droplevels()
unique(adj_RR$target_f)
adj_RR <- adj_RR %>% mutate(target_f=factor(target_f, levels=c("Ascaris","Trichuris","Giardia","C. difficile","Campylo-\nbacter","Pathogenic\nE. coli","Shigella","Norovirus","Salmonella","Rotavirus")))


#---------------------------------------------------------------
#plot function
#---------------------------------------------------------------


mydf <- adj_RR 
legend_labels=sample_cats
drop_full_sparse=F

pathogen_plot <- function(mydf, legend_labels=sample_cats, drop_full_sparse=F){
  
  my_colors = c("grey20",carto_pal(12, "Prism"))
  
  colours <- c("Any sample" = my_colors[1],
               "Source water" = my_colors[3],
               "Stored water"  = my_colors[4],
               "Child hand rinse"  = my_colors[7],
               "Mother hand rinse" = my_colors[8],
               "Latrine soil" = my_colors[5],
               "House soil" = my_colors[6],
               "Flies" = my_colors[9],
               "Sparse data" = "grey50")
  
  if(drop_full_sparse){
    mydf <- mydf %>% group_by(target) %>%
      filter(n()!=sum(sparse=="yes")) %>% ungroup()
  }
  
  mydf <- mydf %>% droplevels(.)
  ggplot(data = mydf, (aes(x=study, y=RR, group=sample_cat, color=sample_cat, shape=factor(sparse, levels=c("no","yes","pooled"))))) + 
  geom_point(size=3, position = position_dodge(1), alpha=0.75) +
    geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(1),
                  width = 0.3, size = 1) +
    scale_color_manual(breaks = legend_labels,
      values = colours, drop = FALSE) +
    geom_text(aes(y=RR, label=sig_cat), color="black", position = position_dodge(0.5), hjust = -0.5, vjust=-0.05, size=4) +
    scale_shape_manual(values=c(16, 13,18), guide="none") + 
    geom_hline(yintercept = 1, linetype="dashed") +
    facet_wrap(~target_f,  nrow=2, scales="free") +
        scale_y_continuous(
          breaks=c(0.25, .5,1, 2, 4, 8),
          trans='log10',
          labels = c( "1/4","1/2","1", "2", "4", "8")
        ) + coord_flip(ylim=c(0.35,8)) +
    labs(color="Sample type") + xlab("") + ylab("Prevalence ratio") + 
    theme_ki() + 
    theme(axis.ticks.x=element_blank(),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.text.x = element_text(size=10, face = "bold"),
          strip.text.y = element_text(size=10, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
          panel.spacing = unit(0, "lines")) 
}


#---------------------------------------------------------------
# Plot figures
#---------------------------------------------------------------
  
#Primary figure
max(adj_RR$ci.ub)
min(adj_RR$ci.lb)

adj_RR
p_pathogen <- adj_RR %>% 
  pathogen_plot(drop_full_sparse=T)
p_pathogen

ggsave(p_pathogen, file = paste0(here::here(),"/figures/pngs/aim2_p_pathogen.png"), width = 10, height = 6)


#save figures
save(list=ls(pattern="p_"), file=here("figures/aim2_pathogen_figures.Rdata"))
ls(pattern="p_")


