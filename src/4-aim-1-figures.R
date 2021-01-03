
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim1_RR.Rds"))
unadj_RD <- readRDS(file=here("results/unadjusted_aim1_RD.Rds"))
unadj_diff <- readRDS(file=here("results/unadjusted_aim1_diff.Rds"))
adj_RR <- readRDS(file=here("results/adjusted_aim1_RR.Rds"))
adj_RD <- readRDS(file=here("results/adjusted_aim1_RD.Rds"))
adj_diff <- readRDS(file=here("results/adjusted_aim1_diff.Rds"))

unadj_RR %>% distinct(study, target, type) %>% as.data.frame()

#function to clean results/order factors
clean_res <- function(d){
  d$target <- factor(d$target, levels =c(
     "any_entero",   "any_human_MST",    "any_animal_MST",
     "EC23S",         "HF183",        
     "Mnif",       "ECVG",          "Hum",          
     "BC",         "av",            "br",    "sth",                     
     "ascaris",       "trichuris" 
  ))
  return(d)
}

unadj_RR <- clean_res(unadj_RR)
unadj_RD <- clean_res(unadj_RD)
unadj_diff <- clean_res(unadj_diff)
adj_RR <- clean_res(adj_RR)
adj_RD <- clean_res(adj_RD)
adj_diff <- clean_res(adj_diff)


#Primary figure
p1 <- unadj_RR %>% 
  filter(aggregate_Y==1, target!="sth") %>%
  droplevels(.) %>%
  mutate(study=paste0(study,"-",round)) %>%
  ggplot(., (aes(x=study, y=RR))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_grid(target~type,  scales="free") +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip()+
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) + theme_ki()
p1


