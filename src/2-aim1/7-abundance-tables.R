

rm(list=ls())
source(here::here("0-config.R"))
library(table1)
library(rvest)



d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
unadj_diff <- readRDS(file=here("results/unadjusted_aim1_diff.Rds")) %>% mutate(sparse="no")
adj_diff <- readRDS(file=here("results/adjusted_aim1_diff.Rds")) %>% mutate(sparse="no")


d <- d %>% filter(!is.na(abund)) %>% droplevels(.)

table(d$qual)
table(is.na(d$qual))
table(d$study[is.na(d$qual)])
table(d$target[is.na(d$qual) & d$study=="Fuhrmeister 2020"])
table(d$sample[is.na(d$qual) & d$study=="Holcomb 2021"])

colnames(unadj_diff)
head(unadj_diff)

#Drop low ROQ
unadj_diff <- unadj_diff %>% filter(perc_ROQ  >= 50)
adj_diff <- adj_diff %>% filter(perc_ROQ  >= 50)

clean_tab = function(df){
  df <- df %>% 
    mutate(
      coef=round(coef,2),
      RR=round(RR,2),
      ci.lb=round(ci.lb,2),
      ci.ub=round(ci.ub,2),
      pval=round(pval,2),
      wilcox.p=round(wilcox.p,2),
      model=ifelse(model=="neg. binomial","*",""),
      est=ifelse(model=="",
                 paste0(coef, " (",ci.lb," ",ci.ub,")"),
                 paste0(RR, " (",ci.lb," ",ci.ub,")",model)),
      mean_control=paste0(round(mean_control,2), " (",round(sd_control,2),"), ",round(med.cont,2), " (", round(iqr1.cont,2), "-", round(iqr2.cont,2),")"), 
      mean_int=paste0(round(mean_int,2), " (",round(sd_int,2),"), ",round(med.int,2), " (", round(iqr1.int,2), "-", round(iqr2.int,2),")"), 
      sample =case_when(
        sample == "SW" ~ "Source water",
        sample == "W" ~ "Stored water",
        sample == "CH" ~ "Child hand rinse",
        sample == "MH" ~ "Mother's hand rinse",
        sample == "FlyKitch" ~ "Flies in kitchen",
        sample == "FlyLat" ~ "Flies in latrine",
        sample == "LS" ~ "Latrine soil",
        sample == "S" ~ "House soil"),
      sample = factor(sample, 
                          levels=c("Any sample","Source water","Stored water",
                                   "Child hand rinse", "Mother's hand rinse", "Latrine soil",
                                   "House soil", "Flies in kitchen",  "Flies in latrine", "Sparse data"))) %>%
    subset(., select = c(study, sample,target, N, mean_control, mean_int, 
                           est, pval, wilcox.p, perc_ROQ)) %>% arrange(study, sample, target) %>%
    group_by(study, sample) %>% mutate(sample = ifelse(row_number() == 1, as.character(sample), "-")) %>%
    group_by(study) %>% mutate(study = ifelse(row_number() == 1, as.character(study), "-"))
  
  colnames(df)  <- c("Study",   "Sample", "Target", "N", "Control mean (SD), median (IQR)", "Intervention mean (SD), median (IQR)", "Difference or ratio (95% CI)", "P value","Wilcoxon P value", "% in ROQ")
  return(df)
}

tab_unadj_diff <- clean_tab(unadj_diff)
tab_adj_diff <- clean_tab(adj_diff)

save(tab_unadj_diff, tab_adj_diff, file=here("figures/abundance_tables.Rdata"))



#Old abundance checking tables
tab <- d %>% group_by(study, target, sample, qual) %>%
  droplevels(.) %>%
  summarise(N=n(), med=round(median(abund),1)) %>%
  mutate(val=paste0(N, " (",med,")")) %>%
  select(study, target, sample, qual, val) %>%
  pivot_wider(id_cols= c("study","target","qual"), names_from = sample , values_from = val) %>%
  subset(., select = -c(`any sample type`)) %>%
  as.data.frame()
tab$qual[is.na(tab$qual)] <- "BLOD"

table(d$qual)
d <- d %>% group_by(study, sample, target) %>% mutate(N=n(), Nmiss=sum(is.na(qual))) %>%
  mutate(qual = case_when(
    N==Nmiss & is.na(qual) ~ "ROQ",
    N!=Nmiss & is.na(qual) ~ "BLOD",
    !is.na(qual) ~ qual))
d$qual <- factor(d$qual, levels=c("BLOD", "BLOQ",  "ROQ"))
table(d$qual)
table(paste0(d$study, " ",d$sample," ",d$target), is.na(d$qual))
table(paste0(d$study, " ",d$sample," ",d$target), (d$qual))

head(d)

#d <- d %>% filter(study=="Boehm 2016", sample=="Ascaris", target=="Ascaris")

norm_fun <- function(d){

  #add in number of BLOQ and BLOD
  Nqual <- table(d$qual)
  prop.qual <- round(prop.table(table(d$qual)), 3) * 100

  d <- d %>% mutate(N=n()) %>% filter(qual=="ROQ")
  res <- d %>% summarise(N=N[1],
                         mean=round(mean(abund),1), med=round(median(abund),1),
                         trans.mean=round(mean(log10(abund+0.1)),1), trans.med=round(median(log10(abund+0.1)),1))

  res$raw.p <- res$trans.p <- NA
  try(res$raw.p <- round(shapiro.test(d$abund)$p.value, 5))
  try(res$trans.p <- round(shapiro.test(log10(d$abund))$p.value, 5))

  #res <- data.frame(res, as.data.frame(t(as.matrix(Nqual))), as.data.frame(t(as.matrix(prop.qual))))
  res$ROQ = paste0(prop.qual[3]," (",Nqual[3],")")
  res$BLOQ = paste0(prop.qual[2]," (",Nqual[2],")")
  res$BLOD = paste0(prop.qual[1]," (",Nqual[1],")")

  res <- res %>%
    arrange(N, ROQ, BLOQ, BLOD, mean, med, trans.mean, trans.med, trans.p, raw.p)
  return(res)
}


abund_ROQ <- d %>% filter(!is.na(abund)) %>% droplevels() %>%
  group_by(study, sample, target) %>% do(norm_fun(.))

abund_ROQ
save(abund_ROQ, file=here("figures/abundance_table_ROQ.Rdata"))

write.csv(abund_ROQ, file=here("figures/abundance_table_ROQ.csv"))


#Density figures
df <- d %>% filter(!is.na(abund)) %>%
  filter(sample!="any sample type") %>%
  group_by(study, sample, target) %>%
  mutate(perc_roq=mean(qual=="ROQ"),
         mn=mean(abund),
         md=median(abund)) %>% 
  filter(perc_roq>0.5) %>%
  droplevels() 
head(df)  

table(df$perc_roq)

# p_abund_dist <- ggplot(df, aes(x=abund, fill=study, color=study)) + 
#   geom_density() + 
#   scale_x_continuous(trans = "log10") +
#   geom_vline(aes(xintercept = mn)) +
#   geom_vline(aes(xintercept = md), linetype="dashed") +
#   facet_wrap(study~target, scales = "free", ncol=4) + 
#   xlab("Log-10 transformed abundance") + ylab("Density")
# 
# 
# p_abund_dist
# 
# save(p_abund_dist, file=here("figures/abund_dist_figures.Rdata"))


table(df$study)
p_abund_dist_holcomb <- ggplot( df %>% filter(study=="Holcomb 2021"), 
                                aes(x=abund, fill=study, color=study)) + 
  geom_density(alpha=0.5) + 
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(values=tableau11[1]) +
  scale_color_manual(values=tableau11[1]) +
  geom_vline(aes(xintercept = mn)) +
  geom_vline(aes(xintercept = md), linetype="dashed") +
  facet_wrap(sample~target, scales = "free", ncol=4) + 
  xlab("Abundance") + ylab("Density") +
  ggtitle("Holcomb et al. 2020")

p_abund_dist_fuhrmeister <- ggplot( df %>% filter(study=="Fuhrmeister 2020"), 
                                aes(x=abund, fill=study, color=study)) + 
  geom_density(alpha=0.5) + 
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(values=tableau11[1]) +
  scale_color_manual(values=tableau11[1]) +
  geom_vline(aes(xintercept = mn)) +
  geom_vline(aes(xintercept = md), linetype="dashed") +
  facet_wrap(sample~target, scales = "free", ncol=4) + 
  xlab("Abundance") + ylab("Density") +
  ggtitle("Fuhrmeister et al. 2020")

p_abund_dist_capone <- ggplot( df %>% filter(study=="Capone 2022 in prep"), 
                                aes(x=abund, fill=study, color=study)) + 
  geom_density(alpha=0.5) + 
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(values=tableau11[1]) +
  scale_color_manual(values=tableau11[1]) +
  geom_vline(aes(xintercept = mn)) +
  geom_vline(aes(xintercept = md), linetype="dashed") +
  facet_wrap(sample~target, scales = "free", ncol=4) + 
  xlab("Abundance") + ylab("Density") +
  ggtitle("Capone et al. in prep 2022")


            
             

p_abund_dist_steinbaum<- ggplot( df %>% filter(study=="Steinbaum 2019"), 
                                aes(x=abund, fill=study, color=study)) + 
  geom_density(alpha=0.5) + 
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(values=tableau11[1]) +
  scale_color_manual(values=tableau11[1]) +
  geom_vline(aes(xintercept = mn)) +
  geom_vline(aes(xintercept = md), linetype="dashed") +
  facet_wrap(sample~target, scales = "free", ncol=4) + 
  xlab("STH egg count") + ylab("Density") +
  ggtitle("Steinbaum et al. 2019")

p_abund_dist_kwong <- ggplot( df %>% filter(study=="Kwong 2021"), 
                                 aes(x=abund, fill=study, color=study)) + 
  geom_density(alpha=0.5) + 
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(values=tableau11[1]) +
  scale_color_manual(values=tableau11[1]) +
  geom_vline(aes(xintercept = mn)) +
  geom_vline(aes(xintercept = md), linetype="dashed") +
  facet_wrap(sample~target, scales = "free", ncol=4) + 
  xlab("STH egg count") + ylab("Density") +
  ggtitle("Kwong et al. 2021")



save(list=ls(pattern="p_"), file=here("figures/abund_dist_figures.Rdata"))



