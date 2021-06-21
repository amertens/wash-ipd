

rm(list=ls())
source(here::here("0-config.R"))
d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
d <- d %>% filter(!is.na(abund))

df <- d %>% filter(target=="Avian", study=="Boehm 2016")
table(df$abund)

#make list of density plots
combos <- d %>% distinct(study, target)

plotlist <- list()
for(i in 1:nrow(combos)){
  plotdf <- d %>% filter(study==combos$study[i], target==combos$target[i])
  p <- ggplot(plotdf, aes(x=abund, fill=sample)) + geom_density(adjust = 2) + 
    facet_wrap(~sample, scales="free") + xlab(paste0(plotdf$study[1],"\n",plotdf$target[1]))
  plotlist[[i]] <- p
}

plotlist[[1]]



logplotlist <- list()
for(i in 1:nrow(combos)){
  plotdf <- d %>% filter(study==combos$study[i], target==combos$target[i])
  p <- ggplot(plotdf, aes(x=log10(abund), fill=sample)) + geom_density(adjust = 2 ) + 
    facet_wrap(~sample, scales="free") + xlab(paste0(plotdf$study[1],"\n",plotdf$target[1]))
  logplotlist[[i]] <- p
}

logplotlist[[1]]


save(plotlist, logplotlist, file=here("figures/density_figures.Rdata"))
