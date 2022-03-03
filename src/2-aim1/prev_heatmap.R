


rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)

table(d$study)
table(d$target)


#drop baseline observations and food because only in one study and no estimates
table(is.na(d$round))
d <- d %>% filter(round!="bl", sample!="FP") %>% droplevels()

unique(d$target)
d <- d %>% filter(!(target %in% c( "Any pathogen","Any MST",
                                  "Any virus","Any protozoa","Any bacteria",
                                  "Any STH" ,  "Any human MST","Any animal MST",
                                  "Any zoonotic","Any non-zoonotic" )), 
                  sample!="any sample type")

df <- d %>% group_by(study, target, sample) %>% summarize(prev=mean(pos, na.rm=T))
summary(df$prev)
textcol= "grey20"


#Get significance category
df$prev_cat <- cut(df$prev, breaks = c(-1,0.025,0.05,0.1, 0.25, 0.5, 0.8, 2), labels = c("<2%","2-5%","5-10%","10-25%","25-50%","50-80%",">80%"))
table(df$prev_cat)
length(unique(df$prev_cat))

df$prev_cat <- factor(df$prev_cat, 
                      levels = c("<2%","2-5%","5-10%","10-25%","25-50%","50-80%",">80%"))

cols = rev(RColorBrewer::brewer.pal(n = 8, name = "Spectral"))



p <- ggplot(df, aes(x = target, y = sample, fill = prev_cat )) + 
  geom_tile(colour = "grey80", size = 0.25) + scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal(base_size = 8) +
  facet_wrap(~study, scales = "free") +
  scale_fill_manual(values = cols, drop = FALSE, na.value = 'grey80') +
  # scale_color_manual(values = c("grey30", "black"), drop = FALSE, guide = FALSE) +
  theme(legend.title = element_text(color = textcol,
                                    size = 8), legend.margin = margin(grid::unit(0.1, "cm")), legend.text = element_text(colour = textcol,
                                                                                                                         size = 7, face = "bold"), legend.key.height = grid::unit(0.2,   "cm"), legend.key.width = grid::unit(1, "cm"),
        legend.position = "bottom", 
        axis.text.x = element_text(size = 8, angle=25, hjust=1, colour = textcol), 
        axis.ticks = element_line(size = 0.4),
        axis.text.y = element_markdown(size = 8, vjust = 0.2),  
        plot.title = element_text(colour = textcol, hjust = 0, size = 12, face = "bold"), strip.text.x = element_text(size = 10),
        strip.text.y = element_text(angle = 0, size = 10),
        plot.background = element_blank(), panel.border = element_blank(),
        strip.background = element_blank(), panel.background = element_rect(fill = "grey80", colour = "grey80"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend("Prevalence")) + labs(x = "Target", y = "Sample", title = "")

p




