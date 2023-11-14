
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 

unadj_RR <- clean_res(unadj_RR) %>% rename(unadj.RR=RR, unadj.diff=coef)
adj_RR <- clean_res(adj_RR) %>% rename(adj.RR=RR, adj.diff=coef)

#count number of covariates
adj_RR$N_W <- str_count(adj_RR$W,",") + 1
adj_RR$N_W[adj_RR$W=="unadjusted"] <- 0


dim(unadj_RR)
df <- left_join(unadj_RR, adj_RR, by = c("Y", "sample", "target", "study" )) %>% filter(!is.na(se.x),!is.na(se.y))
dim(df)

table(df$Y)


df <- df %>% mutate(diff=round(adj.diff-unadj.diff,6), abs_diff=abs(diff), RR.ratio = adj.RR/unadj.RR, abs_ratio = ifelse(RR.ratio>1,RR.ratio, 1/RR.ratio))

ggplot(df %>% filter(Y=="haz"), aes(x=unadj.diff, y=adj.diff)) + geom_point() + geom_abline() + geom_smooth()
ggplot(df %>% filter(Y=="diar7d"), aes(x=unadj.RR, y=adj.RR)) + geom_point() + geom_abline() + geom_smooth() +     
  scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10')


#Drop non-distinct repeats of any virus, etc.
dim(df)
df <- df %>% ungroup() %>% distinct(study, Y, diff, .keep_all = T)
dim(df)

unique(df$Y)
df2 <- df %>% filter(Y %in% c("diar7d","stunt","wast","underwt"))
df3 <- df %>% filter(Y %in% c("haz","whz","waz"))

summary(df2$adj.RR/df2$unadj.RR)

# ggplot(df2, aes(x=RR.ratio)) + geom_density() + geom_vline(xintercept=1, linetype="dashed") 
# ggplot(df3, aes(x=diff)) + geom_density() + geom_vline(xintercept=0, linetype="dashed") 
# 
# ggplot(df2, aes(x=N_W, y=(RR.ratio))) + geom_point() + scale_y_continuous(trans='log10') + geom_smooth() + geom_hline(yintercept=1, linetype="dashed") 
# ggplot(df3, aes(x=N_W, y=(diff))) + geom_point() + geom_smooth() + geom_hline(yintercept=0, linetype="dashed") 


#Report interquartile range
summary(df2$RR.ratio)
summary(df3$diff)

mean(df$adj.RR[df$adj.RR>1], na.rm=T)
mean(df$unadj.RR[df$unadj.RR>1], na.rm=T)
mean(df$adj.RR[df$adj.RR<1], na.rm=T)
mean(df$unadj.RR[df$unadj.RR<1], na.rm=T)
median(df$adj.RR[df$adj.RR>1], na.rm=T)
median(df$unadj.RR[df$unadj.RR>1], na.rm=T)
median(df$adj.RR[df$adj.RR<1], na.rm=T)
median(df$unadj.RR[df$unadj.RR<1], na.rm=T)


mean(df$unadj.diff, na.rm=T)
mean(df$adj.diff, na.rm=T)
median(df$unadj.diff, na.rm=T)
median(df$adj.diff, na.rm=T)


mean(abs(df$unadj.diff), na.rm=T)
mean(abs(df$adj.diff), na.rm=T)
median(abs(df$unadj.diff), na.rm=T)
median(abs(df$adj.diff), na.rm=T)

mean(abs(df3$unadj.diff), na.rm=T)
mean(abs(df3$adj.diff), na.rm=T)
median(abs(df3$unadj.diff), na.rm=T)
median(abs(df3$adj.diff), na.rm=T)


mean(abs(df3$adj.diff)-abs(df3$unadj.diff), na.rm=T)


median((df3$diff), na.rm=T)


df3 %>% group_by(N_W) %>% 
  summarise(mean(diff), mean(abs_diff, na.rm=T))

df2 %>% group_by(N_W) %>% 
  summarise(mean(RR.ratio), median(RR.ratio), mean(abs_ratio, na.rm=T))

df2 %>% group_by(N_W) %>% 
  summarise(mean(diff), median(diff), mean(abs_diff, na.rm=T))

ave_abs_logdiff = round(mean(df2$abs_diff, na.rm=T),2)
p_logdiff <-  ggplot(df2, aes(x=N_W , y=diff)) + 
  geom_point(size = 4, alpha=0.25) + geom_smooth(se=F) +
  xlab("Number of covariates used in the adjusted analysis") + 
  ylab("Difference between unadjusted and adjusted estimates") +
  geom_hline(yintercept = 0) + theme_ki() +
  scale_x_continuous(breaks = pretty(df$N_W, n = 10)) +
  geom_text(aes(x=10, y=-0.6, label=paste0("Average absolute log-transformed difference: ",ave_abs_logdiff))) 
p_logdiff


p_ave_abs_diff = round(mean(df3$abs_diff, na.rm=T),2)
p_ave_abs_ratio = round(mean(df2$abs_ratio, na.rm=T),2)

ave_diff = round(mean(df3$diff, na.rm=T),2)
ave_ratio = round(mean(df2$RR.ratio, na.rm=T),2)


p_diff <-  ggplot(df3, aes(x=N_W , y=diff)) + 
  geom_point(size = 4, alpha=0.25) + geom_smooth(se=F) +
  xlab("Number of covariates used in the adjusted analysis") + 
  ylab("Difference between adjusted and unadjusted estimates") +
  geom_hline(yintercept = 0) + theme_ki() +
  scale_x_continuous(breaks = pretty(df$N_W, n = 10)) +
  geom_text(aes(x=9, y=-0.6, label=paste0("Average absolute difference: ",p_ave_abs_diff))) 
p_diff

ggsave(p_diff, file = paste0(here::here(),"/figures/pngs/aim2_adjusted_unadjusted_differences.png"), width = 10, height = 6)


p_RR <-  ggplot(df2, aes(x=N_W , y=RR.ratio)) + 
  geom_point(size = 4, alpha=0.25) + geom_smooth(se=F) +
  xlab("Number of covariates used in the adjusted analysis") + 
  ylab("Ratio between adjusted and unadjusted estimates") +
  geom_hline(yintercept = 1) + theme_ki() +
  scale_x_continuous(breaks = pretty(df$N_W, n = 10)) +
  geom_text(aes(x=9, y=0.5, label=paste0("Average absolute ratio: ",p_ave_abs_ratio))) 
p_RR

ggsave(p_RR, file = paste0(here::here(),"/figures/pngs/aim2_adjusted_unadjusted_ratio.png"), width = 10, height = 6)
save(list=ls(pattern="p_"), file=here("figures/aim2_adjusted_unadjusted_comp_figures.Rdata"))

