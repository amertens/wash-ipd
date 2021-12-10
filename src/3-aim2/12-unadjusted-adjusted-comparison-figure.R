
rm(list=ls())
source(here::here("0-config.R"))

unadj_RR <- readRDS(file=here("results/unadjusted_aim2_pooled.Rds")) 
adj_RR <- readRDS(file=here("results/adjusted_aim2_pooled.Rds")) 

unadj_RR <- clean_res(unadj_RR) %>% rename(unadj.RR=RR)
adj_RR <- clean_res(adj_RR) %>% rename(adj.RR=RR)

#count number of covariates
adj_RR$N_W <- str_count(adj_RR$W,",") + 1
adj_RR$N_W[adj_RR$W=="unadjusted"] <- 0


dim(unadj_RR)
df <- left_join(unadj_RR, adj_RR, by = c("Y", "sample", "target", "study" )) %>% filter(!is.na(se.x),!is.na(se.y))
dim(df)

table(df$Y)

df <- df %>% mutate(diff=coef.y-coef.x, RR.ratio = adj.RR/unadj.RR)
ggplot(df, aes(x=diff)) + geom_histogram() + geom_vline(xintercept=0, linetype="dashed") + facet_wrap(~Y)

df2 <- df %>% filter(Y=="diar7d")
df3 <- df %>% filter(Y=="haz")

summary(df2$adj.RR/df2$unadj.RR)

ggplot(df2, aes(x=RR.ratio)) + geom_density() + geom_vline(xintercept=1, linetype="dashed") 
ggplot(df3, aes(x=diff)) + geom_density() + geom_vline(xintercept=0, linetype="dashed") 


#Report interquartile range
summary(df2$RR.ratio)
summary(df3$diff)

#Difference by number of adjustment covariates
df3$diff

ggplot(df3, aes(x=N_W, y=(diff))) + geom_point() + geom_smooth(method="lm", se=F) + geom_hline(yintercept=0, linetype="dashed") 
ggplot(df3, aes(x=N_W, y=abs(diff))) + geom_point() + geom_smooth(method="lm", se=F) + geom_hline(yintercept=0, linetype="dashed") 
temp<-df3[df3$N_W==2,]

df2$abs_ratio <- ifelse(df2$RR.ratio>1,df2$RR.ratio, 1/df2$RR.ratio)
ggplot(df2, aes(x=N_W, y=abs_ratio)) + geom_point() + geom_smooth(method="lm", se=F) + geom_hline(yintercept=1, linetype="dashed") 

#Difference goes down because or more extreme adjustment in sparse samples? 
