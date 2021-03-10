
library(tidyverse)

theme_ki<-function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

d <- data.frame(
  study = rep(c("Odisha","Gram Vikas","MapSan",rep("WBB",4),rep("WBK",4),"CHoBI7", rep("pooled",4)),3),
  tr = rep(c("Sanitation","Combined","Sanitation","Water","Sanitation","Hygeine","Combined","Water","Sanitation","Hygeine","Combined","Combined","Water","Sanitation","Hygeine","Combined"),3),
  Outcome = rep(c("any enteropathogen", "any human MST markers","any animal MST markers"), each=16),
  RR = rnorm(48, 0.9, 0.25))

d$RR[d$study=="CHoBI7" & d$Outcome=="any human MST markers"] <-  NA
d$RR[d$study=="CHoBI7" & d$Outcome=="any animal MST markers"] <-  NA


d$ci.1 = d$RR - 0.2
d$ci.2 = d$RR + 0.2


d$study <- factor(d$study, levels=rev(unique(d$study)))
d$tr <- factor(d$tr, levels=rev(c("Combined","Hygeine","Sanitation","Water")))
d$Outcome <- factor(d$Outcome, levels=c("any enteropathogen", "any human MST markers","any animal MST markers"))



p <- ggplot(d, (aes(x=study, y=RR))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.1, ymax=ci.2),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 1, linesample="dashed") +
  geom_vline(xintercept = 1.5) +
  facet_grid(tr~Outcome,  scales="free") +
  scale_y_continuous(trans='log10') +
  coord_flip()+
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) + theme_ki()
p














#mediation plot

df <- rbind(
  data.frame(d, sample="Total effect"),
  data.frame(d, sample="Controlled direct effect"),
  data.frame(d, sample="Natural direct effect"),
  data.frame(d, sample="Natural indirect effect")
)
df$RR[df$sample!="Total effect"] <- df$RR[df$sample!="Total effect"] - 0.2
df$ci.1[df$sample!="Total effect"] <- df$ci.1[df$sample!="Total effect"] - 0.2
df$ci.2[df$sample!="Total effect"] <- df$ci.2[df$sample!="Total effect"] - 0.2

df$RR[df$RR<0.2] <- 0.2
df$ci.1[df$ci.1<0.2] <- 0.2
df$ci.2[df$ci.2<0.2] <- 0.2

df <- df %>% filter(study!="pooled", Outcome=="any enteropathogen")

df <- rbind(
  data.frame(df, mediator="any entero-\npathogen"),
  data.frame(df, mediator="any human\nMST markers"),
  data.frame(df, mediator="any animal\nMST markers")
)


p <- ggplot(df, (aes(x=study, y=RR, group=sample, fill=sample, color=sample))) + 
  geom_point(size=3, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin=ci.1, ymax=ci.2), 
                position = position_dodge(0.5),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 1, linesample="dashed") +
  facet_grid(mediator~tr,  scales="free") +
  scale_y_continuous(trans='log10') +
  scale_fill_manual(values=c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
  scale_color_manual(values=c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
  coord_flip()+
  theme_ki() +
  theme(axis.ticks.x=element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) 
p