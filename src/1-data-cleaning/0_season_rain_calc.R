
rm(list=ls())
source(here::here("0-config.R"))

bd <- read.table(here("data/rain_data/crucy.v3.23.1901.2014.Bangladesh.pre.per"), header=T, skip = 3) 
india <- read.table(here("data/rain_data/crucy.v3.23.1901.2014.India.pre.per"), header=T, skip = 3) 
kenya <- read.table(here("data/rain_data/crucy.v3.23.1901.2014.Kenya.pre.per"), header=T, skip = 3) 
mz <- read.table(here("data/rain_data/crucy.v3.23.1901.2014.Mozambique.pre.per"), header=T, skip = 3) 

mn_bd <- colMeans(bd) %>% as.data.frame(.) %>% mutate(country="Bangladesh")
mn_india <- colMeans(india) %>% as.data.frame(.) %>% mutate(country="India")
mn_kenya <- colMeans(kenya) %>% as.data.frame(.) %>% mutate(country="Kenya")
mn_mz <- colMeans(mz) %>% as.data.frame(.) %>% mutate(country="Mozambique")
mn_bd$month <- rownames(mn_bd)
mn_india$month <- rownames(mn_india)
mn_kenya$month <- rownames(mn_kenya)
mn_mz$month <- rownames(mn_mz)

df <- bind_rows(mn_bd, mn_india, mn_kenya, mn_mz)
df <- df %>% filter(!(month %in% c("YEAR", "MAM", "JJA", "SON", "DJF", "ANN")))
colnames(df)[1] <- "rain"

df <- df %>% group_by(country) %>% 
  mutate(rain=scale(rain)) %>% ungroup() %>%
  mutate(month = factor(month, levels=c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")))

ggplot(df, aes(y=country, x=month, fill=rain)) + geom_tile() + theme(legend.position = "bottom")


# Southeast Asia has a monsoon season (May-Oct) where most rainfall occurs, Kenya has two distinct rainy seasons (Apr-Jun, Nov-Dec), 
# and Mozambique experiences a rainy season (Nov-Apr); the six months with the highest rainfall are intended to capture these rainy periods. 