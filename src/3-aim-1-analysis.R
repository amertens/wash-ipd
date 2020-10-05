
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_data.rds"))


# ms %>% group_by(type, target) %>%
#   summarize(N=n(), npos=sum(pos), prev=round(mean(pos),3)*100, mean_log_quant=round(mean(logquant, na.rm=T),2)) %>%
#   as.data.frame()


study="WBB"
type="ds"
family="gaussian"
target="Mnif"
outcome="detect"
family="binomial"

# 1.	Child birth order/parity 
# 2.	Asset-based wealth index 
# 3.	Number of individuals and children in household
# 4.	Household food security 
# 5.	Household electrification and construction, including wall/roof material 
# 6.	Parental age 
# 7.	Parental education 
# 8.	Parental employment 
# a.	Indicator for works in agriculture 
# 9.	Land ownership 
Ws=c("hhwealth",
     "Nhh",
     "nrooms",
     "momedu",
     "walls",
     "floor",
     "elec")




washb_prescreen(Y=df$Y, W=Ws, family=family, print=T)


outcome="detect"
study="mapsan"
type="ds"
target="Mnif"
family="gaussian"


#Regression function 
aim1_glm <- function(outcome="detect", study="mapsan", type="ds", target="Mnif", family="gaussian"){
  df <- d %>% filter(study==study, type==type, target==target)
  df$Y <- df[,outcome]

  
  res = glm(Y~tr, data=df, family=family)
  summary(res)$coefficients[2,]
  
  return(res)
}



#Estimate pooling function







