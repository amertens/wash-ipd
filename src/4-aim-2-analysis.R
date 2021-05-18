
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_CH_data.rds"))
head(d)
d <- droplevels(d)
table(d$trial, d$pos)


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

#TODO: need to add child health specific covariates
Wvars = c("hhwealth",
     "Nhh",
     "momedu",
     "hfiacat",
  "hhwealth","nrooms","momedu","walls" ,    
"floor","elec","tr")         

# Ws <- d %>% select(!!(Wvars))
# try(Ws<-Ws[,-nearZeroVar(Ws)])

# table(d$study)
# table(d$target)
# table(d$sample)
# d <- d %>% filter(target=="Any pathogen", sample=="S")
# 
# res <- aim2_glm(d, Ws=NULL, outcome="diar7d", exposure="pos", study="Fuhrmeister et al. 2020", sample="S", target="Any pathogen", family="binomial")
# 
# res <- washb_glm(Y=d$diar7d, tr=d$pos, W=Ws, id=d$clusterid.x, contrast=c("0","1"), family=binomial(link='log'), pval=0.2, print=TRUE)
# res

#Subset to

#-----------------------------------
# Unadjusted RR
#-----------------------------------
table(d$pos, d$diar7d)

outcome="diar7d"
exposure="pos"
study=d$study[1]
sample=d$sample[1]
target=d$target[1]
family="binomial"
Ws=NULL

res <- d %>% group_by(study, sample, target) %>%
   do(aim2_glm(., outcome="diar7d", exposure="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], family="binomial"))
res


# Primary outcomes for this aim include: 
#   Prevalence of diarrhea and length-for-age z-scores (LAZ).
# Secondary outcomes include z-scores for weight-for-age (WAZ), 
# weight-for-length (WLZ), head circumference and middle-upper-arm-circumference,
# prevalence of stunting, wasting and underweight, prevalence and intensity of infection with 
# specific enteropathogens, and prevalence of respiratory infections. We will address Aim 2 
# by estimating PRs and PDs for the binary health outcomes and mean differences for the 
# continuous health outcomes for individuals with vs. without exposure to pathogens and 
# MST markers in environmental samples. The primary outcomes of Aim 1 (prevalence of any enteropathogen, 
# and any general, human or animal MST markers 
# in environmental samples) will be used as the exposure variables for this aim. 

# For the LAZ, stunting and head circumference outcomes,
# we will consider all environmental samples collected over the child's 
# lifetime prior to the anthropometry measurement. For the other outcomes, 
# we will only consider environmental samples collected up to three months 
# before the measurement of the health outcome. 
summary(res$RR)

#drop estimates with less than 10 values
res <- res %>% filter(minN>=10)

saveRDS(res, file=here("results/unadjusted_aim2_RR.Rds"))




