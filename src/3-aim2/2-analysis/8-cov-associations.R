
rm(list=ls())
source(here::here("0-config.R"))


d <- readRDS(paste0(dropboxDir,"Data/merged_env_CH_data_clean.rds"))
head(d)
d <- droplevels(d)
table(d$study, d$pos)
table(d$study)
table(d$study, d$hhwealth)
for(i in unique(d$study)){
  print(summary(d$hhwealth_cont[d$study==i]))
}
table(d$study, d$sample, d$Nhh)

d <- d %>% filter(sample!="FP") %>% droplevels()


#subset data to single obs per sample (ignoring multiple children)
d <- d %>% distinct(study, sample, target, clusterid, hhid, pos, .keep_all = T)

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

Wvars = c("sex","age","hfiacat","momage","hhwealth_cont", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         

for(i in Wvars){
  cat(i,":\n")
  if(length(unique(d[[i]]))>10){
    print(table(d$study, is.na(d[[i]])))
  }else{
    print(table(d$study, d[[i]]))
  }
}

#-----------------------------------
#washb_prescreen by study and outcome for all covariates
#-----------------------------------

prescreen_pval <- function (Y, W, family = "binomial", pval = 0.2, print = TRUE){
  require(lmtest)
  if (pval > 1 | pval < 0) {
    stop("P-value threshold not set between 0 and 1.")
  }
  Ws <- as.data.frame(W)
  dat <- bind_cols(Ws, data.frame(Y=Y))
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  LRp <- data.frame(var= names(Ws), pval=NA)
  
  for (i in 1:nW) {
    dat$W <- dat[, i]
    df <- data.frame(Y=dat$Y, W=dat$W)
    df <- df[complete.cases(df), ]
    
    if (class(dat$W) == "factor" & dim(table(dat$W)) == 
        1) {
      fit1 <- fit0 <- glm(Y ~ 1, data = df, family = family)
    }
    else {
      fit1 <- glm(Y ~ W, data = df, family = family)
      fit0 <- glm(Y ~ 1, data = df, family = family)
    }
    LRp$pval[i] <- lrtest(fit1, fit0)[2, 5]
  }
  
  return(LRp)
}


calc_cov_p <- function(d, Ws, study, sample, target){
  df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
  cat(levels(df$study),", ", sample,", ", target,"\n")
  cat("N before dropping missing: ", nrow(df),"\n")
  
  
  df <- df %>% filter(!is.na(pos))
  Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
  
  res=NULL
  try(res <- prescreen_pval(Y=df$pos, W=Wdf, family="binomial", print=F))
  if(is.null(res)){
    res <- data.frame(var="failed", pval=NA)
  }
  return(res)
}


head(d)
res <- d %>% group_by(study, sample, target) %>%
  do(calc_cov_p(., Ws=Wvars, study=.$study[1], sample=.$sample[1], target=.$target[1]))

unique(res$var)
res$var <- recode(res$var,  
                  nrooms="Number of rooms",
                  #hhwealth="HH wealth cat",
                  hhwealth_cont="HH wealth",
                  landacre="Acres owned",
                  momedu="Mom edu.",
                  hfiacat="HH food security",
                  floor="Improved floor",
                  elec="Electricity",
                  walls="Improved walls",
                  roof="Improved roof",
                  momage="Mom age",
                  age="Child age",
                  tr="Treatment",
                  dadagri="In agriculture",
                  sex="Sex",
                  Nhh="HH size",
                  landown="Owns land")


saveRDS(res, file=here("results/covariate_associations.Rds"))
