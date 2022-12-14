
rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)

table(d$study)
table(d$target)


#drop baseline observations and food because only in one study and no estimates
table(is.na(d$round))
food <- d %>% filter(sample=="FP") %>% droplevels()
table(food$study)
table(food$target, food$pos)
d <- d %>% filter(round!="bl", sample!="FP") %>% droplevels()

#clean covariates
d <- aim1_clean_covariates(d)

Wvars = c("hhwealth", "Nhh","nrooms","walls", "floor","roof","elec","dadagri","landown","landacre", "momedu", "momage")         





d
Ws=Wvars
outcome="pos"
study="Boehm 2016"
sample="any sample type"
target="Any pathogen"
family="poisson"
table(d$study)

#analysis function
aim1_cowboy <- function(d, Ws=NULL, outcome="pos", study="mapsan", sample="ds", target="Mnif", family="poisson"){
  
  
  df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
  
  #Odisha is unadjusted because of village-level
  if(df$trial[1]=="Odisha"){Ws=NULL}
  
  cat(df$study[1],", ", sample,", ", target,"\n")
  cat("N before dropping missing: ", nrow(df),"\n")
  
  df$Y <- df[[outcome]]
  df <- df %>% filter(!is.na(Y))
  #print(summary(df$Y))
  
  Wvars<-NULL
  minN<-NA
  
  
  if(family=="binomial"|family=="poisson"){
    if(length(unique(df$Y))>1 & length(unique(df$tr))>1){
      minN <- min(table(df$Y))
    }else{
      minN <- 0
    }
    #Get cell counts
    a <- sum(df$Y==1 & df$tr=="Intervention")
    b <- sum(df$Y==0 & df$tr=="Intervention")
    c <- sum(df$Y==1 & df$tr=="Control")
    d <- sum(df$Y==0 & df$tr=="Control")
  }else{
    minN <- length(unique(df$Y))
    #Get cell counts
    a <- mean(df$Y[df$tr=="Control"], na.rm=T)
    b <- mean(df$Y[df$tr=="Intervention"], na.rm=T)
    c <- sd(df$Y[df$tr=="Control"], na.rm=T)
    d <- sd(df$Y[df$tr=="Intervention"], na.rm=T)
    
    med.cont <- median(df$Y[df$tr=="Control"], na.rm=T)
    med.int <- median(df$Y[df$tr=="Intervention"], na.rm=T)
    
    #perc_ROQ <- round(mean(df$qual=="ROQ", na.rm=T)*100,1)
  }
  
  
  Wdf=NULL
  if(!is.null(Ws)){
    Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
    
    if(ncol(Wdf)>0){
      #drop covariates with near zero variance
      if(length(nearZeroVar(Wdf))>0){
        Wdf <- Wdf[,-nearZeroVar(Wdf)]
      }
    }
  }else{
    Wdf=NULL
  }
  
  
  if((minN>=5  | length(unique(df$Y)) > 2) & !is.null(Wdf)){
    
    res=NULL
    Wdf <- droplevels(Wdf)
    Wdf=design_matrix(Wdf)
    try(res <- washb_glm_lasso_boot(Y=df$Y,tr=df$tr,pair=NULL,id=df$clusterid, W=Wdf, contrast=c("Control","Intervention"), family="poisson", pval=0.2, print=TRUE))
    
    if(!is.null(res)){
      coef=res$TR
      if(family=="gaussian"){
        res <- data.frame(Y=outcome,
                          sample=sample,
                          target=target,
                          coef=coef$Estimate,
                          RR=NA,
                          se=coef$`Std. Error`,
                          Zval=coef$`z value`,
                          pval=coef$`Pr(>|z|)`)
        
        res$ci.lb <- res$coef - 1.96*res$se
        res$ci.ub <- res$coef + 1.96*res$se
      }else{
        res <- data.frame(Y=outcome,
                          sample=sample,
                          target=target,
                          coef=log(coef$RR),
                          RR=coef$RR,
                          se=coef$se,
                          Zval=NA,
                          pval=NA)
        
        res$ci.lb <- coef$ci.lb
        res$ci.ub <- coef$ci.ub
      }
    }else{
      res <- data.frame(Y=outcome,
                        sample=sample,
                        target=target,
                        coef=NA,
                        RR=NA,
                        se=NA,
                        Zval=NA,
                        pval=NA,
                        ci.lb=NA,
                        ci.ub=NA)
    }
      

  }else{
    res <- data.frame(Y=outcome,
                      sample=sample,
                      target=target,
                      coef=NA,
                      RR=NA,
                      se=NA,
                      Zval=NA,
                      pval=NA,
                      ci.lb=NA,
                      ci.ub=NA)
  }
  
  if(family=="binomial"){
    res$minN <- minN
    res$n<-sum(df$Y, na.rm=T)
    res$a <- a
    res$b <- b
    res$c <- c
    res$d <- d
  }else{
    res$mean_control <- a
    res$mean_int <- b
    res$sd_control <- c
    res$sd_int <- d

    # res$med.cont <-med.cont 
    # res$med.int <-med.int 
  }
  
  
  res$N<-nrow(df)
  res$W <-ifelse(is.null(Wdf), "unadjusted", paste(colnames(Wdf), sep="", collapse=", "))
  res$study <- study
  
  return(res)
}



#-----------------------------------
# Adjusted RR
#-----------------------------------

unique(d$target)
#d <- d %>% filter(grepl("Any", target)) 
d <- d %>% filter(target=="Any pathogen"|target=="Any MST") 

res <- d %>% 
  group_by(study, sample, target, aggregate_Y) %>%
  do(aim1_cowboy(., outcome="pos", study=.$study[1], sample=.$sample[1], target=.$target[1], Ws=Wvars, family="poisson"))
res$sparse <- ifelse(is.na(res$RR), "yes", "no")
res$RR[is.na(res$RR)] <- 1
res

saveRDS(res, file=here("results/adjusted_aim1_RR_prescreen_sens.Rds"))
