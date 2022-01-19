
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
table(d$study, d$sample, d$pos)

d <- d %>% filter(sample!="FP") %>% droplevels()



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

Wvars = c("sex","age","hfiacat","momage","hhwealth_cont","hhwealth", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         
Wvars_anthro = c("sex","age_anthro","hfiacat","momage","hhwealth_cont", "Nhh","nrooms","walls", "roof", "floor","elec","dadagri","landacre","landown", "momedu", "tr")         


#-----------------------------------
# Unadjusted RR
#-----------------------------------

outcome="diar7d"
exposure="pos"
study="Holcomb 2020"
sample="any sample type"
target= "Any MST"
family="binomial"
forcedW=c("age", "hhwealth_cont", "hhwealth")
Ws=Wvars



  df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
  
  cat(levels(df$study)[1],", ", sample,", ", target,"\n")
  cat("N before dropping missing: ", nrow(df),"\n")
  
  df$Y <- df[[outcome]]
  df <- df %>% filter(!is.na(Y))
  #print(summary(df$Y))
  
  df$X <- df[[exposure]]
  df <- df %>% filter(!is.na(X))
  #print(summary(df$exposure))
  
  forcedWdf<-Wvars<-NULL
  minN<-NA
  forcedW_n <- 0
  
  if(length(unique(df$Y))<=2){
    if(length(unique(df$Y))>1 & length(unique(df$X))>1){
      minN <- min(table(df$Y,df$X))
    }else{
      minN <- 0
    }
    #Get cell counts
    a <- sum(df$Y==1 & df$X==1)
    b <- sum(df$Y==0 & df$X==1)
    c <- sum(df$Y==1 & df$X==0)
    d <- sum(df$Y==0 & df$X==0)
  }
  
  if(length(unique(df$Y))>2){
    minN <- length(unique(df$Y))
    #Get cell counts
    a <- mean(df$Y[df$X==0], na.rm=T)
    b <- mean(df$Y[df$X==1], na.rm=T)
    # c <- median(df$Y[df$X==0], na.rm=T)
    # d <- median(df$Y[df$X==1], na.rm=T)
    c <- sd(df$Y[df$X==0], na.rm=T)
    d <- sd(df$Y[df$X==1], na.rm=T)
  }
  
  cat(minN,"\n")
  
  
  
  #cat(minN>=10 | length(unique(df$Y)) > 2)
  #if((minN>=10 & min(table(df$Y, df$X))>1) | (minN>=10 & length(unique(df$Y)) > 2 & length(unique(df$X)) == 2)){
  #if((minN>=2 & min(table(df$Y, df$X))>1) | (minN>=2 & length(unique(df$Y)) > 2 & length(unique(df$X)) == 2) | (minN>=2 & length(unique(df$X)) > 2 & length(unique(df$Y)) >= 2)){
    
    if(!is.null(Ws)){
      forcedW_n <- 0
      if(!is.null(forcedW)){
        Ws <- Ws[!(Ws %in% forcedW)]
        forcedWdf <- df %>% ungroup() %>% select(any_of(forcedW)) %>% select_if(~sum(!is.na(.)) > 0)
        if(length(nearZeroVar(forcedWdf))>0){
          forcedWdf <- forcedWdf[,-nearZeroVar(forcedWdf)]
        }
        forcedW_n <- ncol(forcedWdf)
      }
      
      Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
      
      if(ncol(Wdf)>0){
        #drop covariates with near zero variance
        if(length(nearZeroVar(Wdf))>0){
          Wdf <- Wdf[,-nearZeroVar(Wdf)]
        }
        if(family=="neg.binom"){
          Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=F)
        }else{
          Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family=family, print=F)
        }
        if(family!="gaussian" & !is.null(Wvars)){
          nY<-floor(min(table(df$Y))/10) -1 #minus one because 10 variables needed to estimate coef. of X
          nY <- nY - forcedW_n
          if(nY>=1){
            if(length(Wvars)>nY){
              Wvars<-Wvars[1:nY]
            }        
          }else{
            Wvars=NULL
          }
        }
        if(identical(Wvars, character(0)) ){
          Wvars <- NULL
        }
      }else{
        Wvars <- NULL
      }
      if(!is.null(forcedW)){
        #Wvars <- c(Wvars,forcedW)
        df <- df %>% subset(., select =c("Y","X","clusterid",Wvars))
        df <- data.frame(df,forcedWdf)
        Wvars = colnames(df)[-c(1:3)]
      }else{
        df <- df %>% subset(., select =c("Y","X","clusterid", Wvars))
      }
      
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }else{
      df <- df %>% subset(., select =c("Y","X","clusterid"))
      cat("N before dropping missing: ", nrow(df),"\n")
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }
    
    #Get cell counts
    if(length(unique(df$Y))<=2){
      a <- sum(df$Y==1 & df$X==1)
      b <- sum(df$Y==0 & df$X==1)
      c <- sum(df$Y==1 & df$X==0)
      d <- sum(df$Y==0 & df$X==0)
    }
    
    #model formula
    f <- ifelse(is.null(Wvars),
                "Y ~ X",
                paste0("Y ~ X + ", paste(Wvars, collapse = " + ")))
    
    
    f
    res <- glm(Y ~ X, data=df, family=family)
    summary(res)    
    res <- glm(Y ~ hhwealth_cont, data=df, family=family)
    summary(res)    
    res <- glm(Y ~ X + age + hhwealth_cont, data=df, family=family)
    summary(res)    
    res <- glm(Y ~ X + age + hhwealth, data=df, family=family)
    summary(res)    
    
    
    
    
    
    
    
    #fit model
    fit <- mpreg(formula = as.formula(f), df = df, vcv=FALSE, family=family)
    coef <- as.data.frame(t(fit[2,]))
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
                        coef=coef$Estimate,
                        RR=exp(coef$Estimate),
                        se=coef$`Std. Error`,
                        Zval=coef$`z value`,
                        pval=coef$`Pr(>|z|)`)
      
      res$ci.lb <- exp(res$coef - 1.96*res$se)
      res$ci.ub <- exp(res$coef + 1.96*res$se) 
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
                      ci.ub=NA,
                      minN=minN)
  }
  
  if(length(unique(df$Y))<=2){
    res$minN <- minN
    res$n<-sum(df$Y, na.rm=T)
    res$a <- a
    res$b <- b
    res$c <- c
    res$d <- d
  }else{
    res$mean_control <- a
    res$mean_int <- b
    # res$med_control <- c
    # res$median_int <- d
    res$sd_control <- c
    res$sd_int <- d
  }
  
  res$N<-nrow(df)
  # if(!is.null(forcedW)){
  #   Wvars <- c(Wvars, colnames(forcedWdf))
  # }
  res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))
  res$study <- study

