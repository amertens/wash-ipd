

makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  tibble(name = names(labels),
         label = labels)
}


#Regression function 
aim1_glm <- function(d, Ws=NULL, outcome="pos", study="mapsan", sample="ds", target="Mnif", family="binomial"){
  
  
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
  
  if(length(unique(df$Y))<=2){
    if(length(unique(df$Y))>1 & length(unique(df$tr))>1){
      minN <- min(table(df$Y))
    }else{
      minN <- 0
    }
  }
  if(length(unique(df$Y))>2){
    minN <- length(unique(df$Y))
  }
  
  #Get cell counts
  a <- sum(df$Y==1 & df$tr=="Intervention")
  b <- sum(df$Y==0 & df$tr=="Intervention")
  c <- sum(df$Y==1 & df$tr=="Control")
  d <- sum(df$Y==0 & df$tr=="Control")
  
  if((minN>=10 & min(table(df$Y, df$tr))>1) | length(unique(df$Y)) > 2){
    
    if(!is.null(Ws)){
      Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
      
      if(ncol(Wdf)>0){
        #drop covariates with near zero variance
        if(length(nearZeroVar(Wdf))>0){
          Wdf <- Wdf[,-nearZeroVar(Wdf)]
        }
        if(family=="neg.binom"){
          Wvars <- washb_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=F)
        }else{
          Wvars <- washb_prescreen(Y=df$Y, W=Wdf, family=family, print=F)
        }
        if(identical(Wvars, character(0)) ){
          Wvars <- NULL
        }
      }else{
        Wvars <- NULL
      }
      df <- df %>% subset(., select =c("Y","tr","clusterid", Wvars))
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }else{
      df <- df %>% subset(., select =c("Y","tr","clusterid"))
      cat("N before dropping missing: ", nrow(df),"\n")
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }
    
    #Get cell counts after dropping
    a <- sum(df$Y==1 & df$tr=="Intervention")
    b <- sum(df$Y==0 & df$tr=="Intervention")
    c <- sum(df$Y==1 & df$tr=="Control")
    d <- sum(df$Y==0 & df$tr=="Control")
    
    #model formula
    f <- ifelse(is.null(Wvars),
                "Y ~ tr",
                paste0("Y ~ tr + ", paste(Wvars, collapse = " + ")))
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
                      ci.ub=NA)
  }
  
  if(length(unique(df$Y))<=2){
    res$minN <- minN
    res$n<-sum(df$Y, na.rm=T)
  }
  
  res$a <- a
  res$b <- b
  res$c <- c
  res$d <- d
  res$N<-nrow(df)
  res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))
  res$study <- study
  
  return(res)
}




# --------------------------------------
# Convenience wrapper function to run 
# modified Poisson models and obtain 
# robust SEs (clusterd on hhid)
# this is the work horse of all the 
# regressions run in this analysis
# --------------------------------------

mpreg <- function(formula, df, vcv=FALSE, family) {
  # modified Poisson regression formula
  # dataset used to fit the model	
  if(family!="neg.binom"){
    fit <- glm(as.formula(formula),family=family,  data=df)
  }else{
    fit <- MASS::glm.nb(as.formula(formula), data=df)
  }
  vcovCL <- cl(df=df, fm=fit, cluster=df$clusterid)
  rfit <- coeftest(fit, vcovCL)
  #print(summary(fit))
  #cat("\n\nRobust, Sandwich Standard Errors Account for Clustering:\n")
  #print(rfit) 
  if(vcv==FALSE) {
    return(rfit)
  } else {
    list(fit=rfit,vcovCL=vcovCL)
  }
}



cl   <- function(df,fm, cluster){
  # df: data used to fit the model
  # fm : model fit (object)
  # cluster : vector of cluster IDs
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum)) ;
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}
















#Regression function -aim2
# Ws=NULL
# outcome="diar7d"
# exposure="pos"
# study="Fuhrmeister et al. 2020"
# sample="S"
# target="Any pathogen"
# family="binomial"

aim2_glm <- function(d, Ws=NULL, outcome="pos", exposure, study="mapsan", sample="ds", target="Mnif", family="binomial"){
  df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
  
  cat(study,", ", sample,", ", target,"\n")
  cat("N before dropping missing: ", nrow(df),"\n")
  
  df$Y <- df[[outcome]]
  df <- df %>% filter(!is.na(Y))
  #print(summary(df$Y))
  
  df$X <- df[[exposure]]
  df <- df %>% filter(!is.na(X))
  #print(summary(df$exposure))
  
  Wvars<-NULL
  minN<-NA
  
  if(length(unique(df$Y))<=2){
    if(length(unique(df$Y))>1 & length(unique(df$X))>1){
      minN <- min(table(df$Y))
    }else{
      minN <- 0
    }
  }
  cat(minN,"\n")
  
  #cat(minN>=10 | length(unique(df$Y)) > 2)
  if(minN>=10 | length(unique(df$Y)) > 2){
    
    if(!is.null(Ws)){
      Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
      
      if(ncol(Wdf)>0){
        #drop covariates with near zero variance
        if(length(nearZeroVar(Wdf))>0){
          Wdf <- Wdf[,-nearZeroVar(Wdf)]
        }
        if(family=="neg.binom"){
          Wvars <- washb_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=F)
        }else{
          Wvars <- washb_prescreen(Y=df$Y, W=Wdf, family=family, print=F)
        }
        if(identical(Wvars, character(0)) ){
          Wvars <- NULL
        }
      }else{
        Wvars <- NULL
      }
      df <- df %>% subset(., select =c("Y","X","clusterid", Wvars))
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }else{
      df <- df %>% subset(., select =c("Y","X","clusterid"))
      cat("N before dropping missing: ", nrow(df),"\n")
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }
    
    #model formula
    f <- ifelse(is.null(Wvars),
                "Y ~ X",
                paste0("Y ~ X + ", paste(Wvars, collapse = " + ")))
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
  }
  
  res$N<-nrow(df)
  res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))
  res$study <- study
  print(res)
  return(res)
}
