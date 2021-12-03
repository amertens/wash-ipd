

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

  
  if(family=="binomial"){
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
  }
  if(family!="binomial"){
    minN <- length(unique(df$Y))
    #Get cell counts
    a <- mean(df$Y[df$tr=="Control"], na.rm=T)
    b <- mean(df$Y[df$tr=="Intervention"], na.rm=T)
    # c <- median(df$Y[df$tr=="Control"], na.rm=T)
    # d <- median(df$Y[df$tr=="Intervention"], na.rm=T)
    c <- sd(df$Y[df$tr=="Control"], na.rm=T)
    d <- sd(df$Y[df$tr=="Intervention"], na.rm=T)
    
    perc_ROQ <- round(mean(df$qual=="ROQ", na.rm=T)*100,1)
  }
  

  if((minN>=5 & min(table(df$Y, df$tr))>1) | length(unique(df$Y)) > 2){
    
    if(!is.null(Ws)){
      Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
      
      if(ncol(Wdf)>0){
        #drop covariates with near zero variance
        if(length(nearZeroVar(Wdf))>0){
          Wdf <- Wdf[,-nearZeroVar(Wdf)]
        }
        if(ncol(Wdf)>0){
          if(family=="neg.binom"){
            Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=F)
          }else{
            Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family=family, print=T)
          }
          if(family!="gaussian" & !is.null(Wvars)){
            nY<-floor(min(table(df$Y))/10) -1 #minus one because 10 variables needed to estimate coef. of X
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
      df <- df %>% subset(., select =c("Y","tr","clusterid", Wvars))
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
      }else{
        df <- df %>% subset(., select =c("Y","tr","clusterid"))
        cat("N before dropping missing: ", nrow(df),"\n")
        df <- df[complete.cases(df),]
        cat("N after dropping missing: ", nrow(df),"\n")
      }
    }else{
      df <- df %>% subset(., select =c("Y","tr","clusterid"))
      cat("N before dropping missing: ", nrow(df),"\n")
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }
    
    #Get cell counts after dropping
    if(family=="binomial"){
    a <- sum(df$Y==1 & df$tr=="Intervention", na.rm=T)
    b <- sum(df$Y==0 & df$tr=="Intervention", na.rm=T)
    c <- sum(df$Y==1 & df$tr=="Control", na.rm=T)
    d <- sum(df$Y==0 & df$tr=="Control", na.rm=T)
    perc_ROQ <- round(mean(df$qual=="ROQ", na.rm=T)*100,1)
    }
    
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
    # res$med_control <- c
    # res$median_int <- d
    res$sd_control <- c
    res$sd_int <- d
    res$perc_ROQ <- perc_ROQ
  }
  

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
    if(family=="binomial"){
      fit <- glm(as.formula(formula),family=poisson(link="log"),  data=df)
    }else{
      fit <- glm(as.formula(formula),family=family,  data=df)
    }
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
# study="Fuhrmeister 2020"
# sample="S"
# target="Any pathogen"
# family="binomial"

#Need to add forcedW funciton, look at washbgam for code

aim2_glm <- function(d, Ws=NULL, forcedW=NULL, outcome="pos", exposure, study="mapsan", sample="ds", target="Mnif", family="binomial"){
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
  if((minN>=2 & min(table(df$Y, df$X))>1) | (minN>=2 & length(unique(df$Y)) > 2 & length(unique(df$X)) == 2) | (minN>=2 & length(unique(df$X)) > 2 & length(unique(df$Y)) >= 2)){
    
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
  #print(res)
  return(res)
}



#Prescreen function
MICS_prescreen<-function (Y, W, family = "gaussian", pval = 0.2, print = TRUE){
  require(lmtest)
  if (pval > 1 | pval < 0) {
    stop("P-value threshold not set between 0 and 1.")
  }
  Ws <- as.data.frame(W)
  dat <- bind_cols(Ws, data.frame(Y=Y))
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  rownames(LRp) <- names(Ws)
  colnames(LRp) <- "P-value"
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
    LRp[i] <- lrtest(fit1, fit0)[2, 5]
  }
  
  
  p20 <- ifelse(LRp < pval, 1, 0)
  if (print == TRUE) {
    cat("\nLikelihood Ratio Test P-values:\n")
    print(round(LRp, 5))
  }
  
  if (sum(p20) > 0) {
    LRps <- matrix(LRp[p20 == 1, ], ncol = 1)
    rownames(LRps) <- names(Ws)[p20 == 1]
    colnames(LRps) <- "P-value"
    LRps <- LRps %>% as.data.frame() %>% arrange(`P-value`)
    cat(paste("\n\nCovariates selected (P<", pval, 
              "):\n", sep = ""))
    print(LRps)
  }else{
    cat(paste("\nNo covariates were associated with the outcome at P<", 
              pval))
  }
  
  if(sum(p20) > 0){
    return(rownames(LRps))
  }else{
    return(NULL)
    
  }
}





#Subgroup function 
aim1_subgroup <- function(d, Vvar, Ws=NULL, outcome="pos", study="mapsan", sample="ds", target="Mnif", family="binomial"){
  
  
  df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
  
  cat(levels(df$study),", ", sample,", ", target,"\n")
  cat("N before dropping missing: ", nrow(df),"\n")
  
  df$Y <- df[[outcome]]
  df <- df %>% filter(!is.na(Y))
  df$V <- df[[Vvar]]
  df <- df %>% filter(!is.na(V))
  #print(summary(df$Y))
  
  Wvars<-NULL
  minN_v0 <- minN_v1 <- 0
  minN<-NA
  
  if(length(unique(df$Y))<=2){
    if(length(unique(df$Y))>1 & length(unique(df$tr))>1 & length(unique(df$V))>1){
      #minN <- min(table(df$Y))
      minN <- min(table(df$Y, df$tr, df$V))
      minN_v0 <- min(table(df$Y[df$V==0], df$tr[df$V==0]))
      minN_v1 <- min(table(df$Y[df$V==1], df$tr[df$V==1]))
    }else{
      minN <- 0
    }
    #Get cell counts
    a <- sum(df$Y==1 & df$tr=="Intervention")
    b <- sum(df$Y==0 & df$tr=="Intervention")
    c <- sum(df$Y==1 & df$tr=="Control")
    d <- sum(df$Y==0 & df$tr=="Control")
  }
  if(length(unique(df$Y))>2){
    minN <- length(unique(df$Y))
    #Get cell counts
    a <- mean(df$Y[df$tr=="Control"], na.rm=T)
    b <- mean(df$Y[df$tr=="Intervention"], na.rm=T)
    # c <- median(df$Y[df$tr=="Control"], na.rm=T)
    # d <- median(df$Y[df$tr=="Intervention"], na.rm=T)
    c <- sd(df$Y[df$tr=="Control"], na.rm=T)
    d <- sd(df$Y[df$tr=="Intervention"], na.rm=T)
  }
  
  
  #if((minN>=10 & min(table(df$Y, df$tr, df$V))>1) | length(unique(df$Y)) > 2){
  if((minN>=1 & min(table(df$Y, df$tr))>1) | length(unique(df$Y)) > 2){
    
    if(!is.null(Ws)){
      Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
      
      if(ncol(Wdf)>0){
        #drop covariates with near zero variance
        if(length(nearZeroVar(Wdf))>0){
          Wdf <- Wdf[,-nearZeroVar(Wdf)]
        }
        if(ncol(Wdf)>0){
        if(family=="neg.binom"){
          Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=F)
        }else{
          Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family=family, print=T)
        }
        if(family!="gaussian" & !is.null(Wvars)){
          nY<-floor(min(table(df$Y))/10) -1 #minus one because 10 variables needed to estimate coef. of X
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
      }else{
        Wvars <- NULL
      }
      df <- df %>% subset(., select =c("Y","tr","V","clusterid", Wvars))
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }else{
      df <- df %>% subset(., select =c("Y","tr","V","clusterid"))
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }
    
    #Get cell counts after dropping
    if(length(unique(df$Y))<=2){
      a <- sum(df$Y==1 & df$tr=="Intervention")
      b <- sum(df$Y==0 & df$tr=="Intervention")
      c <- sum(df$Y==1 & df$tr=="Control")
      d <- sum(df$Y==0 & df$tr=="Control")
    }
    
    #model formula
    f <- ifelse(is.null(Wvars),
                "Y ~ tr +V",
                paste0("Y ~ tr + V +", paste(Wvars, collapse = " + ")))
    #fit model
    fullfit <- mpreg(formula = as.formula(f), df = df, vcv=TRUE, family=family)
    #coef <- as.data.frame(t(fit[2,]))
    fit <- fullfit$fit
    
    contrasts1 <- contrasts2 <- rep(0, nrow(fit))
    contrasts1[2] <- 1
    contrasts2[2:3] <- 1
    
    meas <- ifelse(family=="gausian","RD","RR")
    v1 <- suppressWarnings(lincom(lc = contrasts1, fit = fullfit$fit, vcv = fullfit$vcovCL, measure = meas, flag = 1))
    v2 <- suppressWarnings(lincom(lc = contrasts2, fit = fullfit$fit, vcv = fullfit$vcovCL, measure = meas, flag = 1))
    res <- bind_rows(data.frame(v1),data.frame(v2))
    res$Vlevel<-c(0,1)
    res$nV <- NA
    res$nV[1] <- sum(df$V==0)
    res$nV[2] <- sum(df$V==1)
    res$int.p <- fit[3,4]
    
    
    res <- data.frame(Y=outcome,
                      V=Vvar,
                      sample=sample,
                      target=target,
                      res)
    
  }else{
    
    res <-res_v0 <-res_v1 <- NULL
    if((minN_v0 >= 1 | minN_v1 >= 1)){
      if(minN_v0 >= 1 & min(table(df$Y[df$V==0], df$tr[df$V==0]))>=1& length(unique(df$Y[df$V==0]))>1 & length(unique(df$tr[df$V==0]))>1){
        res_v0 <- subgroup_single_level_est(df=df, Vvar=Vvar, Vlevel=0, family=family, Ws=Ws, outcome=outcome, sample=sample, target=target)
        #cat("d")
        Wvars <- res_v0[[2]]
        res_v0 <- res_v0[[1]]
      }
      if(minN_v1 >= 1 & min(table(df$Y[df$V==1], df$tr[df$V==1]))>=1 & length(unique(df$Y[df$V==1]))>1 & length(unique(df$tr[df$V==1]))>1){
        res_v1 <- subgroup_single_level_est(df=df, Vvar=Vvar, Vlevel=1, family=family, Ws=Ws, outcome=outcome, sample=sample, target=target)
        Wvars <- res_v1[[2]]
        res_v1 <- res_v1[[1]]
      }
    }
    if(is.null(res_v0)){
      res_v0 <- data.frame(Y=outcome,
                        sample=sample,
                        target=target,
                        V=Vvar,
                        Vlevel=0,
                        coef=NA,
                        RR=NA,
                        se=NA,
                        Zval=NA,
                        pval=NA,
                        ci.lb=NA,
                        ci.ub=NA)
    }
    if(is.null(res_v1)){
      res_v1 <- data.frame(Y=outcome,
                        sample=sample,
                        target=target,
                        V=Vvar,
                        Vlevel=1,
                        coef=NA,
                        RR=NA,
                        se=NA,
                        Zval=NA,
                        pval=NA,
                        ci.lb=NA,
                        ci.ub=NA)
    }
    res <- bind_rows(res_v0,res_v1)
    
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
    res$sd_control <- c
    res$sd_int <- d
  }
  #cat("e")
  
  
  res$N<-nrow(df)
  res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))
  res$study <- study
  #cat("f")
  
  return(res)
}


subgroup_single_level_est <- function(df, Vvar, Vlevel, family, Ws, outcome, sample, target){
  df <- df[df[[Vvar]]==Vlevel,]
  
  if((min(table(df$Y, df$tr))>1 & length(unique(df$Y)) >= 2 )| length(unique(df$Y)) > 2){
  
  Wvars <- NULL
  if(!is.null(Ws)){
    Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
    
    if(ncol(Wdf)>0){
      #drop covariates with near zero variance
      if(length(nearZeroVar(Wdf))>0){
        Wdf <- Wdf[,-nearZeroVar(Wdf)]
      }
      if(ncol(Wdf)>0){
        
        if(family=="neg.binom"){
          Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=F)
        }else{
          Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family=family, print=T)
        }
        if(family!="gaussian" & !is.null(Wvars)){
          nY<-floor(min(table(df$Y))/10) -1 #minus one because 10 variables needed to estimate coef. of X
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
    }else{
      Wvars <- NULL
    }
    df <- df %>% subset(., select =c("Y","tr","V","clusterid", Wvars))
    df <- df[complete.cases(df),]
    cat("N after dropping missing: ", nrow(df),"\n")
  }else{
    df <- df %>% subset(., select =c("Y","tr","V","clusterid"))
    df <- df[complete.cases(df),]
    cat("N after dropping missing: ", nrow(df),"\n")
  }
  
  #Get cell counts after dropping
  if(length(unique(df$Y))<=2){
    a <- sum(df$Y==1 & df$tr=="Intervention")
    b <- sum(df$Y==0 & df$tr=="Intervention")
    c <- sum(df$Y==1 & df$tr=="Control")
    d <- sum(df$Y==0 & df$tr=="Control")
  }
  
  #model formula
  f <- ifelse(is.null(Wvars),
              "Y ~ tr",
              paste0("Y ~ tr + ", paste(Wvars, collapse = " + ")))
 # cat("a")
  
  fit <- mpreg(formula = as.formula(f), df = df, vcv=FALSE, family=family)
  #cat("b")
  
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
                      V=Vvar,
                      coef=NA,
                      RR=NA,
                      se=NA,
                      Zval=NA,
                      pval=NA,
                      ci.lb=NA,
                      ci.ub=NA)
  }
  

  res$V <- Vvar
  res$Vlevel<- Vlevel
  res$nV <- sum(df$V==Vlevel)
  res$int.p <- NA
  #cat("c")
  
  return(list(res, Wvars))
}




# aim1_subgroup_old <- function(d, Vvar, Ws=NULL, outcome="pos", study="mapsan", sample="ds", target="Mnif", family="binomial"){
#   
#   
#   df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
#   
#   cat(df$study[1],", ", sample,", ", target,"\n")
#   cat("N before dropping missing: ", nrow(df),"\n")
#   
#   df$Y <- df[[outcome]]
#   df <- df %>% filter(!is.na(Y))
#   df$V <- df[[Vvar]]
#   df <- df %>% filter(!is.na(V))
#   #print(summary(df$Y))
#   
#   Wvars<-NULL
#   minN<-NA
#   
#   if(length(unique(df$Y))<=2){
#     if(length(unique(df$Y))>1 & length(unique(df$tr))>1 & length(unique(df$V))>1){
#       #minN <- min(table(df$Y))
#       minN <- min(table(df$Y, df$tr, df$V))
#     }else{
#       minN <- 0
#     }
#     #Get cell counts
#     a <- sum(df$Y==1 & df$tr=="Intervention")
#     b <- sum(df$Y==0 & df$tr=="Intervention")
#     c <- sum(df$Y==1 & df$tr=="Control")
#     d <- sum(df$Y==0 & df$tr=="Control")
#   }
#   if(length(unique(df$Y))>2){
#     minN <- length(unique(df$Y))
#     #Get cell counts
#     a <- mean(df$Y[df$tr=="Control"], na.rm=T)
#     b <- mean(df$Y[df$tr=="Intervention"], na.rm=T)
#     # c <- median(df$Y[df$tr=="Control"], na.rm=T)
#     # d <- median(df$Y[df$tr=="Intervention"], na.rm=T)
#     c <- sd(df$Y[df$tr=="Control"], na.rm=T)
#     d <- sd(df$Y[df$tr=="Intervention"], na.rm=T)
#   }
#   
#   
#   #if((minN>=10 & min(table(df$Y, df$tr, df$V))>1) | length(unique(df$Y)) > 2){
#   if((minN>=1 & min(table(df$Y, df$tr))>1) | length(unique(df$Y)) > 2){
#     
#     if(!is.null(Ws)){
#       Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
#       
#       if(ncol(Wdf)>0){
#         #drop covariates with near zero variance
#         if(length(nearZeroVar(Wdf))>0){
#           Wdf <- Wdf[,-nearZeroVar(Wdf)]
#         }
#         if(family=="neg.binom"){
#           Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=F)
#         }else{
#           Wvars <- MICS_prescreen(Y=df$Y, W=Wdf, family=family, print=T)
#         }
#         if(family!="gaussian" & !is.null(Wvars)){
#           nY<-floor(min(table(df$Y))/10) -1 #minus one because 10 variables needed to estimate coef. of X
#           if(nY>=1){
#             if(length(Wvars)>nY){
#               Wvars<-Wvars[1:nY]
#             }        
#           }else{
#             Wvars=NULL
#           }
#         }
#         if(identical(Wvars, character(0)) ){
#           Wvars <- NULL
#         }
#       }else{
#         Wvars <- NULL
#       }
#       df <- df %>% subset(., select =c("Y","tr","V","clusterid", Wvars))
#       df <- df[complete.cases(df),]
#       cat("N after dropping missing: ", nrow(df),"\n")
#     }else{
#       df <- df %>% subset(., select =c("Y","tr","V","clusterid"))
#       df <- df[complete.cases(df),]
#       cat("N after dropping missing: ", nrow(df),"\n")
#     }
#     
#     #Get cell counts after dropping
#     if(length(unique(df$Y))<=2){
#       a <- sum(df$Y==1 & df$tr=="Intervention")
#       b <- sum(df$Y==0 & df$tr=="Intervention")
#       c <- sum(df$Y==1 & df$tr=="Control")
#       d <- sum(df$Y==0 & df$tr=="Control")
#     }
#     
#     #model formula
#     f <- ifelse(is.null(Wvars),
#                 "Y ~ tr +V",
#                 paste0("Y ~ tr + V +", paste(Wvars, collapse = " + ")))
#     #fit model
#     fullfit <- mpreg(formula = as.formula(f), df = df, vcv=TRUE, family=family)
#     #coef <- as.data.frame(t(fit[2,]))
#     fit <- fullfit$fit
#     
#     contrasts1 <- contrasts2 <- rep(0, nrow(fit))
#     contrasts1[2] <- 1
#     contrasts2[2:3] <- 1
#     
#     meas <- ifelse(family=="gausian","RD","RR")
#     v1 <- suppressWarnings(lincom(lc = contrasts1, fit = fullfit$fit, vcv = fullfit$vcovCL, measure = meas, flag = 1))
#     v2 <- suppressWarnings(lincom(lc = contrasts2, fit = fullfit$fit, vcv = fullfit$vcovCL, measure = meas, flag = 1))
#     res <- bind_rows(data.frame(v1),data.frame(v2))
#     res$Vlevel<-c(0,1)
#     res$nV <- NA
#     res$nV[1] <- sum(df$V==0)
#     res$nV[2] <- sum(df$V==1)
#     res$int.p <- fit[3,4]
#     
#   
#       res <- data.frame(Y=outcome,
#                         V=Vvar,
#                         sample=sample,
#                         target=target,
#                         res)
#     
#   }else{
#     res <- data.frame(Y=outcome,
#                       sample=sample,
#                       target=target,
#                       V=Vvar,
#                       coef=NA,
#                       RR=NA,
#                       se=NA,
#                       Zval=NA,
#                       pval=NA,
#                       ci.lb=NA,
#                       ci.ub=NA)
#   }
#   
#   if(length(unique(df$Y))<=2){
#     res$minN <- minN
#     res$n<-sum(df$Y, na.rm=T)
#     res$a <- a
#     res$b <- b
#     res$c <- c
#     res$d <- d
#   }else{
#     res$mean_control <- a
#     res$mean_int <- b
#     # res$med_control <- c
#     # res$median_int <- d
#     res$sd_control <- c
#     res$sd_int <- d
#   }
#   
#   
#   res$N<-nrow(df)
#   res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))
#   res$study <- study
#   
#   return(res)
# }



lincom <- function (lc = NULL, fit, vcv, measure = "RR", flag = NULL){
  x <- fit
  if (!is.null(lc)) {
    coef <- paste(rownames(x)[which(lc != 0)], collapse = " + ")
  }
  else {
    stop("Specify lc")
  }
  if (measure == "RD") {
    est <- (t(lc) %*% x[, 1])
    se <- sqrt(t(lc) %*% vcv %*% lc)
    Z <- (est)/se
    P <- 2 * pnorm(-abs(Z))
    lb <- est - 1.96 * se
    ub <- est + 1.96 * se
    res <- matrix(c(est, se, lb, ub, P), nrow = 1)
    colnames(res) <- c("coef", "se", "ci.lb", 
                       "ci.ub", "pval")
  }
  else {
    coef <- (t(lc) %*% x[, 1])
    est <- exp(t(lc) %*% x[, 1])
    se <- sqrt(t(lc) %*% vcv %*% lc)
    Z <- log(est)/se
    P <- 2 * pnorm(-abs(Z))
    lb <- exp(log(est) - 1.96 * se)
    ub <- exp(log(est) + 1.96 * se)
    res <- matrix(c(coef,est, se, lb, ub, P), nrow = 1)
    colnames(res) <- c("coef","RR", "se", "ci.lb", 
                       "ci.ub", "pval")
  }
  if (is.null(flag)) {
    cat("\nLinear combination of coefficients:\n")
    print(coef)
  }
  return(res)
}

















# library(tmle)
# d<-df
#  Ws = Wvars_anthro
#  forcedW=c("age", "hhwealth")
#  outcome="haz"
#  exposure="pos"
#  study=df$study[1]
#  sample=df$sample[1]
#  target=df$target[1]
#  family="gaussian"

SL.forcedW <- function(X,...){
  returnCols <- rep(FALSE, ncol(X))
  returnCols[names(X) %in% c("age","hhwealth")] <- TRUE
  return(returnCols)
}



aim2_tmle <- function(d, Ws=NULL, forcedW=NULL, outcome, exposure="pos", study="mapsan", sample="ds", target="Mnif", family="binomial", overwrite=F){
  
  res = NULL
  if(overwrite==F){
    try(res <- readRDS(file=paste0(here::here(),"/results/tmle/",outcome,"-",exposure,"-",sample,"-",target,"-",study,".RDS")))
  }
  if(is.null(res)){
    
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
    if((minN>=2 & min(table(df$Y, df$X))>1) | (minN>=2 & length(unique(df$Y)) > 2 & length(unique(df$X)) == 2) | (minN>=2 & length(unique(df$X)) > 2 & length(unique(df$Y)) >= 2)){
      
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
          Wvars <- colnames(Wdf)
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
      
  
      #fit model
      Ydf <- df %>% ungroup() %>% select(Y)
      Xdf <- df %>% ungroup() %>% select(X) 
      Wdf <- df %>% ungroup() %>% select(any_of(Wvars)) %>% select_if(~sum(!is.na(.)) > 0)
      if(length(nearZeroVar(Wdf))>0){
        Wdf <- Wdf[,-nearZeroVar(Wdf)]
      }
      
     
  
      SL.lib=list(c("SL.glm","SL.forcedW"),c("SL.glm","screen.corRank"),
                      c("SL.mean","All"),
                      c("SL.glmnet","All"),
                  c("SL.randomForest","All"))

      
      fit <- tmle(Y=df$Y, A=df$X, W=Wdf, id=df$clusterid, family=family,
                  Q.SL.library = SL.lib,
                  g.SL.library = SL.lib)
      #print(summary(fit))
  
      if(family=="gaussian"){
        res <- data.frame(Y=outcome,
                          sample=sample,
                          target=target,
                          coef=fit$estimates$ATE$psi,
                          RR=NA,
                          se=NA,
                          Zval=NA,
                          pval=fit$estimates$ATE$pvalue)
        
        res$ci.lb <- fit$estimates$ATE$CI[1]
        res$ci.ub <- fit$estimates$ATE$CI[2]
      }else{
        res <- data.frame(Y=outcome,
                          sample=sample,
                          target=target,
                          coef=log(fit$estimates$RR$psi),
                          RR=fit$estimates$RR$psi,
                          se=NA,
                          Zval=NA,
                          pval=fit$estimates$RR$pvalue)
        
        res$ci.lb <- fit$estimates$RR$CI[1]
        res$ci.ub <- fit$estimates$RR$CI[2]
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
    
  
    
    saveRDS(res, file=paste0(here::here(),"/results/tmle/",outcome,"-",exposure,"-",sample,"-",target,"-",study,".RDS"))
  }
   
  return(res)
}




















poolRR<-function(d, method="REML"){
  
  if(nrow(d)>0){
    
    d <- d %>% rename(untransformed_estimate=coef, untransformed_se=se)  
    
    fit<-NULL
    try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"))
    if(method=="REML"){
      if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="DL", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="HE", measure="RR"))}
    }
    
    #confint(fit)
    
    est<-data.frame(fit$b, fit$se, fit$I2)
    colnames(est)<-c("logRR.psi","logSE", "I2")
    
    est$RR<-exp(est$logRR)
    est$ci.lb<-exp(est$logRR - 1.96 * est$logSE)
    est$ci.ub<-exp(est$logRR + 1.96 * est$logSE)
    est$N <- d$N[1]
    est <- est %>% mutate(Y=d$Y[1],study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
    
  }else{
    est <- data.frame(Y=NA,study=NA, sparse=NA, sample_type=NA, sample_cat=NA, target_f=NA)
  }
  
  return(est)
}


pool.cont<-function(d, method="REML"){
  
  fit<-NULL
  try(fit<-rma(yi=coef, sei=se, data=d, method=method, measure="GEN"))
  if(method=="REML"){
    if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="ML", measure="GEN"))}
    if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="DL", measure="GEN"))}
    if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="HE", measure="GEN"))}
  }
  est<-data.frame(fit$b, fit$ci.lb, fit$ci.ub, fit$I2)
  colnames(est)<-c("coef","ci.lb","ci.ub", "I2")
  
  est$N <- d$N[1]
  est <- est %>% mutate(Y=d$Y[1],study="Pooled", sparse="pooled", sample_type=d$sample_type[1], sample_cat=d$sample_cat[1], target_f=d$target_f[1])
  
  rownames(est) <- NULL
  return(est)
}
