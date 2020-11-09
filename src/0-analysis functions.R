

makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  tibble(name = names(labels),
         label = labels)
}


#Regression function 
aim1_glm <- function(d, Ws=NULL, outcome="pos", study="mapsan", type="ds", target="Mnif", family="binomial"){
  df <- d %>% filter(study=={{study}}, type=={{type}}, target=={{target}}) %>% droplevels(.)
  
  cat(study,", ", type,", ", target,"\n")
  #print(head(df))
  
  df$Y <- df[[outcome]]
  Wvars<-NULL
  minN<-NA
  
  if(length(unique(df$Y))<=2){
    if(length(unique(df$Y))>1){
      minN <- min(table(df$tr, df$Y))
    }else{
      minN <- 0
    }
  }
  if(minN>=10 | length(unique(df$Y))>2){
    
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
                        type=type,
                        target=target,
                        coef=coef$Estimate,
                        se=coef$`Std. Error`,
                        Zval=coef$`z value`,
                        pval=coef$`Pr(>|z|)`)
      
      res$ci.lb <- res$coef - 1.96*res$se
      res$ci.ub <- res$coef + 1.96*res$se
    }else{
      res <- data.frame(Y=outcome,
                        type=type,
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
                      type=type,
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
  # dataaset used to fit the model	
  if(family!="neg.binom"){
    fit <- glm(as.formula(formula),family=family,  data=df)
  }else{
    fit <- MASS::glm.nb(as.formula(formula), data=df)
  }
  vcovCL <- cl(df=df,fm=fit,cluster=df$clusterid)
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