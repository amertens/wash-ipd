
aim2_glm_pathogens <- function(d, Ws=NULL, forcedW=NULL, outcome="pos", exposure, study="mapsan", sample="ds", target="Mnif", family="binomial", minN_thres = 0){
  
  df <- d %>% filter(study=={{study}}, sample=={{sample}}, target=={{target}}) %>% droplevels(.)
  
   cat(levels(df$study)[1],", ", sample,", ", target,"\n")

  df$Y <- df[[outcome]]
  df <- df %>% filter(!is.na(Y))

  df$X <- df[[exposure]]
  df <- df %>% filter(!is.na(X))

  forcedWdf<-Wvars<-NULL
  minN<-minN_thres
  forcedW_n <- 0
  a <- b <- c <- d <- NA
  
  if(length(unique(df$Y))<=2){
    if(length(unique(df$Y))>1 & length(unique(df$X))>1){
      if(length(unique(df$X))>2){
        minN <- min(table(df$Y))
      }else{
        minN <- min(table(df$Y,df$X))
      }
    }else{
      minN <- 0
    }
    #Get cell counts
    a <- sum(df$Y==1 & df$X==1)
    b <- sum(df$Y==0 & df$X==1)
    c <- sum(df$Y==1 & df$X==0)
    d <- sum(df$Y==0 & df$X==0)
  }

  if(length(unique(df$Y))>2 & length(unique(df$X)) <= 2){

    minN <- min(table(df$X))

    #Get cell counts
    a <- mean(df$Y[df$X==0], na.rm=T)
    b <- mean(df$Y[df$X==1], na.rm=T)
    c <- sd(df$Y[df$X==0], na.rm=T)
    d <- sd(df$Y[df$X==1], na.rm=T)
  }

  cat(minN,"\n")
  
  if(dim(df)[1]>=10 & length(unique(paste0(df$X, df$Y)))==4){

    if(!is.null(Ws) & minN>0){
      forcedW_n <- 0

      Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)

      if(ncol(Wdf)>0){
        #drop covariates with near zero variance
        if(length(nearZeroVar(Wdf))>0){
            Wdf <- Wdf[,-nearZeroVar(Wdf)]
          }        

        
        Wvars <- colnames(Wdf)
        
        if(identical(Wvars, character(0)) ){
          Wvars <- NULL
        }
      }else{
        Wvars <- NULL
      }
      if(!is.null(forcedW)){
        df <- df %>% subset(., select =c("Y","X","clusterid",Wvars))
        df <- data.frame(df,forcedWdf)
        Wvars = colnames(df)[-c(1:3)]
      }else{
        df <- df %>% subset(., select =c("Y","X","clusterid", Wvars))
      }

      df <- df[complete.cases(df),]
    }else{
      df <- df %>% subset(., select =c("Y","X","clusterid"))
      df <- df[complete.cases(df),]
    }

    #Get cell counts
    if(length(unique(df$Y))<=2){
      a <- sum(df$Y==1 & df$X==1)
      b <- sum(df$Y==0 & df$X==1)
      c <- sum(df$Y==1 & df$X==0)
      d <- sum(df$Y==0 & df$X==0)
    }
  
    cat(table(df$X, df$Y))
    
    #model formula
    f <- ifelse(is.null(Wvars),
                "Y ~ X",
                paste0("Y ~ X + ", paste(Wvars, collapse = " + ")))
    
    #fit model
    res <- glm(formula = as.formula(f), data = df, family=family)
    summary(res)
    
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
  
    res$minN <- minN
    res$n<-sum(df$Y, na.rm=T)
    res$a <- a
    res$b <- b
    res$c <- c
    res$d <- d

  
  res$N<-nrow(df)

  res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))
  res$study <- study

  return(res)
}




