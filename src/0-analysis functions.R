

makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  tibble(name = names(labels),
         label = labels)
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
  fit <- glm(as.formula(formula),family=family, weights = df$weight, data=df)
  vcovCL <- cl(df=df,fm=fit,cluster=df$id)
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
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}