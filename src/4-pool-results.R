
rm(list=ls())
source(here::here("0-config.R"))
library(metafor)
d <- readRDS(here("results/unadjusted_aim1_RR.Rds"))

poolRR<-function(d, method="REML"){

   d <- d %>% rename(untransformed_estimate=coef, untransformed_se=se)  
  
    fit<-NULL
    try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"))
    if(method=="REML"){
      if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="DL", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="HE", measure="RR"))}
    }
 
      est<-data.frame(fit$b, fit$se)
      colnames(est)<-c("logRR.psi","logSE")
      
      est$RR<-exp(est$logRR)
      est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
      est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
  
  return(est)
}

#pool primary estimates by study

head(d)
table(d$sample, d$target)

d <- d %>% filter(sample=="any sample type", target=="Any pathogen")
head(d)
d$N

res <- poolRR(d)
res

saveRDS(res, file=here("results/pooled_RR.Rds"))
