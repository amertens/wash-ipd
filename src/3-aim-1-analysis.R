
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/cleaned_ipd_env_data.rds"))
head(d)
d <- droplevels(d)
d$id <- 1:nrow(d)

# ms %>% group_by(type, target) %>%
#   summarize(N=n(), npos=sum(pos), prev=round(mean(pos),3)*100, mean_log_quant=round(mean(logquant, na.rm=T),2)) %>%
#   as.data.frame()


study="WBB"
type="ds"
family="gaussian"
target="Mnif"
outcome="pos"
family="binomial"

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
Ws=c("hhwealth",
     "Nhh",
     "momedu",
     "hfiacat",
  "hhwealth","nrooms","momedu","walls" ,    
"floor","elec")         



Ws=NULL


outcome="pos"
study="WBB"
type="CH"
target="ECVG"
family="binomial"


#Regression function 
aim1_glm <- function(d, Ws=NULL, outcome="pos", study="mapsan", type="ds", target="Mnif", family="binomial"){
  df <- d %>% filter(study=={{study}}, type=={{type}}, target=={{target}})
  
  cat(study,", ", type,", ", target,"\n")
  #print(head(df))
  
  df$Y <- df[[outcome]]
  Wvars<-NULL
  
  if(!is.null(Ws)){
    Wdf <- df %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
    Wvars <- washb_prescreen(Y=df$Y, W=Wdf, family=family, print=F)
  }

  #model formula
  f <- ifelse(is.null(Wvars),
              "Y ~ tr",
              paste0("Y ~ tr + ", paste(Wvars, collapse = " + ")))
  #fit model
  
  fit <- mpreg(formula = as.formula(f), df = df, vcv=FALSE, family=family)
  coef <- as.data.frame(t(fit[2,]))
  res <- data.frame(Y=outcome,
                    type=type,
                    target=target,
                    coef=coef$Estimate,
                    RR=exp(coef$Estimate),
                    se=coef$`Std. Error`,
                    Zval=coef$`z value`,
                    pval=coef$`Pr(>|z|)`)
  
  #Calc 95%CI
  if(family=="gaussian"){
    res$ci.lb <- res$coef - 1.96*res$se
    res$ci.ub <- res$coef + 1.96*res$se
  }else{
    res$ci.lb <- exp(res$coef - 1.96*res$se)
    res$ci.ub <- exp(res$coef + 1.96*res$se) 
  }
  
  
  res$n<-sum(df$Y, na.rm=T)
  res$N<-nrow(df)
  res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))
  res$study <- study
  
  return(res)
}



# #wrapper_function
# run_aim1_glms <- function(){
#   fullres <- NULL
#   
#   return(fullres)
# }



d %>% distinct(study, type, target)

res <- aim1_glm(d, outcome="pos", study="mapsan", type="ls", target="cEC", family="binomial")
res



#NOTES:
#add code to skip estimate if sparse N <10, and add code to only pick n/10 covariates


#-----------------------------------
# Unadjusted RR
#-----------------------------------
res <- d %>% group_by(study, type, target) %>%
   do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], family="binomial"))
res

summary(res$RR)
res[res$RR<0.1,]
res[res$RR>20,]

save(res, file=here("results/unadjusted_aim1_RR.Rds"))


#-----------------------------------
# Unadjusted RD
#-----------------------------------
res <- d %>% group_by(study, type, target) %>%
  do(aim1_glm(., outcome="pos", study=.$study[1], type=.$type[1], target=.$target[1], family="binomial"))
res

summary(res$RR)
res[res$RR<0.1,]
res[res$RR>20,]

save(res, file=here("results/unadjusted_aim1_RR.Rds"))



#-----------------------------------
# Unadjusted abundance (negative binomial)
#-----------------------------------




#Primary figure
res %>% filter(RR>0.1, RR < 20) %>%
  #filter(adjusted==1, binary==1) %>% 
  droplevels(.) %>%
  ggplot(., aes(y=RR, x=target, color=Y)) +
  facet_grid(type~study) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau10[4:1]) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")






