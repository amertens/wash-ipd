


library(tmle)
library(washb)

set.seed(123)
Y <- rnorm(100, 5, 3)
summary(Y )
Y<-floor(Y)
Y[Y<0] <- 0
table(Y)

A <- rbinom(100, 1, 0.3)
W <- data.frame(W1=rnorm(100, 5, 3), W2=rnorm(100, 5, 3))
head(W)

res1 <- tmle(Y=Y, A=A, W=W, family="gaussian")
res1


res2 <- tmle(Y=Y, A=A, W=W, family="poisson")
res2$estimates$OR


res3 <- washb_tmle(Y=Y, tr=A, W = W,  family = "gaussian", contrast=c(0,1), Q.SL.library = c("SL.glm","SL.step", "SL.glm.interaction"), FECR = "arithmetic") 



res3