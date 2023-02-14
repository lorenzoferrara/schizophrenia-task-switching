pvalue.test <- function(v1, v2, pval_min = 0.05){
  n1 <- length(v1)[1] # n1=3
  n2 <- length(v2)[1] # n2=4
  
  p=1
  
  v1.mean <- mean(v1, na.rm=T)
  v2.mean <- mean(v2, na.rm=T)
  v1.cov  <-  var(v1, na.rm=T)
  v2.cov  <-  var(v2, na.rm=T)
  Sp      <- ((n1-1)*v1.cov + (n2-1)*v2.cov)/(n1+n2-2)
  # we compare the matrices

  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  alpha   <- pval_min
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (v1.mean-v2.mean-delta.0) %*% Spinv %*% (v1.mean-v2.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
P <- 1 - pchisq(T2, p)
return(P)
}