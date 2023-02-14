
univariate.test = function(v1, v2, alpha=0.05, CI=FALSE){
  
  library(MASS)
  n1 <- length(v1)
  n2 <- length(v2)
  
  p=1
  
  v1.mean <- mean(v1, na.rm=T)
  v2.mean <- mean(v2, na.rm=T)
  v1.cov  <-  var(v1, na.rm=T)
  v2.cov  <-  var(v2, na.rm=T)
  Sp      <- ((n1-1)*v1.cov + (n2-1)*v2.cov)/(n1+n2-2)
    
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (v1.mean-v2.mean-delta.0) %*% Spinv %*% (v1.mean-v2.mean-delta.0)
  cfr.chisq <- (p*(n1+n2-2)/(n1+n2-1-p))*qchisq(1-alpha,p)
  
  P <- 1 - pchisq(T2, p)
  
  cat("Univariate test to verify if there's a difference between the vectors.\n", sep = "")
  cat("The p-value of the test is ", P, ".",sep = "")
  
  if(P<alpha)
    cat("So at level ", alpha, " we have enough evidence to say that mu1 differs from mu2.\n", sep = "")

  else    
    cat("So at level ", alpha, " we don't have enough evidence to say that mu1 differs from mu2.\n", sep = "")
  
  if(CI==TRUE){
    cat("\n")
    cat("The confidence interval for the difference of the mean is:\n")
    IC.differenza_tempi <- data.frame(lower=v1.mean[1]-v2.mean[1]-sqrt(cfr.chisq*Sp*(1/n1+1/n2)), upper=v1.mean[1]-v2.mean[1]+sqrt(cfr.chisq*Sp*(1/n1+1/n2)) )
    print(IC.differenza_tempi)
  }
}

save(list = "univariate.test", file = "./materiale/workspaces/univariate.test.RData")
