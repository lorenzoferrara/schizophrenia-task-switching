
iticost_sani <- t_sani_itiShort - t_sani_itiLong
iticost_schz <- t_schz_itiShort - t_schz_itiLong
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(iticost_sani), max(iticost_schz))
  m=min(min(iticost_sani), min(iticost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_iticost_sani = rep(0,num)
  table_iticost_schz = rep(0,num)
  
  for (j in 1:length(iticost_sani)){
    for (i in 1:num){
      if(iticost_sani[j] > a[i] & iticost_sani[j] <= a[i+1]){
        table_iticost_sani[i] = table_iticost_sani[i] + 1
      }
    }
  }
  table_iticost_sani=table_iticost_sani/length(iticost_sani)
  
  for (j in 1:length(iticost_schz)){
    for (i in 1:num){
      if(iticost_schz[j] > a[i] & iticost_schz[j] <= a[i+1]){
        table_iticost_schz[i] = table_iticost_schz[i] + 1
      }
    }
  }
  table_iticost_schz=table_iticost_schz/length(iticost_schz)
  
  nomi=round(a[1:num], digits=2)
}

{
  X11()
  par(mfrow=c(2,2))
  m=min(min(iticost_sani), min(iticost_schz))
  M=max(max(iticost_sani), max(iticost_schz))
  boxplot(iticost_sani, xlab="Control", ylim=c(m, M), ylab="iticost")
  boxplot(iticost_schz, xlab="Schizophrenia", ylim=c(m, M),ylab="iticost")
  
  M=max(max(table_iticost_schz), max(table_iticost_sani))+0.04
  barplot(table_iticost_sani, names.arg = nomi, ylim = c(0, M), main="Control", xlab="iticost", ylab="Density")
  barplot(table_iticost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="iticost", ylab="Density")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}


#___________________________________
{
  n1 <- length(iticost_sani)[1] # n1=3
  n2 <- length(iticost_schz)[1] # n2=4
  
  p=1
  
  iticost_sani
  iticost_schz
  
  iticost_sani.mean <- mean(iticost_sani, na.rm=T)
  iticost_schz.mean <- mean(iticost_schz, na.rm=T)
  iticost_sani.cov  <-  var(iticost_sani, na.rm=T)
  iticost_schz.cov  <-  var(iticost_schz, na.rm=T)
  Sp      <- ((n1-1)*iticost_sani.cov + (n2-1)*iticost_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=iticost_sani.cov, S2=iticost_schz.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (iticost_sani.mean-iticost_schz.mean-delta.0) %*% Spinv %*% (iticost_sani.mean-iticost_schz.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
# P-value=0.64

graphics.off()
