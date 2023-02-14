
#DA BUTTARE E DA RIFARE

fumocost_sani <- t_sani_fumo - t_sani_nonfumo
fumocost_schz <- t_schz_fumo - t_schz_nonfumo
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(fumocost_sani), max(fumocost_schz))
  m=min(min(fumocost_sani), min(fumocost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_fumocost_sani = rep(0,num)
  table_fumocost_schz = rep(0,num)
  
  for (j in 1:length(fumocost_sani)){
    for (i in 1:num){
      if(fumocost_sani[j] > a[i] & fumocost_sani[j] <= a[i+1]){
        table_fumocost_sani[i] = table_fumocost_sani[i] + 1
      }
    }
  }
  table_fumocost_sani=table_fumocost_sani/length(fumocost_sani)
  
  for (j in 1:length(fumocost_schz)){
    for (i in 1:num){
      if(fumocost_schz[j] > a[i] & fumocost_schz[j] <= a[i+1]){
        table_fumocost_schz[i] = table_fumocost_schz[i] + 1
      }
    }
  }
  table_fumocost_schz=table_fumocost_schz/length(fumocost_schz)
  
  nomi=round(a[1:num], digits=2)
}

{
  X11()
  par(mfrow=c(2,2))
  m=min(min(fumocost_sani), min(fumocost_schz))
  M=max(max(fumocost_sani), max(fumocost_schz))
  boxplot(fumocost_sani, xlab="Control", ylim=c(m, M), ylab="Smoking cost")
  boxplot(fumocost_schz, xlab="Schizophrenia", ylim=c(m, M),ylab="Smoking cost")
  
  M=max(max(table_fumocost_schz), max(table_fumocost_sani))+0.04
  barplot(table_fumocost_sani, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Smoking cost", ylab="Density")
  barplot(table_fumocost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Smoking cost", ylab="Density")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}


#___________________________________
{
  n1 <- length(fumocost_sani)[1] # n1=3
  n2 <- length(fumocost_schz)[1] # n2=4
  
  p=1
  
  fumocost_sani.mean <- mean(fumocost_sani, na.rm=T)
  fumocost_schz.mean <- mean(fumocost_schz, na.rm=T)
  fumocost_sani.cov  <-  var(fumocost_sani, na.rm=T)
  fumocost_schz.cov  <-  var(fumocost_schz, na.rm=T)
  Sp      <- ((n1-1)*fumocost_sani.cov + (n2-1)*fumocost_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=fumocost_sani.cov, S2=fumocost_schz.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (fumocost_sani.mean-fumocost_schz.mean-delta.0) %*% Spinv %*% (fumocost_sani.mean-fumocost_schz.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
# P-value=0.64

graphics.off()
