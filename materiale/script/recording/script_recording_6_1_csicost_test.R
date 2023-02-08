
csicost_sani <- t_sani_CsiShort - t_sani_CsiLong
csicost_schz <- t_schz_CsiShort - t_schz_CsiLong
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(csicost_sani), max(csicost_schz))
  m=min(min(csicost_sani), min(csicost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_csicost_sani = rep(0,num)
  table_csicost_schz = rep(0,num)
  
  for (j in 1:length(csicost_sani)){
    for (i in 1:num){
      if(csicost_sani[j] > a[i] & csicost_sani[j] <= a[i+1]){
        table_csicost_sani[i] = table_csicost_sani[i] + 1
      }
    }
  }
  table_csicost_sani=table_csicost_sani/length(csicost_sani)
  
  for (j in 1:length(csicost_schz)){
    for (i in 1:num){
      if(csicost_schz[j] > a[i] & csicost_schz[j] <= a[i+1]){
        table_csicost_schz[i] = table_csicost_schz[i] + 1
      }
    }
  }
  table_csicost_schz=table_csicost_schz/length(csicost_schz)
  
  nomi=round(a[1:num], digits=2)
}

{
  X11()
  par(mfrow=c(2,2))
  m=min(min(csicost_sani), min(csicost_schz))
  M=max(max(csicost_sani), max(csicost_schz))
  boxplot(csicost_sani, xlab="Control", ylim=c(m, M), ylab="Csicost")
  boxplot(csicost_schz, xlab="Schizophrenia", ylim=c(m, M),ylab="Csicost")
  
  M=max(max(table_csicost_schz), max(table_csicost_sani))+0.04
  barplot(table_csicost_sani, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Csicost", ylab="Density")
  barplot(table_csicost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Csicost", ylab="Density")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}


#___________________________________
{
n1 <- length(csicost_sani)[1] # n1=3
n2 <- length(csicost_schz)[1] # n2=4

p=1

csicost_sani
csicost_schz

csicost_sani.mean <- mean(csicost_sani, na.rm=T)
csicost_schz.mean <- mean(csicost_schz, na.rm=T)
csicost_sani.cov  <-  var(csicost_sani, na.rm=T)
csicost_schz.cov  <-  var(csicost_schz, na.rm=T)
Sp      <- ((n1-1)*csicost_sani.cov + (n2-1)*csicost_schz.cov)/(n1+n2-2)
# we compare the matrices
list(S1=csicost_sani.cov, S2=csicost_schz.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

alpha   <- .05
delta.0 <- rep(0,p) #tutti zero
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (csicost_sani.mean-csicost_schz.mean-delta.0) %*% Spinv %*% (csicost_sani.mean-csicost_schz.mean-delta.0)

cfr.chisq <- qchisq(1-alpha,p)
T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
# P-value=0.64

graphics.off()
