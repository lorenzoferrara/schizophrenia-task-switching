
correctcost_sani <- t_sani_correct - t_sani_noncorrect
correctcost_schz <- t_schz_correct - t_schz_noncorrect
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(correctcost_sani), max(correctcost_schz))
  m=min(min(correctcost_sani), min(correctcost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_correctcost_sani = rep(0,num)
  table_correctcost_schz = rep(0,num)
  
  for (j in 1:length(correctcost_sani)){
    for (i in 1:num){
      if(correctcost_sani[j] > a[i] & correctcost_sani[j] <= a[i+1]){
        table_correctcost_sani[i] = table_correctcost_sani[i] + 1
      }
    }
  }
  table_correctcost_sani=table_correctcost_sani/length(correctcost_sani)
  
  for (j in 1:length(correctcost_schz)){
    for (i in 1:num){
      if(correctcost_schz[j] > a[i] & correctcost_schz[j] <= a[i+1]){
        table_correctcost_schz[i] = table_correctcost_schz[i] + 1
      }
    }
  }
  table_correctcost_schz=table_correctcost_schz/length(correctcost_schz)
  
  nomi=round(a[1:num], digits=2)
}

{
  X11()
  m=min(correctcost_schz,correctcost_sani)
  M=max(correctcost_schz, correctcost_sani)
  boxplot(correctcost_sani, correctcost_schz, names = c("Control", "Schizophrenia"), ylim=c(m, M),ylab="Correctcost")
}

{x11(width =3.6)
  library(ggplot2)
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Correct.Cost=c(correctcost_sani,correctcost_schz)),aes(x=Diagnosis,y=Correct.Cost))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=17, face = "bold")) + xlab("") + ylab("") + labs(title = "Correct costs")
  
  pp
}

{
  x11()
  par(mfrow=c(1,2))
  M=max(max(table_correctcost_schz), max(table_correctcost_sani))
  barplot(table_correctcost_sani, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Correctcost", ylab="Density")
  barplot(table_correctcost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Correctcost", ylab="Density")
}

#___________________________________

{
  n1 <- length(correctcost_sani)[1] # n1=3
  n2 <- length(correctcost_schz)[1] # n2=4
  
  p=1
  
  correctcost_sani
  correctcost_schz
  
  correctcost_sani.mean <- mean(correctcost_sani, na.rm=T)
  correctcost_schz.mean <- mean(correctcost_schz, na.rm=T)
  correctcost_sani.cov  <-  var(correctcost_sani, na.rm=T)
  correctcost_schz.cov  <-  var(correctcost_schz, na.rm=T)
  Sp      <- ((n1-1)*correctcost_sani.cov + (n2-1)*correctcost_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=correctcost_sani.cov, S2=correctcost_schz.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (correctcost_sani.mean-correctcost_schz.mean-delta.0) %*% Spinv %*% (correctcost_sani.mean-correctcost_schz.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
# P-value=0.057

t.test(correctcost_sani, correctcost_schz)
graphics.off()
