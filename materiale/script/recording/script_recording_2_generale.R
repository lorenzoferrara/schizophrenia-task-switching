##    Comparison of REACTION TIME between control and SCHIZOPHRENIC

setwd("./materiale/SubjData")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_control = c()
t_schz = c()
#CARICO UNO AD UNO I RECORDINGS DEI SOGGETTI
for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.csv(val, sep="")

  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime <- mean(s$ReactionTime, na.rm=T)
  if (p$diagnosis[i]=="CONTROL"){
    t_control <- c(t_control, meantime)
  } else {
    t_schz <- c(t_schz, meantime)
  }
}

#creo le tabelle delle frequenze
{
  num=13
  M=max(max(t_schz), max(t_control))
  m=min(min(t_schz), min(t_control))
  a <- seq(m, M, (M-m)/(num))
  table_control= rep(0,num)
  table_schz= rep(0,num)
  for (j in 1:length(t_control)){
    for (i in 1:num){
      if(t_control[j] > a[i] & t_control[j] < a[i+1]){
        table_control[i] = table_control[i] + 1
      }
    }
  }
  table_control=table_control/length(t_control)
  
  for (j in 1:length(t_schz)){
    for (i in 1:num){
      if(t_schz[j] > a[i] & t_schz[j]<a[i+1]){
        table_schz[i]= table_schz[i]+1
      }
    }
  }
  table_schz=table_schz/length(t_schz)
  
  nomi=round(a[1:num], digits=1)
}


{
  X11()
  par(mfrow=c(2,2))
  M=max(max(t_control), max(t_schz))
  m=min(min(t_control), min(t_schz))
  boxplot(t_control, xlab="Control", ylim=c(m, M), ylab="Reaction Time")
  boxplot(t_schz, xlab="Schizophrenia", ylim=c(m, M),ylab="Reaction Time")
  
  M=max(max(table_control), max(table_schz))+0.05
  barplot(table_control, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Reaction Time", ylab="Density")
  barplot(table_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Reaction Time", ylab="Density")
  
  #mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

library(ggplot2)
{x11(width=4)
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Times = c(t_control, t_schz)),aes(x=Diagnosis,y=Times))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=20, face = "bold")) +
    xlab("") + ylab("") + labs(title="Reaction Time")
  pp
}

#METTO I BOX??????
times=c(t_control, t_schz)
#___________________________________

{
  n1 <- length(t_control)[1] # n1=3
  n2 <- length(t_schz)[1] # n2=4
  
  p=1
  
  t_control.mean <- mean(t_control, na.rm=T)
  t_schz.mean <- mean(t_schz, na.rm=T)
  t_control.cov  <-  var(t_control, na.rm=T)
  t_schz.cov  <-  var(t_schz, na.rm=T)
  Sp      <- ((n1-1)*t_control.cov + (n2-1)*t_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=t_control.cov, S2=t_schz.cov, Spooled=Sp)
  
  library(MASS)
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (t_control.mean-t_schz.mean-delta.0) %*% Spinv %*% (t_control.mean-t_schz.mean-delta.0)
  
  cfr.chisq <- (p*(n1+n2-2)/(n1+n2-1-p))*qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  

shapiro.test(t_control)
shapiro.test(t_schz)

var.test(t_control, t_schz)
t.test(t_control, t_schz, alternative = "less", var.equal = T)
# Confidence interval
IC.differenza_tempi <- c(t_control.mean[1]-t_schz.mean[1]-sqrt(cfr.chisq*Sp*(1/n1+1/n2)),t_control.mean[1]-t_schz.mean[1], t_control.mean[1]-t_schz.mean[1]+sqrt(cfr.chisq*Sp*(1/n1+1/n2)) )
c('inf','mid','sup')                        
-IC.differenza_tempi


