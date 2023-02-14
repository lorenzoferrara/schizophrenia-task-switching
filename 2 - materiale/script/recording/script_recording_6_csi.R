
setwd("C:/Users/lofer/OneDrive/Documenti/GitHub/Brain-Connectivity")
setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_control_CsiShort = c()
t_schz_CsiShort = c()
t_control_CsiLong=c()
t_schz_CsiLong=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.csv(val)
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_CsiShort <- mean(s[s$CSI=="SHORT",]$ReactionTime, na.rm=T)
  meantime_CsiLong <- mean(s[s$CSI=="LONG",]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_control_CsiShort <- c(t_control_CsiShort, meantime_CsiShort)
    t_control_CsiLong <- c(t_control_CsiLong, meantime_CsiLong)
  } else {
    t_schz_CsiShort <- c(t_schz_CsiShort, meantime_CsiShort)
    t_schz_CsiLong <- c(t_schz_CsiLong, meantime_CsiLong)
  }
}

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_control_CsiShort = rep(0,num)
  table_control_CsiLong = rep(0,num)
  table_schz_CsiShort = rep(0,num)
  table_schz_CsiLong = rep(0,num)
  
  for (j in 1:length(t_control_CsiShort)){
    for (i in 1:num){
      if(t_control_CsiShort[j] > a[i] & t_control_CsiShort[j] < a[i+1]){
        table_control_CsiShort[i] = table_control_CsiShort[i] + 1
      }
    }
  }
  table_control_CsiShort=table_control_CsiShort/length(t_control_CsiShort)
  
  for (j in 1:length(t_control_CsiLong)){
    for (i in 1:num){
      if(t_control_CsiLong[j] > a[i] & t_control_CsiLong[j] < a[i+1]){
        table_control_CsiLong[i] = table_control_CsiLong[i] + 1
      }
    }
  }
  table_control_CsiLong=table_control_CsiLong/length(t_control_CsiLong)
  
  for (j in 1:length(t_schz_CsiShort)){
    for (i in 1:num){
      if(t_schz_CsiShort[j] > a[i] & t_schz_CsiShort[j]<a[i+1]){
        table_schz_CsiShort[i]= table_schz_CsiShort[i]+1
      }
    }
  }
  table_schz_CsiShort=table_schz_CsiShort/length(t_schz_CsiShort)
  
  for (j in 1:length(t_schz_CsiLong)){
    for (i in 1:num){
      if(t_schz_CsiLong[j] > a[i] & t_schz_CsiLong[j]<a[i+1]){
        table_schz_CsiLong[i]= table_schz_CsiLong[i]+1
      }
    }
  }
  table_schz_CsiLong=table_schz_CsiLong/length(t_schz_CsiLong)
  
  nomi=round(a[1:num], digits=1)
}


{
  X11()
  par(mfrow=c(2,4))
  m=min(c(min(t_control_CsiShort), min(t_schz_CsiShort), min(t_control_CsiLong), min(t_schz_CsiLong)))
  M=max(c(max(t_control_CsiShort), max(t_schz_CsiShort), max(t_control_CsiLong), max(t_schz_CsiLong)))
  boxplot(t_control_CsiShort, xlab="Control CsiShort", ylim=c(m, M), ylab="Mean reaction time")
  boxplot(t_control_CsiLong, xlab="Control CsiLong", ylim=c(m,M), ylab="Mean reaction time")
  boxplot(t_schz_CsiShort, xlab="Schz CsiShort", ylim=c(m,M),ylab="Mean reaction time")
  boxplot(t_schz_CsiLong, xlab="Schz CsiLong", ylim=c(m,M),ylab="Mean reaction time")
  
  barplot(table_control_CsiShort, names.arg = nomi, ylim = c(0, 0.4), main="control CsiShort")
  barplot(table_schz_CsiShort, names.arg = nomi, ylim = c(0, 0.4), main="schz CsiShort")
  barplot(table_control_CsiLong, names.arg = nomi, ylim = c(0, 0.4), main="control CsiLong")
  barplot(table_schz_CsiLong, names.arg = nomi, ylim = c(0, 0.4), main="schz CsiLong")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

{
  X11()
  m=min(c(min(t_control_CsiShort), min(t_schz_CsiShort), min(t_control_CsiLong), min(t_schz_CsiLong)))
  M=max(c(max(t_control_CsiShort), max(t_schz_CsiShort), max(t_control_CsiLong), max(t_schz_CsiLong)))
  par(mfrow=c(1,4))
  boxplot(t_control_CsiShort, xlab="Control CsiShort", ylim=c(m, M), ylab="Mean reaction time")
  boxplot(t_control_CsiLong, xlab="Control CsiLong", ylim=c(m,M), ylab="Mean reaction time")
  boxplot(t_schz_CsiShort, xlab="Schz CsiShort", ylim=c(m,M),ylab="Mean reaction time")
  boxplot(t_schz_CsiLong, xlab="Schz CsiLong", ylim=c(m,M),ylab="Mean reaction time")
}


csicost_control <- t_control_CsiShort - t_control_CsiLong
csicost_schz <- t_schz_CsiShort - t_schz_CsiLong
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(csicost_control), max(csicost_schz))
  m=min(min(csicost_control), min(csicost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_csicost_control = rep(0,num)
  table_csicost_schz = rep(0,num)
  
  for (j in 1:length(csicost_control)){
    for (i in 1:num){
      if(csicost_control[j] > a[i] & csicost_control[j] <= a[i+1]){
        table_csicost_control[i] = table_csicost_control[i] + 1
      }
    }
  }
  table_csicost_control=table_csicost_control/length(csicost_control)
  
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
  m=min(min(csicost_control), min(csicost_schz))
  M=max(max(csicost_control), max(csicost_schz))
  boxplot(csicost_control, xlab="Control", ylim=c(m, M), ylab="Csicost")
  boxplot(csicost_schz, xlab="Schizophrenia", ylim=c(m, M),ylab="Csicost")
  
  M=max(max(table_csicost_schz), max(table_csicost_control))+0.04
  barplot(table_csicost_control, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Csicost", ylab="Density")
  barplot(table_csicost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Csicost", ylab="Density")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}


#___________________________________
{
  n1 <- length(csicost_control)[1] # n1=3
  n2 <- length(csicost_schz)[1] # n2=4
  
  p=1
  
  csicost_control
  csicost_schz
  
  csicost_control.mean <- mean(csicost_control, na.rm=T)
  csicost_schz.mean <- mean(csicost_schz, na.rm=T)
  csicost_control.cov  <-  var(csicost_control, na.rm=T)
  csicost_schz.cov  <-  var(csicost_schz, na.rm=T)
  Sp      <- ((n1-1)*csicost_control.cov + (n2-1)*csicost_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=csicost_control.cov, S2=csicost_schz.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (csicost_control.mean-csicost_schz.mean-delta.0) %*% Spinv %*% (csicost_control.mean-csicost_schz.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
# P-value=0.64

graphics.off()


