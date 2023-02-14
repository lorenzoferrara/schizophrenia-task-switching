
setwd("C:/Users/lofer/OneDrive/Documenti/GitHub/Brain-Connectivity")
setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_control_correct = c()
t_schz_correct = c()
t_control_noncorrect=c()
t_schz_noncorrect=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.csv(val, sep="")
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_correct <- mean(s[s$CorrectResp ==1,]$ReactionTime, na.rm=T)
  meantime_noncorrect <- mean(s[s$CorrectResp==0,]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_control_correct <- c(t_control_correct, meantime_correct)
    if(is.na(meantime_noncorrect)){
    t_control_noncorrect <- c(t_control_noncorrect, 0)
    } else{
      t_control_noncorrect <- c(t_control_noncorrect, meantime_noncorrect)
    }
  } 
  else {
    t_schz_correct <- c(t_schz_correct, meantime_correct)
    if(is.na(meantime_noncorrect)){
      t_schz_noncorrect <- c(t_schz_noncorrect, 0)
    } else{
      t_schz_noncorrect <- c(t_schz_noncorrect, meantime_noncorrect)
    }
  }
}

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_control_correct = rep(0,num)
  table_control_noncorrect = rep(0,num)
  table_schz_correct = rep(0,num)
  table_schz_noncorrect = rep(0,num)
  
  for (j in 1:length(t_control_correct)){
    for (i in 1:num){
      if(t_control_correct[j] >= a[i] & t_control_correct[j] < a[i+1]){
        table_control_correct[i] = table_control_correct[i] + 1
      }
    }
  }
  table_control_correct=table_control_correct/length(t_control_correct)
  
  for (j in 1:length(t_control_noncorrect)){
    for (i in 1:num){
      if(t_control_noncorrect[j] >= a[i] & t_control_noncorrect[j] < a[i+1]){
        table_control_noncorrect[i] = table_control_noncorrect[i] + 1
      }
    }
  }
  table_control_noncorrect=table_control_noncorrect/length(t_control_noncorrect)
  
  for (j in 1:length(t_schz_correct)){
    for (i in 1:num){
      if(t_schz_correct[j] >= a[i] & t_schz_correct[j]<a[i+1]){
        table_schz_correct[i]= table_schz_correct[i]+1
      }
    }
  }
  table_schz_correct=table_schz_correct/length(t_schz_correct)
  
  for (j in 1:length(t_schz_noncorrect)){
    for (i in 1:num){
      if(t_schz_noncorrect[j] >= a[i] & t_schz_noncorrect[j]<a[i+1]){
        table_schz_noncorrect[i]= table_schz_noncorrect[i]+1
      }
    }
  }
  table_schz_noncorrect=table_schz_noncorrect/length(t_schz_noncorrect)
  
  nomi=round(a[1:num], digits=1)
}

library(gridExtra)
library(ggplot2)

{x11(width=10)
group <-  factor(c(rep("Control_Correct",125),rep("Control_NonCorrect",125),rep("Schz_Correct",50),rep("Schz_NonCorrect",50)),levels = c("Control_Correct","Control_NonCorrect","Schz_Correct","Schz_NonCorrect"))
times <- c(t_control_correct,t_control_noncorrect,t_schz_correct,t_schz_noncorrect)

p1 = ggplot(data.frame(g=group, Reaction.Time=times),aes(x=g,y=Reaction.Time))+
  geom_boxplot(aes(fill=g),width=0.5)+
  scale_fill_manual(values = c("royalblue","royalblue","orange2","orange2" ))+
  theme_minimal(base_size = 17)+
  theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=20, face = "bold")) +xlab("") +
  ylab("") + labs(title="Reaction Time")
p1
}

maximum = 3.2
{x11()
  p2 = ggplot(data.frame(time=t_control_correct),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="royalblue")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Correct")+ylab("")+ylim(0, maximum)
  p3 = ggplot(data.frame(time=t_schz_correct),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="orange2")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Correct")+ylab("")+ylim(0, maximum)
  p4 = ggplot(data.frame(time=t_control_noncorrect),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="royalblue")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Non Correct")+ylab("")+ylim(0, maximum)
  p5 = ggplot(data.frame(time=t_schz_noncorrect),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="orange2")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Non Correct")+ylab("")+ylim(0, maximum)
  
  grid.arrange(p2,p3,p4,p5,nrow=1,top="Density of Reaction Time")
}


correctcost_control <- t_control_correct - t_control_noncorrect
correctcost_schz <- t_schz_correct - t_schz_noncorrect
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(correctcost_control), max(correctcost_schz))
  m=min(min(correctcost_control), min(correctcost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_correctcost_control = rep(0,num)
  table_correctcost_schz = rep(0,num)
  
  for (j in 1:length(correctcost_control)){
    for (i in 1:num){
      if(correctcost_control[j] > a[i] & correctcost_control[j] <= a[i+1]){
        table_correctcost_control[i] = table_correctcost_control[i] + 1
      }
    }
  }
  table_correctcost_control=table_correctcost_control/length(correctcost_control)
  
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

{x11(width =3.6)
  library(ggplot2)
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Correct.Cost=c(correctcost_control,correctcost_schz)),aes(x=Diagnosis,y=Correct.Cost))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=17, face = "bold")) + xlab("") + ylab("") + labs(title = "Correct costs")
  
  pp
}

{
  x11()
  par(mfrow=c(1,2))
  M=max(max(table_correctcost_schz), max(table_correctcost_control))
  barplot(table_correctcost_control, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Correctcost", ylab="Density")
  barplot(table_correctcost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Correctcost", ylab="Density")
}

#___________________________________

{
  n1 <- length(correctcost_control)[1] # n1=3
  n2 <- length(correctcost_schz)[1] # n2=4
  
  p=1
  
  correctcost_control
  correctcost_schz
  
  correctcost_control.mean <- mean(correctcost_control, na.rm=T)
  correctcost_schz.mean <- mean(correctcost_schz, na.rm=T)
  correctcost_control.cov  <-  var(correctcost_control, na.rm=T)
  correctcost_schz.cov  <-  var(correctcost_schz, na.rm=T)
  Sp      <- ((n1-1)*correctcost_control.cov + (n2-1)*correctcost_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=correctcost_control.cov, S2=correctcost_schz.cov, Spooled=Sp)

  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (correctcost_control.mean-correctcost_schz.mean-delta.0) %*% Spinv %*% (correctcost_control.mean-correctcost_schz.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
# P-value=0.057

t.test(correctcost_control, correctcost_schz)
graphics.off()

