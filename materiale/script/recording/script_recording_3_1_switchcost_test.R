
setwd("G:/Il mio Drive/Brain Connectivity")

setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_sani_switch = c()
t_schz_switch = c()
t_sani_noswitch=c()
t_schz_noswitch=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_switch <- mean(s[s$Switching=="SWITCH",]$ReactionTime, na.rm=T)
  meantime_noswitch <- mean(s[s$Switching=="NOSWITCH",]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_sani_switch <- c(t_sani_switch, meantime_switch)
    t_sani_noswitch <- c(t_sani_noswitch, meantime_noswitch)
  } else {
    t_schz_switch <- c(t_schz_switch, meantime_switch)
    t_schz_noswitch <- c(t_schz_noswitch, meantime_noswitch)
  }
}

switchcost_sani = t_sani_switch - t_sani_noswitch
switchcost_schz = t_schz_switch - t_schz_noswitch

#creo le tabelle delle frequenze
{
  num=13
  M=max(max(switchcost_sani), max(switchcost_schz))
  m=min(min(switchcost_sani), min(switchcost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_switchcost_sani = rep(0,num)
  table_switchcost_schz = rep(0,num)
  
  
  for (j in 1:length(switchcost_sani)){
    for (i in 1:num){
      if(switchcost_sani[j] > a[i] & switchcost_sani[j] <= a[i+1]){
        table_switchcost_sani[i] = table_switchcost_sani[i] + 1
      }
    }
  }
  table_switchcost_sani=table_switchcost_sani/length(switchcost_sani)
  
  for (j in 1:length(switchcost_schz)){
    for (i in 1:num){
      if(switchcost_schz[j] > a[i] & switchcost_schz[j] <= a[i+1]){
        table_switchcost_schz[i] = table_switchcost_schz[i] + 1
      }
    }
  }
  table_switchcost_schz=table_switchcost_schz/length(switchcost_schz)
  
  nomi=round(a[1:num], digits=1)
}


{
  X11()
  m=min(switchcost_sani, switchcost_schz)
  M=max(switchcost_sani, switchcost_schz)
  boxplot(switchcost_sani, switchcost_schz, names = c("Control","Schizophrenia"), ylim=c(m, M),ylab="Switchcost")
}

{x11()
library(ggplot2)
pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Switch.Cost=c(switchcost_sani,switchcost_schz)),aes(x=Diagnosis,y=Switch.Cost))+
  geom_boxplot(aes(fill=Diagnosis),width=0.5)+
  scale_fill_manual(values = c("royalblue","orange2"))+
  theme_minimal()+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=17, face = "bold")) + xlab("") + ylab("") + labs(title = "Switch costs")

pp
}


{
  X11()
  par(mfrow=c(2,2))
  m=min(switchcost_sani, switchcost_schz)
  M=max(switchcost_sani, switchcost_schz)
  boxplot(switchcost_sani, switchcost_schz, names = c("Control","Schizophrenia"), ylim=c(m, M),ylab="Switchcost")
  
  M=max(max(table_switchcost_sani), max(table_switchcost_schz))
  barplot(table_switchcost_sani, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Switchcost", ylab="Density")
  barplot(table_switchcost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Switchcost", ylab="Density")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

p1 = ggplot(data.frame(Switch.Cost=switchcost_sani),aes(x=Switch.Cost))+
  geom_histogram(aes(y = ..density..),breaks=a,colour="gray", fill="royalblue")+
  theme_minimal()+theme(legend.position = "")
p2 = ggplot(data.frame(Switch.Cost=switchcost_schz),aes(x=Switch.Cost))+
  geom_histogram(aes(y = ..density..),breaks=a,colour="gray", fill="orange2")+
  theme_minimal()+theme(legend.position = "")

grid.arrange(p1,p2,nrow=1,top="Density of Reaction Time")


graphics.off()

#___________________________________

{
n1 <- length(switchcost_sani)[1] # n1=3
n2 <- length(switchcost_schz)[1] # n2=4

p=1

switchcost_sani.mean <- mean(switchcost_sani, na.rm=T)
switchcost_schz.mean <- mean(switchcost_schz, na.rm=T)
switchcost_sani.cov  <-  var(switchcost_sani, na.rm=T)
switchcost_schz.cov  <-  var(switchcost_schz, na.rm=T)
Sp      <- ((n1-1)*switchcost_sani.cov + (n2-1)*switchcost_schz.cov)/(n1+n2-2)
# we compare the matrices
list(S1=switchcost_sani.cov, S2=switchcost_schz.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

library(MASS)
alpha   <- .05
delta.0 <- rep(0,p) #tutti zero
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (switchcost_sani.mean-switchcost_schz.mean-delta.0) %*% Spinv %*% (switchcost_sani.mean-switchcost_schz.mean-delta.0)

cfr.chisq <- qchisq(1-alpha,p)
T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  

var.test(switchcost_sani, switchcost_schz)
t.test(switchcost_sani, switchcost_schz)

