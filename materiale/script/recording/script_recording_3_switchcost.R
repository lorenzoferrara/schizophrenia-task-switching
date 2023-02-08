
library(gridExtra)
library(ggplot2)

setwd("C:/Users/lofer/OneDrive/Documenti/GitHub/Brain-Connectivity")
setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_control_switch = c()
t_schz_switch = c()
t_control_noswitch=c()
t_schz_noswitch=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.csv(val, sep="")
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_switch <- mean(s[s$Switching=="SWITCH",]$ReactionTime, na.rm=T)
  meantime_noswitch <- mean(s[s$Switching=="NOSWITCH",]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_control_switch <- c(t_control_switch, meantime_switch)
    t_control_noswitch <- c(t_control_noswitch, meantime_noswitch)
  } else {
    t_schz_switch <- c(t_schz_switch, meantime_switch)
    t_schz_noswitch <- c(t_schz_noswitch, meantime_noswitch)
  }
}

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_control_switch = rep(0,num)
  table_control_noswitch = rep(0,num)
  table_schz_switch = rep(0,num)
  table_schz_noswitch = rep(0,num)
  
  for (j in 1:length(t_control_switch)){
    for (i in 1:num){
      if(t_control_switch[j] > a[i] & t_control_switch[j] < a[i+1]){
        table_control_switch[i] = table_control_switch[i] + 1
      }
    }
  }
  table_control_switch=table_control_switch/length(t_control_switch)
  
  for (j in 1:length(t_control_noswitch)){
    for (i in 1:num){
      if(t_control_noswitch[j] > a[i] & t_control_noswitch[j] < a[i+1]){
        table_control_noswitch[i] = table_control_noswitch[i] + 1
      }
    }
  }
  table_control_noswitch=table_control_noswitch/length(t_control_noswitch)
  
  for (j in 1:length(t_schz_switch)){
    for (i in 1:num){
      if(t_schz_switch[j] > a[i] & t_schz_switch[j]<a[i+1]){
        table_schz_switch[i]= table_schz_switch[i]+1
      }
    }
  }
  table_schz_switch=table_schz_switch/length(t_schz_switch)
  
  for (j in 1:length(t_schz_noswitch)){
    for (i in 1:num){
      if(t_schz_noswitch[j] > a[i] & t_schz_noswitch[j]<a[i+1]){
        table_schz_noswitch[i]= table_schz_noswitch[i]+1
      }
    }
  }
  table_schz_noswitch=table_schz_noswitch/length(t_schz_noswitch)
  
  nomi=round(a[1:num], digits=1)
}


{
  x11( width = 11)
  group <-  factor(c(rep("Control_NoSwitch",125),rep("Control_Switch",125),rep("Schz_NoSwitch",50),rep("Schz_Switch",50)),levels = c("Control_NoSwitch", "Control_Switch","Schz_NoSwitch","Schz_Switch"))
  times <- c(t_control_noswitch, t_control_switch,t_schz_noswitch,t_schz_switch)
  
  p1 = ggplot(data.frame(g=group, Reaction.Time=times),aes(x=g,y=Reaction.Time))+
       geom_boxplot(aes(fill=g),width=0.5)+
       scale_fill_manual(values = c("royalblue","royalblue","orange2","orange2" ))+
       theme_minimal(base_size = 20)+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=20, face = "bold"))+
        xlab("") + ylab("") + labs(title="Reaction Time")
  p1
}

{
  x11()
  p2 = ggplot(data.frame(time=t_control_switch),aes(x=time))+
       geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="royalblue")+
       theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("Switch")+ylab("")
  p3 = ggplot(data.frame(time=t_schz_switch),aes(x=time))+
       geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="orange2")+
       theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("Switch")+ylab("")
  p4 = ggplot(data.frame(time=t_control_noswitch),aes(x=time))+
       geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="royalblue")+
       theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("No Switch")+ylab("")
  p5 = ggplot(data.frame(time=t_schz_noswitch),aes(x=time))+
       geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="orange2")+
       theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("No Switch")+ylab("")
  
  grid.arrange(p2,p3,p4,p5,nrow=1,top="Density of Reaction Time")
}

#############################################

switchcost_control = t_control_switch - t_control_noswitch
switchcost_schz = t_schz_switch - t_schz_noswitch

#creo le tabelle delle frequenze
{
  num=13
  M=max(max(switchcost_control), max(switchcost_schz))
  m=min(min(switchcost_control), min(switchcost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_switchcost_control = rep(0,num)
  table_switchcost_schz = rep(0,num)
  
  
  for (j in 1:length(switchcost_control)){
    for (i in 1:num){
      if(switchcost_control[j] > a[i] & switchcost_control[j] <= a[i+1]){
        table_switchcost_control[i] = table_switchcost_control[i] + 1
      }
    }
  }
  table_switchcost_control=table_switchcost_control/length(switchcost_control)
  
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

{x11()
  library(ggplot2)
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Switch.Cost=c(switchcost_control,switchcost_schz)),aes(x=Diagnosis,y=Switch.Cost))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=17, face = "bold")) + xlab("") + ylab("") + labs(title = "Switch costs")
  
  pp
}

{
  X11()
  p1 = ggplot(data.frame(Switch.Cost=switchcost_control),aes(x=Switch.Cost))+
    geom_histogram(aes(y = ..density..),breaks=a,colour="gray", fill="royalblue")+
    theme_minimal()+theme(legend.position = "")
  p2 = ggplot(data.frame(Switch.Cost=switchcost_schz),aes(x=Switch.Cost))+
    geom_histogram(aes(y = ..density..),breaks=a,colour="gray", fill="orange2")+
    theme_minimal()+theme(legend.position = "")
  grid.arrange(p1,p2,nrow=1,top="Density of Reaction Time")
}

#___________________________________
# TEST FOR THE DIFFERENCE
{
  n1 <- length(switchcost_control)[1] # n1=3
  n2 <- length(switchcost_schz)[1] # n2=4
  
  p=1
  
  switchcost_control.mean <- mean(switchcost_control, na.rm=T)
  switchcost_schz.mean <- mean(switchcost_schz, na.rm=T)
  switchcost_control.cov  <-  var(switchcost_control, na.rm=T)
  switchcost_schz.cov  <-  var(switchcost_schz, na.rm=T)
  Sp      <- ((n1-1)*switchcost_control.cov + (n2-1)*switchcost_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=switchcost_control.cov, S2=switchcost_schz.cov, Spooled=Sp)
  
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (switchcost_control.mean-switchcost_schz.mean-delta.0) %*% Spinv %*% (switchcost_control.mean-switchcost_schz.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  

var.test(switchcost_control, switchcost_schz)
t.test(switchcost_control, switchcost_schz)



