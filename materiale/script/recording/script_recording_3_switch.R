
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

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_sani_switch = rep(0,num)
  table_sani_noswitch = rep(0,num)
  table_schz_switch = rep(0,num)
  table_schz_noswitch = rep(0,num)
  
  for (j in 1:length(t_sani_switch)){
    for (i in 1:num){
      if(t_sani_switch[j] > a[i] & t_sani_switch[j] < a[i+1]){
        table_sani_switch[i] = table_sani_switch[i] + 1
      }
    }
  }
  table_sani_switch=table_sani_switch/length(t_sani_switch)
  
  for (j in 1:length(t_sani_noswitch)){
    for (i in 1:num){
      if(t_sani_noswitch[j] > a[i] & t_sani_noswitch[j] < a[i+1]){
        table_sani_noswitch[i] = table_sani_noswitch[i] + 1
      }
    }
  }
  table_sani_noswitch=table_sani_noswitch/length(t_sani_noswitch)
  
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


{  X11()
  par(mfrow=c(2,4))
  boxplot(t_sani_switch, xlab="sani switch", ylim=c(0.4,1.4), ylab="mean time", col="yellow")
  boxplot(t_schz_switch, xlab="schz switch", ylim=c(0.4,1.4),ylab="mean time", col="yellow")
  boxplot(t_sani_noswitch, xlab="sani noswitch", ylim=c(0.4,1.4), ylab="mean time", col="red")
  boxplot(t_schz_noswitch, xlab="schz noswitch", ylim=c(0.4,1.4),ylab="mean time", col="red")
  
  barplot(table_sani_switch, names.arg = nomi, ylim = c(0, 0.4), main="sani switch")
  barplot(table_schz_switch, names.arg = nomi, ylim = c(0, 0.4), main="schz switch")
  barplot(table_sani_noswitch, names.arg = nomi, ylim = c(0, 0.4), main="sani noswitch")
  barplot(table_schz_noswitch, names.arg = nomi, ylim = c(0, 0.4), main="schz noswitch")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

library(gridExtra)
library(ggplot2)
{x11( width = 11)
group <-  factor(c(rep("Control_NoSwitch",125),rep("Control_Switch",125),rep("Schz_NoSwitch",50),rep("Schz_Switch",50)),levels = c("Control_NoSwitch", "Control_Switch","Schz_NoSwitch","Schz_Switch"))
times <- c(t_sani_noswitch, t_sani_switch,t_schz_noswitch,t_schz_switch)

p1 = ggplot(data.frame(g=group, Reaction.Time=times),aes(x=g,y=Reaction.Time))+
     geom_boxplot(aes(fill=g),width=0.5)+
     scale_fill_manual(values = c("royalblue","royalblue","orange2","orange2" ))+
     theme_minimal(base_size = 20)+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=20, face = "bold"))+
      xlab("") + ylab("") + labs(title="Reaction Time")
p1
}

{
p2 = ggplot(data.frame(time=t_sani_switch),aes(x=time))+
     geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="royalblue")+
     theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("Switch")+ylab("")
p3 = ggplot(data.frame(time=t_schz_switch),aes(x=time))+
     geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="orange2")+
     theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("Switch")+ylab("")
p4 = ggplot(data.frame(time=t_sani_noswitch),aes(x=time))+
     geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="royalblue")+
     theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("No Switch")+ylab("")
p5 = ggplot(data.frame(time=t_schz_noswitch),aes(x=time))+
     geom_histogram(aes(y = ..density..),breaks=seq(0,1.5,0.1),colour="gray", fill="orange2")+
     theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlab("No Switch")+ylab("")

grid.arrange(p2,p3,p4,p5,nrow=1,top="Density of Reaction Time")
}


{
  X11(width=300, height=200)
  m=min(c(min(t_sani_switch), min(t_sani_noswitch), min(t_schz_switch), min(t_schz_noswitch)))
  M=max(c(max(t_sani_switch), max(t_sani_noswitch), max(t_schz_switch), max(t_schz_noswitch)))
  par(mfrow=c(1,4))
  boxplot(t_sani_switch, xlab="Control switch", ylim=c(m, M), ylab="Mean response time", cex=15)
  boxplot(t_sani_noswitch, xlab="Control noswitch", ylim=c(m,M), ylab="Mean response time", cex=5)
  boxplot(t_schz_switch, xlab="Schz switch", ylim=c(m,M),ylab="Mean response time", cex=1)
  boxplot(t_schz_noswitch, xlab="Schz noswitch", ylim=c(m,M),ylab="Mean response time", cex=5)
  mtext("Representation of ReactionTime", side=3, line=-3, outer=T)
}

graphics.off()


