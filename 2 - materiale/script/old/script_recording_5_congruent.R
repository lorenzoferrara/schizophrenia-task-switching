
setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_sani_congruent = c()
t_schz_congruent = c()
t_sani_incongruent = c()
t_schz_incongruent = c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_congruent <- mean(s[s$Congruency=="CONGRUENT",]$ReactionTime, na.rm=T)
  meantime_incongruent <- mean(s[s$Congruency=="INCONGRUENT",]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_sani_congruent <- c(t_sani_congruent, meantime_congruent)
    t_sani_incongruent <- c(t_sani_incongruent, meantime_incongruent)
  } else {
    t_schz_congruent <- c(t_schz_congruent, meantime_congruent)
    t_schz_incongruent <- c(t_schz_incongruent, meantime_incongruent)
  }
}

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_sani_congruent = rep(0,num)
  table_sani_incongruent = rep(0,num)
  table_schz_congruent = rep(0,num)
  table_schz_incongruent = rep(0,num)
  
  for (j in 1:length(t_sani_congruent)){
    for (i in 1:num){
      if(t_sani_congruent[j] > a[i] & t_sani_congruent[j] < a[i+1]){
        table_sani_congruent[i] = table_sani_congruent[i] + 1
      }
    }
  }
  table_sani_congruent=table_sani_congruent/length(t_sani_congruent)
  
  for (j in 1:length(t_sani_incongruent)){
    for (i in 1:num){
      if(t_sani_incongruent[j] > a[i] & t_sani_incongruent[j] < a[i+1]){
        table_sani_incongruent[i] = table_sani_incongruent[i] + 1
      }
    }
  }
  table_sani_incongruent=table_sani_incongruent/length(t_sani_incongruent)
  
  for (j in 1:length(t_schz_congruent)){
    for (i in 1:num){
      if(t_schz_congruent[j] > a[i] & t_schz_congruent[j]<a[i+1]){
        table_schz_congruent[i]= table_schz_congruent[i]+1
      }
    }
  }
  table_schz_congruent=table_schz_congruent/length(t_schz_congruent)
  
  for (j in 1:length(t_schz_incongruent)){
    for (i in 1:num){
      if(t_schz_incongruent[j] > a[i] & t_schz_incongruent[j]<a[i+1]){
        table_schz_incongruent[i]= table_schz_incongruent[i]+1
      }
    }
  }
  table_schz_incongruent=table_schz_incongruent/length(t_schz_incongruent)
  
  nomi=round(a[1:num], digits=1)
}


{
  X11()
  m=min(c(min(t_sani_congruent), min(t_schz_congruent), min(t_sani_incongruent), min(t_schz_incongruent)))
  M=max(c(max(t_sani_congruent), max(t_schz_congruent), max(t_sani_incongruent), max(t_schz_incongruent)))
  par(mfrow=c(1,4))
  boxplot(t_sani_congruent, xlab="Control Congruent", ylim=c(m, M), ylab="Mean reaction time")
  boxplot(t_sani_incongruent, xlab="Control Incongruent", ylim=c(m,M), ylab="Mean reaction time")
  boxplot(t_schz_congruent, xlab="Schz Congruent", ylim=c(m,M),ylab="Mean reaction time")
  boxplot(t_schz_incongruent, xlab="Schz Incongruent", ylim=c(m,M),ylab="Mean reaction time")
  mtext("Representation of ReactionTime", side=3, line=-3, outer=T)
}

library(gridExtra)
library(ggplot2)

group <-  factor(c(rep("Control_Congruent",125),rep("Schz_Congruent",50),rep("Control_Incongruent",125),rep("Schz_Incongruent",50)),levels = c("Control_Congruent","Schz_Congruent","Control_Incongruent","Schz_Incongruent"))
times <- c(t_sani_congruent,t_schz_congruent,t_sani_incongruent,t_schz_incongruent)

p1 = ggplot(data.frame(g=group, Reaction.Time=times),aes(x=g,y=Reaction.Time))+
  geom_boxplot(aes(fill=g),width=0.5)+
  scale_fill_manual(values = c("royalblue","orange2","royalblue","orange2" ))+
  theme_minimal(base_size = 20)+theme(legend.position = "")
p1
p2 = ggplot(data.frame(time=t_sani_congruent),aes(x=time))+
  geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="royalblue")+
  theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlim(0,1.5)+xlab("Congruent")+ylab("")
p3 = ggplot(data.frame(time=t_schz_congruent),aes(x=time))+
  geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="orange2")+
  theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlim(0,1.5)+xlab("Congruent")+ylab("")
p4 = ggplot(data.frame(time=t_sani_incongruent),aes(x=time))+
  geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="royalblue")+
  theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlim(0,1.5)+xlab("Incongruent")+ylab("")
p5 = ggplot(data.frame(time=t_schz_incongruent),aes(x=time))+
  geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="orange2")+
  theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,2.5)+xlim(0,1.5)+xlab("Incongruent")+ylab("")

grid.arrange(p2,p3,p4,p5,nrow=1,top="Density of Reaction Time")


graphics.off()

