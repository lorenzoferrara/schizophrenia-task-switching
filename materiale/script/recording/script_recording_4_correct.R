
setwd("G:/Il mio Drive/Brain Connectivity/materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_sani_correct = c()
t_schz_correct = c()
t_sani_noncorrect=c()
t_schz_noncorrect=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_correct <- mean(s[s$CorrectResp ==1,]$ReactionTime, na.rm=T)
  meantime_noncorrect <- mean(s[s$CorrectResp==0,]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_sani_correct <- c(t_sani_correct, meantime_correct)
    if(is.na(meantime_noncorrect)){
    t_sani_noncorrect <- c(t_sani_noncorrect, 0)
    } else{
      t_sani_noncorrect <- c(t_sani_noncorrect, meantime_noncorrect)
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

#t_sani_correct = t_sani_correct[t_sani_noncorrect!=0]
#t_sani_noncorrect = t_sani_noncorrect[t_sani_noncorrect!=0]

# t_schz_correct = t_schz_correct[t_schz_noncorrect!=0]
# t_schz_noncorrect = t_schz_noncorrect[t_schz_noncorrect!=0]

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_sani_correct = rep(0,num)
  table_sani_noncorrect = rep(0,num)
  table_schz_correct = rep(0,num)
  table_schz_noncorrect = rep(0,num)
  
  for (j in 1:length(t_sani_correct)){
    for (i in 1:num){
      if(t_sani_correct[j] >= a[i] & t_sani_correct[j] < a[i+1]){
        table_sani_correct[i] = table_sani_correct[i] + 1
      }
    }
  }
  table_sani_correct=table_sani_correct/length(t_sani_correct)
  
  for (j in 1:length(t_sani_noncorrect)){
    for (i in 1:num){
      if(t_sani_noncorrect[j] >= a[i] & t_sani_noncorrect[j] < a[i+1]){
        table_sani_noncorrect[i] = table_sani_noncorrect[i] + 1
      }
    }
  }
  table_sani_noncorrect=table_sani_noncorrect/length(t_sani_noncorrect)
  
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

{
  X11()
  par(mfrow=c(2,4))
  boxplot(t_sani_correct, xlab="sani correct", ylim=c(0.4,1.4), ylab="mean time")
  boxplot(t_schz_correct, xlab="schz correct", ylim=c(0.4,1.4),ylab="mean time")
  boxplot(t_sani_noncorrect, xlab="sani noncorrect", ylim=c(0.4,1.4), ylab="mean time")
  boxplot(t_schz_noncorrect, xlab="schz noncorrect", ylim=c(0.4,1.4),ylab="mean time")
  
  barplot(table_sani_correct, names.arg = nomi, ylim = c(0, 0.4), main="sani correct")
  barplot(table_schz_correct, names.arg = nomi, ylim = c(0, 0.4), main="schz correct")
  barplot(table_sani_noncorrect, names.arg = nomi, ylim = c(0, 0.4), main="sani noncorrect")
  barplot(table_schz_noncorrect, names.arg = nomi, ylim = c(0, 0.4), main="schz noncorrect")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

library(gridExtra)
library(ggplot2)

{x11(width=10)
group <-  factor(c(rep("Control_Correct",125),rep("Control_NonCorrect",125),rep("Schz_Correct",50),rep("Schz_NonCorrect",50)),levels = c("Control_Correct","Control_NonCorrect","Schz_Correct","Schz_NonCorrect"))
times <- c(t_sani_correct,t_sani_noncorrect,t_schz_correct,t_schz_noncorrect)


p1 = ggplot(data.frame(g=group, Reaction.Time=times),aes(x=g,y=Reaction.Time))+
  geom_boxplot(aes(fill=g),width=0.5)+
  scale_fill_manual(values = c("royalblue","royalblue","orange2","orange2" ))+
  theme_minimal(base_size = 17)+
  theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=20, face = "bold")) +xlab("") +
  ylab("") + labs(title="Reaction Time")
p1
}

{x11()
  group <-  factor(c(rep("Control_Switch",125),rep("Control_NoSwitch",125),rep("Schz_Switch",50),rep("Schz_NoSwitch",50)),levels = c("Control_Switch","Control_NoSwitch","Schz_Switch","Schz_NoSwitch"))
  times <- c(t_sani_switch,t_sani_noswitch,t_schz_switch, t_schz_noswitch)
  
  p1 = ggplot(data.frame(g=group, Reaction.Time=times),aes(x=g,y=Reaction.Time))+
    geom_boxplot(aes(fill=g),width=0.5)+
    scale_fill_manual(values = c("royalblue","royalblue","orange2","orange2" ))+
    theme_minimal(base_size = 20)+theme(legend.position = "")  +xlab("")
  p1
}

#QUELLO DI LOLLO è IL PRIMO, IL SECONDO è DI ERICA, LOLLO PREFERISCE QUELLO DI LOLLO
maximum = 3.2
{x11()
  p2 = ggplot(data.frame(time=t_sani_correct),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="royalblue")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Correct")+ylab("")+ylim(0, maximum)
  p3 = ggplot(data.frame(time=t_schz_correct),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="orange2")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Correct")+ylab("")+ylim(0, maximum)
  p4 = ggplot(data.frame(time=t_sani_noncorrect),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="royalblue")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Non Correct")+ylab("")+ylim(0, maximum)
  p5 = ggplot(data.frame(time=t_schz_noncorrect),aes(x=time))+
    geom_histogram(aes(y = ..density..),breaks = a,colour="gray", fill="orange2")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Non Correct")+ylab("")+ylim(0, maximum)
  
  grid.arrange(p2,p3,p4,p5,nrow=1,top="Density of Reaction Time")
}

{x11()
  p2 = ggplot(data.frame(time=t_sani_correct),aes(x=time))+
    geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="royalblue")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Correct")+ylab("")+ylim(0, 3.1)
  p3 = ggplot(data.frame(time=t_schz_correct),aes(x=time))+
    geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="orange2")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Correct")+ylab("")+ylim(0, 3.1)
  p4 = ggplot(data.frame(time=t_sani_noncorrect),aes(x=time))+
    geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="royalblue")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Non Correct")+ylab("")+ylim(0, 3.1)
  p5 = ggplot(data.frame(time=t_schz_noncorrect),aes(x=time))+
    geom_histogram(aes(y = ..density..),binwidth = 0.1,colour="gray", fill="orange2")+
    theme_minimal(base_size = 15)+theme(legend.position = "")+xlab("Non Correct")+ylab("")+ylim(0, 3.1)
  
  grid.arrange(p2,p3,p4,p5,nrow=1,top="Density of Reaction Time")
}
graphics.off()

