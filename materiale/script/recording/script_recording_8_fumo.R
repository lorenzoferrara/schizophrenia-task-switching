
setwd("G:/Il mio Drive/Brain Connectivity")

###

setwd("./materiale/events recording")
load("../workspaces/fumo.RData")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_sani_fumo = c()
t_schz_fumo = c()
t_sani_nonfumo=c()
t_schz_nonfumo=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_fumo <- mean(s[current_smoking_bool==1,]$ReactionTime, na.rm=T)
  meantime_nonfumo <- mean(s[current_smoking_bool==0,]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_sani_fumo <- c(t_sani_fumo, meantime_fumo)
    t_sani_nonfumo <- c(t_sani_nonfumo, meantime_nonfumo)
  } else {
    t_schz_fumo <- c(t_schz_fumo, meantime_fumo)
    t_schz_nonfumo <- c(t_schz_nonfumo, meantime_nonfumo)
  }
  if (p$diagnosis[i]=="CONTROL"){
    t_sani_fumo <- c(t_sani_fumo, meantime_fumo)
    t_sani_nonfumo <- c(t_sani_nonfumo, meantime_nonfumo)
  } else {
    t_schz_fumo <- c(t_schz_fumo, meantime_fumo)
    t_schz_nonfumo <- c(t_schz_nonfumo, meantime_nonfumo)
  }
}

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_sani_fumo = rep(0,num)
  table_sani_nonfumo = rep(0,num)
  table_schz_fumo = rep(0,num)
  table_schz_nonfumo = rep(0,num)
  
  for (j in 1:length(t_sani_fumo)){
    for (i in 1:num){
      if(t_sani_fumo[j] > a[i] & t_sani_fumo[j] < a[i+1]){
        table_sani_fumo[i] = table_sani_fumo[i] + 1
      }
    }
  }
  table_sani_fumo=table_sani_fumo/length(t_sani_fumo)
  
  for (j in 1:length(t_sani_nonfumo)){
    for (i in 1:num){
      if(t_sani_nonfumo[j] > a[i] & t_sani_nonfumo[j] < a[i+1]){
        table_sani_nonfumo[i] = table_sani_nonfumo[i] + 1
      }
    }
  }
  table_sani_nonfumo=table_sani_nonfumo/length(t_sani_nonfumo)
  
  for (j in 1:length(t_schz_fumo)){
    for (i in 1:num){
      if(t_schz_fumo[j] > a[i] & t_schz_fumo[j]<a[i+1]){
        table_schz_fumo[i]= table_schz_fumo[i]+1
      }
    }
  }
  table_schz_fumo=table_schz_fumo/length(t_schz_fumo)
  
  for (j in 1:length(t_schz_nonfumo)){
    for (i in 1:num){
      if(t_schz_nonfumo[j] > a[i] & t_schz_nonfumo[j]<a[i+1]){
        table_schz_nonfumo[i]= table_schz_nonfumo[i]+1
      }
    }
  }
  table_schz_nonfumo=table_schz_nonfumo/length(t_schz_nonfumo)
  
  nomi=round(a[1:num], digits=1)
}


{
  X11()
  par(mfrow=c(2,4))
  boxplot(t_sani_fumo, xlab="Control fumo", ylim=c(0.4,1.4), ylab="Mean response time")
  boxplot(t_schz_fumo, xlab="Schz fumo", ylim=c(0.4,1.4),ylab="Mean response time")
  boxplot(t_sani_nonfumo, xlab="Control nonfumo", ylim=c(0.4,1.4), ylab="Mean response time")
  boxplot(t_schz_nonfumo, xlab="Schz nonfumo", ylim=c(0.4,1.4),ylab="Mean response time")
  
  barplot(table_sani_fumo, names.arg = nomi, ylim = c(0, 0.4), main="sani fumo")
  barplot(table_schz_fumo, names.arg = nomi, ylim = c(0, 0.4), main="schz fumo")
  barplot(table_sani_nonfumo, names.arg = nomi, ylim = c(0, 0.4), main="sani nonfumo")
  barplot(table_schz_nonfumo, names.arg = nomi, ylim = c(0, 0.4), main="schz nonfumo")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

{
  png(file = "MeanRT_fumo.png")
  color=c("lightskyblue1","tomato3")
  m=min(c(min(t_sani_fumo), min(t_schz_fumo), min(t_sani_nonfumo), min(t_schz_nonfumo)))
  M=max(c(max(t_sani_fumo), max(t_schz_fumo), max(t_sani_nonfumo), max(t_schz_nonfumo)))
  boxplot(t_sani_fumo,t_sani_nonfumo,t_schz_fumo,t_schz_nonfumo,names = c("Smorker","Smoker","No Smoker","No Smoker"), ylim = c(m, M), ylab = "Mean Reaction Time",col = color)
  legend("topright",col = color, c("Control","Schizophrenic"),lty =19,lwd=4)
  #boxplot(t_sani_fumo, xlab="Control fumo", ylim=c(m, M), ylab="Mean reaction time")
  #boxplot(t_sani_nonfumo, xlab="Control nonfumo", ylim=c(m,M), ylab="Mean reaction time")
  #boxplot(t_schz_fumo, xlab="Schz fumo", ylim=c(m,M),ylab="Mean reaction time")
  #boxplot(t_schz_nonfumo, xlab="Schz nonfumo", ylim=c(m,M),ylab="Mean reaction time")
  dev.off()
}

graphics.off()


