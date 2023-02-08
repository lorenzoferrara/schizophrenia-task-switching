
setwd("G:/Il mio Drive/Brain Connectivity/materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_sani_CsiShort = c()
t_schz_CsiShort = c()
t_sani_CsiLong=c()
t_schz_CsiLong=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_CsiShort <- mean(s[s$CSI=="SHORT",]$ReactionTime, na.rm=T)
  meantime_CsiLong <- mean(s[s$CSI=="LONG",]$ReactionTime, na.rm=T) 
  
  if (p$diagnosis[i]=="CONTROL"){
    t_sani_CsiShort <- c(t_sani_CsiShort, meantime_CsiShort)
    t_sani_CsiLong <- c(t_sani_CsiLong, meantime_CsiLong)
  } else {
    t_schz_CsiShort <- c(t_schz_CsiShort, meantime_CsiShort)
    t_schz_CsiLong <- c(t_schz_CsiLong, meantime_CsiLong)
  }
}

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_sani_CsiShort = rep(0,num)
  table_sani_CsiLong = rep(0,num)
  table_schz_CsiShort = rep(0,num)
  table_schz_CsiLong = rep(0,num)
  
  for (j in 1:length(t_sani_CsiShort)){
    for (i in 1:num){
      if(t_sani_CsiShort[j] > a[i] & t_sani_CsiShort[j] < a[i+1]){
        table_sani_CsiShort[i] = table_sani_CsiShort[i] + 1
      }
    }
  }
  table_sani_CsiShort=table_sani_CsiShort/length(t_sani_CsiShort)
  
  for (j in 1:length(t_sani_CsiLong)){
    for (i in 1:num){
      if(t_sani_CsiLong[j] > a[i] & t_sani_CsiLong[j] < a[i+1]){
        table_sani_CsiLong[i] = table_sani_CsiLong[i] + 1
      }
    }
  }
  table_sani_CsiLong=table_sani_CsiLong/length(t_sani_CsiLong)
  
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
  m=min(c(min(t_sani_CsiShort), min(t_schz_CsiShort), min(t_sani_CsiLong), min(t_schz_CsiLong)))
  M=max(c(max(t_sani_CsiShort), max(t_schz_CsiShort), max(t_sani_CsiLong), max(t_schz_CsiLong)))
  boxplot(t_sani_CsiShort, xlab="Control CsiShort", ylim=c(m, M), ylab="Mean reaction time")
  boxplot(t_sani_CsiLong, xlab="Control CsiLong", ylim=c(m,M), ylab="Mean reaction time")
  boxplot(t_schz_CsiShort, xlab="Schz CsiShort", ylim=c(m,M),ylab="Mean reaction time")
  boxplot(t_schz_CsiLong, xlab="Schz CsiLong", ylim=c(m,M),ylab="Mean reaction time")
  
  barplot(table_sani_CsiShort, names.arg = nomi, ylim = c(0, 0.4), main="sani CsiShort")
  barplot(table_schz_CsiShort, names.arg = nomi, ylim = c(0, 0.4), main="schz CsiShort")
  barplot(table_sani_CsiLong, names.arg = nomi, ylim = c(0, 0.4), main="sani CsiLong")
  barplot(table_schz_CsiLong, names.arg = nomi, ylim = c(0, 0.4), main="schz CsiLong")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

{
  X11()
  m=min(c(min(t_sani_CsiShort), min(t_schz_CsiShort), min(t_sani_CsiLong), min(t_schz_CsiLong)))
  M=max(c(max(t_sani_CsiShort), max(t_schz_CsiShort), max(t_sani_CsiLong), max(t_schz_CsiLong)))
  par(mfrow=c(1,4))
  boxplot(t_sani_CsiShort, xlab="Control CsiShort", ylim=c(m, M), ylab="Mean reaction time")
  boxplot(t_sani_CsiLong, xlab="Control CsiLong", ylim=c(m,M), ylab="Mean reaction time")
  boxplot(t_schz_CsiShort, xlab="Schz CsiShort", ylim=c(m,M),ylab="Mean reaction time")
  boxplot(t_schz_CsiLong, xlab="Schz CsiLong", ylim=c(m,M),ylab="Mean reaction time")
}

graphics.off()


