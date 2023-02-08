
setwd("G:/Il mio Drive/Brain Connectivity/materiale/SubjData")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_sani_ITIShort = c()
t_schz_ITIShort = c()
t_sani_ITIMedium = c()
t_schz_ITIMedium = c()
t_sani_ITILong=c()
t_schz_ITILong=c()
t_sani_ITIVeryLong=c()
t_schz_ITIVeryLong=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ), sep = " ")
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime_ITIShort <- mean(s[round(s$ITI)==1,]$ReactionTime, na.rm=T)
  meantime_ITIMedium <- mean(s[round(s$ITI)==2,]$ReactionTime, na.rm=T)
  meantime_ITILong <- mean(s[round(s$ITI)==3,]$ReactionTime, na.rm=T)
  meantime_ITIVeryLong <- mean(s[round(s$ITI)==4,]$ReactionTime, na.rm=T)
  
  if (p$diagnosis[i]=="CONTROL"){
    t_sani_ITIShort <- c(t_sani_ITIShort, meantime_ITIShort)
    t_sani_ITIMedium <- c(t_sani_ITIMedium, meantime_ITIMedium)
    t_sani_ITILong <- c(t_sani_ITILong, meantime_ITILong)
    t_sani_ITIVeryLong <- c(t_sani_ITIVeryLong, meantime_ITIVeryLong)
  } else {
    t_schz_ITIShort <- c(t_schz_ITIShort, meantime_ITIShort)
    t_schz_ITIMedium <- c(t_schz_ITIMedium, meantime_ITIMedium)
    t_schz_ITILong <- c(t_schz_ITILong, meantime_ITILong)
    t_schz_ITIVeryLong <- c(t_schz_ITIVeryLong, meantime_ITIVeryLong)
  }
}

#creo le tabelle delle frequenze
{
  num=13
  a <- seq(0, 1.4, 1.4/(num))
  table_sani_ITIShort = rep(0,num)
  table_sani_ITILong = rep(0,num)
  table_schz_ITIShort = rep(0,num)
  table_schz_ITILong = rep(0,num)
  
  for (j in 1:length(t_sani_ITIShort)){
    for (i in 1:num){
      if(t_sani_ITIShort[j] > a[i] & t_sani_ITIShort[j] < a[i+1]){
        table_sani_ITIShort[i] = table_sani_ITIShort[i] + 1
      }
    }
  }
  table_sani_ITIShort=table_sani_ITIShort/length(t_sani_ITIShort)
  
  for (j in 1:length(t_sani_ITILong)){
    for (i in 1:num){
      if(t_sani_ITILong[j] > a[i] & t_sani_ITILong[j] < a[i+1]){
        table_sani_ITILong[i] = table_sani_ITILong[i] + 1
      }
    }
  }
  table_sani_ITILong=table_sani_ITILong/length(t_sani_ITILong)
  
  for (j in 1:length(t_schz_ITIShort)){
    for (i in 1:num){
      if(t_schz_ITIShort[j] > a[i] & t_schz_ITIShort[j]<a[i+1]){
        table_schz_ITIShort[i]= table_schz_ITIShort[i]+1
      }
    }
  }
  table_schz_ITIShort=table_schz_ITIShort/length(t_schz_ITIShort)
  
  for (j in 1:length(t_schz_ITILong)){
    for (i in 1:num){
      if(t_schz_ITILong[j] > a[i] & t_schz_ITILong[j]<a[i+1]){
        table_schz_ITILong[i]= table_schz_ITILong[i]+1
      }
    }
  }
  table_schz_ITILong=table_schz_ITILong/length(t_schz_ITILong)
  
  nomi=round(a[1:num], digits=1)
}


{
  X11()
  par(mfrow=c(2,4))
  m=min(c(min(t_sani_ITIShort), min(t_schz_ITIShort), min(t_sani_ITILong), min(t_schz_ITILong)))
  M=max(c(max(t_sani_ITIShort), max(t_schz_ITIShort), max(t_sani_ITILong), max(t_schz_ITILong)))
  boxplot(t_sani_ITIShort, xlab="Control ITIShort", ylim=c(m, M), ylab="Mean reaction time")
  boxplot(t_sani_ITILong, xlab="Control ITILong", ylim=c(m,M), ylab="Mean reaction time")
  boxplot(t_schz_ITIShort, xlab="Schz ITIShort", ylim=c(m,M),ylab="Mean reaction time")
  boxplot(t_schz_ITILong, xlab="Schz ITILong", ylim=c(m,M),ylab="Mean reaction time")
  
  barplot(table_sani_ITIShort, names.arg = nomi, ylim = c(0, 0.4), main="sani ITIShort")
  barplot(table_schz_ITIShort, names.arg = nomi, ylim = c(0, 0.4), main="schz ITIShort")
  barplot(table_sani_ITILong, names.arg = nomi, ylim = c(0, 0.4), main="sani ITILong")
  barplot(table_schz_ITILong, names.arg = nomi, ylim = c(0, 0.4), main="schz ITILong")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

{
  X11()
  m=min(c(min(t_sani_ITIShort), min(t_schz_ITIShort), min(t_sani_ITILong), min(t_schz_ITILong)))
  M=max(c(max(t_sani_ITIShort), max(t_schz_ITIShort), max(t_sani_ITILong), max(t_schz_ITILong)))
  par(mfrow=c(1,4))
  boxplot(t_sani_ITIShort, xlab="Control ITIShort", ylim=c(m, M), ylab="Mean reaction time")
  boxplot(t_sani_ITILong, xlab="Control ITILong", ylim=c(m,M), ylab="Mean reaction time")
  boxplot(t_schz_ITIShort, xlab="Schz ITIShort", ylim=c(m,M),ylab="Mean reaction time")
  boxplot(t_schz_ITILong, xlab="Schz ITILong", ylim=c(m,M),ylab="Mean reaction time")
}

graphics.off()

###################

df_sani = data.frame( t_sani_ITIShort,
                      t_sani_ITIMedium,
                      t_sani_ITILong,
                      t_sani_ITIVeryLong )

df_schz = data.frame( t_schz_ITIShort,
                      t_schz_ITIMedium,
                      t_schz_ITILong,
                      t_schz_ITIVeryLong )


{x11()
 par(mfrow=c(2,2))
 boxplot(df_sani)
 boxplot(df_schz)
 matplot(t(df_sani), type='l', main = 'Data', ylim=range(df_sani, na.rm = T))
 matplot(t(df_schz), type='l', main = 'Data', ylim=range(df_schz, na.rm = T))
}

graphics.off()

#pc.food <- princomp(food.sd, scores=T)

