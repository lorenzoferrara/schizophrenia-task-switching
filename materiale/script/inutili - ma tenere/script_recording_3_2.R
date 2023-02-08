
setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)

#s <- read.delim("G:/Il mio Drive/Brain Connectivity/Lorenzo/events recording/sub-10171_task-taskswitch_events.tsv")

#PARTICIPANTS
p <- read.delim("../data/participants.csv")

elenco_copia$v==p$participant_id
#DEVE ESSERE TUTTO TRUE


t_sani_switch = c()
t_schz_switch = c()
t_sani_noswitch=c()
t_schz_noswitch=c()

for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("G:/Il mio Drive/Brain Connectivity/materiale/events recording/", val, sep="" ))
  
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
delta= switchcost_schz - switchcost_sani
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(switchcost_sani), max(switchcost_schz))
  m=min(min(switchcost_sani), min(switchcost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_delta = rep(0,num)
  
  for (j in 1:length(delta)){
    for (i in 1:num){
      if(delta[j] > a[i] & delta[j] <= a[i+1]){
        table_delta[i] = table_delta[i] + 1
      }
    }
  }
  table_delta=table_delta/length(delta)
  
  nomi=round(a[1:num], digits=1)
}


{
  X11()
  par(mfrow=c(1,2))
  m=min(min(delta), min(delta))
  M=max(max(delta), max(delta))
  boxplot(delta, xlab="Control", ylim=c(m, M), ylab="Switchcost")

  M=max(max(table_delta), max(table_delta))
  barplot(table_delta, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Switchcost", ylab="Density")

  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

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
