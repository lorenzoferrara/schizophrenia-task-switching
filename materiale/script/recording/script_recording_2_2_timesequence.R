
##    IN QUESTO SCRIPT cerco un pattern nei tempi di risposta.
##    voglio controllare se migliorano facendo tanti trials
##
setwd("G:/Il mio Drive/Brain Connectivity")

##

setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

df_sani_switch = matrix(NA,125,96)
df_sani_noswitch = matrix(NA,125,96)
df_schz_switch = matrix(NA,50,96)
df_schz_noswitch = matrix(NA,50,96)

#CARICO UNO AD UNO I RECORDINGS DEI SOGGETTI
for ( i in 1:125) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  s$ReactionTime = as.numeric(s$ReactionTime)
  for (j in 1:96){
    if( s$Switching[j] == "SWITCH")
      df_sani_switch[i,j] = s$ReactionTime[j]
    else
      df_sani_noswitch[i,j] = s$ReactionTime[j]
  }
}

for ( i in 126:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  s$ReactionTime = as.numeric(s$ReactionTime)
  for (j in 1:96){
    if( s$Switching[j] == "SWITCH")
      df_schz_switch[i-125,j] = s$ReactionTime[j]
    else
      df_schz_noswitch[i-125,j] = s$ReactionTime[j]
  }
}
  

#normalizzo
df_sani_switch=as.data.frame(df_sani_switch)
df_sani_noswitch=as.data.frame(df_sani_noswitch)
df_schz_switch=as.data.frame(df_schz_switch)
df_schz_noswitch=as.data.frame(df_schz_noswitch)

row.names(df_sani)=elenco_copia$v[1:125]
row.names(df_schz)=elenco_copia$v[126:175]

Xobs0 <- df_sani_switch[1,]
abscissa <- 1:96
NT <- length(abscissa) # number of locations of observations

{x11()
  plot(abscissa,Xobs0,xlab="t",ylab="observed data", type = "l")
}


df_sani = as.data.frame(df_sani)
names(df_sani)="tempi"
df_sani$indici = 1:96
fit_sani <- lm( tempi ~ indici, data = df_sani )
fit_sani$coefficients
summary(fit_sani)

df_schz = as.data.frame(df_schz)
names(df_schz)="tempi"
df_schz$indici = 1:96
fit_schz <- lm( tempi ~ indici, data = df_schz )
fit_schz$coefficients
summary(fit_schz)

{
  x11()
  #x11(width=400, height = 300)
  par(mfrow=c(2,1))
  plot( 1:96, df_sani$tempi, type="l", lty=1, lwd=1.5, xlab="i-th trial", ylab="Reaction time", main = "Control")
  abline(fit_sani$coefficients[1], fit_sani$coefficients[2], col="red")
  plot( 1:96, df_schz$tempi, type="l", lty=1, lwd=1.5, xlab="i-th trial", ylab="Reaction time", main="Schizophrenia")
  abline(fit_schz$coefficients[1], fit_schz$coefficients[2], col="red")
}

