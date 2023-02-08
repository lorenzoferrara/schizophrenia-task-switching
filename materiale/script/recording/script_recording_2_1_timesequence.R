
##    IN QUESTO SCRIPT cerco un pattern nei tempi di risposta.
##    voglio controllare se migliorano facendo tanti trials
##
setwd("G:/Il mio Drive/Brain Connectivity")

##

setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

sequenza_tempi_sani = rep(0,96)
sequenza_tempi_schz = rep(0,96)
numero_sani= rep(0,96)
numero_schz= rep(0,96)
#CARICO UNO AD UNO I RECORDINGS DEI SOGGETTI
for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  s$ReactionTime = as.numeric(s$ReactionTime)
  if (p$diagnosis[i]=="CONTROL"){
    for (j in 1:96){
      if ( ! is.na(s$ReactionTime[j])) {
        #print( is.na(s$ReactionTime[j]) )
        sequenza_tempi_sani[j] = sequenza_tempi_sani[j] + s$ReactionTime[j]
        numero_sani[j] = numero_sani[j] + 1
      }
    }
  }
  else{
    for (j in 1:96){
      if ( ! is.na(s$ReactionTime[j])) {
        sequenza_tempi_schz[j] = sequenza_tempi_schz[j] + s$ReactionTime[j]
        numero_schz[j] = numero_schz[j] + 1
      }
    }
  }
}
#normalizzo
sequenza_tempi_sani = sequenza_tempi_sani / numero_sani
sequenza_tempi_schz = sequenza_tempi_schz / numero_schz

sequenza_tempi_sani = as.data.frame(sequenza_tempi_sani)
names(sequenza_tempi_sani)="tempi"
sequenza_tempi_sani$indici = 1:96
fit_sani <- lm( tempi ~ indici, data = sequenza_tempi_sani )
fit_sani$coefficients
summary(fit_sani)

sequenza_tempi_schz = as.data.frame(sequenza_tempi_schz)
names(sequenza_tempi_schz)="tempi"
sequenza_tempi_schz$indici = 1:96
fit_schz <- lm( tempi ~ indici, data = sequenza_tempi_schz )
fit_schz$coefficients
summary(fit_schz)

{
  x11()
  #x11(width=400, height = 300)
  par(mfrow=c(2,1))
  plot( 1:96, sequenza_tempi_sani$tempi, type="l", lty=1, lwd=1.5, xlab="i-th trial", ylab="Reaction time", main = "Control")
  abline(fit_sani$coefficients[1], fit_sani$coefficients[2], col="red")
  plot( 1:96, sequenza_tempi_schz$tempi, type="l", lty=1, lwd=1.5, xlab="i-th trial", ylab="Reaction time", main="Schizophrenia")
  abline(fit_schz$coefficients[1], fit_schz$coefficients[2], col="red")
}

