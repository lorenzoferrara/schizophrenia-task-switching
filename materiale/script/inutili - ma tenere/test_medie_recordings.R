setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)

#PARTICIPANTS
p <- read.delim("../data/participants.csv")


tempi_control <- matrix(NA, nrow = 125, ncol = 96)
tempi_schz <- matrix(NA, nrow = 50, ncol = 96)

elenco_copia$v==p$participant_id

# CREO LE MATRICI (CONTROL E SCHZ) CON I TEMPI DI REAZIONE PER OGNI PARTECIPANTE
for (i in 1:125){
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  for (j in 1: 96)
     tempi_control[i,j] <- as.numeric(s$ReactionTime[j])
}

for (i in 126:175){
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  for (j in 1: 96)
    tempi_schz[i-125,j] <- as.numeric(s$ReactionTime[j])
}

 

n1 <- dim(tempi_control)[1] # n1=3
n2 <- dim(tempi_schz)[1] # n2=4
p  <- dim(tempi_control)[2] # p=2

tempi_control
tempi_schz

tempi_control.mean <- colMeans(tempi_control, na.rm=T)
tempi_schz.mean <- colMeans(tempi_schz, na.rm=T)
tempi_control.cov  <-  var(tempi_control, na.rm=T)
tempi_schz.cov  <-  var(tempi_schz, na.rm=T)
Sp      <- ((n1-1)*tempi_control.cov + (n2-1)*tempi_schz.cov)/(n1+n2-2)
# we compare the matrices
list(S1=tempi_control.cov, S2=tempi_schz.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

library(MASS)
alpha   <- .05
delta.0 <- rep(0,p) #tutti zero
Spinv   <- ginv(Sp)

tempi_schz <- n1*n2/(n1+n2) * (tempi_control.mean-tempi_schz.mean-delta.0) %*% Spinv %*% (tempi_control.mean-tempi_schz.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
tempi_schz < cfr.fisher # FALSE: evidence to reject H0 at level 5%

P <- 1 - pf(tempi_schz/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
# P-value=0

# Simultaneous tempi_schz intervals
IC.tempi_schz.X1 <- c(tempi_control.mean[1]-tempi_schz.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), tempi_control.mean[1]-tempi_schz.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.tempi_schz.X2 <- c(tempi_control.mean[2]-tempi_schz.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), tempi_control.mean[2]-tempi_schz.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.tempi_schz <- rbind(IC.tempi_schz.X1, IC.tempi_schz.X2)
dimnames(IC.tempi_schz)[[2]] <- c('inf','sup')                        
IC.tempi_schz

graphics.off()