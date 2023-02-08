#PERFORMANCE INDEX

t_sani
setwd("G:/Il mio Drive/Brain Connectivity")
setwd("./materiale/events recording")

accuracy = rep(0, 175)
for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  
  accuracy[i] = mean(s$CorrectResp, na.rm=T)
  
}

{
x11()
par(mfrow=c(1,3))
boxplot(accuracy[1:125], accuracy[126:175])
boxplot(t_sani, t_schz)
boxplot(t_sani/accuracy[1:125], t_schz/accuracy[126:175], ylim=c(0,3))
}
