
congruentcost_sani <- t_sani_congruent - t_sani_incongruent
congruentcost_schz <- t_schz_congruent - t_schz_incongruent
#creo le tabelle delle frequenze
{
  num=13
  M=max(max(congruentcost_sani), max(congruentcost_schz))
  m=min(min(congruentcost_sani), min(congruentcost_schz))
  a <- seq(m, M, (M-m)/(num))
  table_congruentcost_sani = rep(0,num)
  table_congruentcost_schz = rep(0,num)
  
  for (j in 1:length(congruentcost_sani)){
    for (i in 1:num){
      if(congruentcost_sani[j] > a[i] & congruentcost_sani[j] <= a[i+1]){
        table_congruentcost_sani[i] = table_congruentcost_sani[i] + 1
      }
    }
  }
  table_congruentcost_sani=table_congruentcost_sani/length(congruentcost_sani)
  
  for (j in 1:length(congruentcost_schz)){
    for (i in 1:num){
      if(congruentcost_schz[j] > a[i] & congruentcost_schz[j] <= a[i+1]){
        table_congruentcost_schz[i] = table_congruentcost_schz[i] + 1
      }
    }
  }
  table_congruentcost_schz=table_congruentcost_schz/length(congruentcost_schz)
  
  nomi=round(a[1:num], digits=2)
}

{
  X11()
  par(mfrow=c(2,2))
  m=min(min(congruentcost_sani), min(congruentcost_schz))
  M=max(max(congruentcost_sani), max(congruentcost_schz))
  boxplot(congruentcost_sani, xlab="Control", ylim=c(m, M), ylab="Congruentcost")
  boxplot(congruentcost_schz, xlab="Schizophrenia", ylim=c(m, M),ylab="Congruentcost")
  
  M=max(max(table_congruentcost_schz), max(table_congruentcost_sani))
  barplot(table_congruentcost_sani, names.arg = nomi, ylim = c(0, M), main="Control", xlab="Congruentcost", ylab="Density")
  barplot(table_congruentcost_schz, names.arg = nomi, ylim = c(0, M), main="Schizophrenia", xlab="Congruentcost", ylab="Density")
  
  mtext("Distribuzione dei ReactionTime", side=3, line=-3, outer=T)
}

library(ggplot2)
pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Congruent.Cost=c(congruentcost_sani,congruentcost_schz)),aes(x=Diagnosis,y=Congruent.Cost))+
  geom_boxplot(aes(fill=Diagnosis),width=0.5)+
  scale_fill_manual(values = c("royalblue","orange2"))+
  theme_minimal()+theme(legend.position = "")
pp

p1 = ggplot(data.frame(Congruent.Cost=congruentcost_sani),aes(x=Congruent.Cost))+
  geom_histogram(aes(y = ..density..),binwidth = 0.05,colour="gray", fill="royalblue")+
  theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,7.5)+xlim(-0.3,0.3)+xlab("Control")+ylab("")
p2 = ggplot(data.frame(Congruent.Cost=congruentcost_schz),aes(x=Congruent.Cost))+
  geom_histogram(aes(y = ..density..),binwidth = 0.05,colour="gray", fill="orange2")+
  theme_minimal(base_size = 15)+theme(legend.position = "")+ylim(0,7.5)+xlim(-0.3,0.3)+xlab("Schz")+ylab("")

grid.arrange(p1,p2,nrow=1,top="Density of Reaction Time")


#___________________________________
{
n1 <- length(congruentcost_sani)[1] # n1=3
n2 <- length(congruentcost_schz)[1] # n2=4

p=1

congruentcost_sani
congruentcost_schz

congruentcost_sani.mean <- mean(congruentcost_sani, na.rm=T)
congruentcost_schz.mean <- mean(congruentcost_schz, na.rm=T)
congruentcost_sani.cov  <-  var(congruentcost_sani, na.rm=T)
congruentcost_schz.cov  <-  var(congruentcost_schz, na.rm=T)
Sp      <- ((n1-1)*congruentcost_sani.cov + (n2-1)*congruentcost_schz.cov)/(n1+n2-2)
# we compare the matrices
list(S1=congruentcost_sani.cov, S2=congruentcost_schz.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

alpha   <- .05
delta.0 <- rep(0,p) #tutti zero
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (congruentcost_sani.mean-congruentcost_schz.mean-delta.0) %*% Spinv %*% (congruentcost_sani.mean-congruentcost_schz.mean-delta.0)

cfr.chisq <- qchisq(1-alpha,p)
T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
# P-value=0.85

graphics.off()