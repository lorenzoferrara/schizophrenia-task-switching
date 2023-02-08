
load("./materiale/workspaces/times.RData")
load("./materiale/workspaces/fumo.RData")

times = c(t_sani,t_schz) 
## Plot di RT e BIS 

colors=c(rep("blue",125),rep("red",50))

indici_ex = as.numeric(row.names(ex_smokers))
indici_cur = as.numeric(row.names(current_smokers))
indici_not = as.numeric(row.names(not_smokers))
times_ex = times[indici_ex]
times_cur = times[indici_cur]
times_not = times[indici_not]

indici_notcurr = c(indici_ex, indici_cur)

{
  x11()
  par(mfrow=c(1,3))
  boxplot(times_cur, xlab="Current smokers")
  boxplot(times_ex,xlab="Ex smokers")
  boxplot(times_not, xlab="Not smokers")
}
length(times_cur)
length(times_ex)
length(times_not)


{
  n1 <- length(times_ex)[1] # n1=31
  n2 <- length(times_not)[1] # n2=13
  
  p=1
  
  times_ex.mean <- mean(times_ex, na.rm=T)
  times_not.mean <- mean(times_not, na.rm=T)
  times_ex.cov  <-  var(times_ex, na.rm=T)
  times_not.cov  <-  var(times_not, na.rm=T)
  Sp      <- ((n1-1)*times_ex.cov + (n2-1)*times_not.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=times_ex.cov, S2=times_not.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  library(MASS)
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (times_ex.mean-times_not.mean-delta.0) %*% Spinv %*% (times_ex.mean-times_not.mean-delta.0)
  
  cfr.chisq <- (p*(n1+n2-2)/(n1+n2-1-p))*qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P

############
{
  n1 <- length(times_cur)[1] # n1=31
  n2 <- length(times_not)[1] # n2=13
  
  p=1
  
  times_cur.mean <- mean(times_cur, na.rm=T)
  times_not.mean <- mean(times_not, na.rm=T)
  times_cur.cov  <-  var(times_cur, na.rm=T)
  times_not.cov  <-  var(times_not, na.rm=T)
  Sp      <- ((n1-1)*times_cur.cov + (n2-1)*times_not.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=times_cur.cov, S2=times_not.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  library(MASS)
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (times_cur.mean-times_not.mean-delta.0) %*% Spinv %*% (times_cur.mean-times_not.mean-delta.0)
  
  cfr.chisq <- (p*(n1+n2-2)/(n1+n2-1-p))*qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  

#RISULTATI: il tempo medio di ex smokers e non smoker ? lo stesso
# mentre il tempo medio di current smokers e non smokers ? diverso

#########
#lo rifaccio unendo ex_smokers e non smokers

times_cur = times[as.numeric(row.names(current_smokers))]
times_noncur = c(times_ex, times_not)

{
  x11()
  par(mfrow=c(1,2))
  boxplot(times_cur, xlab="Current smokers")

  boxplot(times_noncur, xlab="Not current smokers")
}

length(times_cur)
length(times_noncur)

{
  n1 <- length(times_cur)[1] # n1=31
  n2 <- length(times_noncur)[1] # n2=13
  
  p=1
  
  times_cur.mean <- mean(times_cur, na.rm=T)
  times_noncur.mean <- mean(times_noncur, na.rm=T)
  times_cur.cov  <-  var(times_cur, na.rm=T)
  times_noncur.cov  <-  var(times_noncur, na.rm=T)
  Sp      <- ((n1-1)*times_cur.cov + (n2-1)*times_noncur.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=times_cur.cov, S2=times_noncur.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  library(MASS)
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (times_cur.mean-times_noncur.mean-delta.0) %*% Spinv %*% (times_cur.mean-times_noncur.mean-delta.0)
  
  cfr.chisq <- (p*(n1+n2-2)/(n1+n2-1-p))*qchisq(1-alpha,p)
  T2 < cfr.chisq # FALSE: evidence to reject H0 at level 5%
}
P <- 1 - pchisq(T2, p)
P  
#PERFETTO, SONO DIVERSI

# Confidence interval
IC.differenza_tempi <- c(times_cur.mean[1]-times_noncur.mean[1]-sqrt(cfr.chisq*Sp*(1/n1+1/n2)), times_cur.mean[1]-times_noncur.mean[1]+sqrt(cfr.chisq*Sp*(1/n1+1/n2)) )
c('inf','sup')                        
IC.differenza_tempi



#################
#GIA CHE CI SONO VEDO COME SI DISTRIBUISCNO I FUMATORI TRA SHCIZO E SANI

current_smoking_bool = rep(0,175)
for(i in as.numeric(row.names(current_smokers))){
  current_smoking_bool[i]=1
}
sum(current_smoking_bool[1:125])/125  #percentuale di fumatori tra i sani
sum(current_smoking_bool[126:175])/50 #percentuale di fumatori tra gli schz

fit <- glm(current_smoking_bool ~ times)
summary(fit)
coef=fit$coefficients
library(sigmoid)
{
  x11()
  plot(times, current_smoking_bool)
  par(new=T)
  y=sigmoid(coef[1]+times*coef[2])
  plot(times, y, ylim=c(0,1))
}
#NON CAPISCO, VOELVO FARE UNA LOGISTIC REGRESSION MA MI VIENE QUELO SCHIFO??


########
data=cbind(c(sum(current_smoking_bool[1:125]), sum(current_smoking_bool[126:175])), c(125-sum(current_smoking_bool[1:125]), 50-sum(current_smoking_bool[126:175]))) #percentuale di fumatori tra gli schz)
data=as.table(t(data))
colnames(data)=c("Control", "Schz")
rownames(data)=c("Current_smoker", "Non_current_smoker")
{
  x11()
  barplot(data, legend=T, beside=T, main='Smoking habits')
}

graphics.off()
