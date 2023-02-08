##    ANALISI SUI DATI DEL BARRATT TEST

library("ggplot2")

colors=c(rep("blue",125),rep("red",50))

load("./materiale/workspaces/rec_2.RData")
setwd("./materiale/script")
part_list <- read.delim("../data/participants.csv")
barratt_raw_data <- read.csv("./barratt/barratt_raw_data.txt", sep="")
score=barratt_raw_data$total_score
score_control=score[1:125]
score_schz = score[126:175]

{
  M=max(max(score_control), max(score_schz))
    num=13
  a <- seq(0, M, M/(num))
  table_control_score = rep(0,num)
  table_schz_score = rep(0,num)
  
  for (j in 1:length(score_control)){
    for (i in 1:num){
      if(score_control[j] > a[i] & score_control[j] < a[i+1]){
        table_control_score[i] = table_control_score[i] + 1
      }
    }
  }
  table_control_score=table_control_score/length(score_control)
  
  for (j in 1:length(score_schz)){
    for (i in 1:num){
      if(score_schz[j] > a[i] & score_schz[j] < a[i+1]){
        table_schz_score[i] = table_schz_score[i] + 1
      }
    }
  }
  table_schz_score=table_schz_score/length(score_schz)
}
nomi=round(a[1:num], digits=1)
{
  m=min(min(table_control_score), min(table_schz_score))
  M=max(max(table_control_score), max(table_schz_score))
  x11()
  par(mfrow=c(1,2))
  barplot(table_control_score, names.arg = nomi, ylim = c(m, M), main="Control",col = "lightblue1")
  barplot(table_schz_score, names.arg = nomi, ylim = c(m, M), main="Schizophrenia",col = "tomato3")
}
color=c("lightskyblue1","tomato3")
{
  m=min(min(score_schz), min(score_control))
  M=max(max(score_schz), max(score_control))
  x11()
  boxplot(score_control,score_schz, names = c("Control","Schizophrenia"),main="Barratt Scores", col= color,ylim = c(m, M))
}

{x11()
pp = ggplot(data.frame(Diagnosis=part_list$diagnosis, Barratt.Score=score),aes(x=Diagnosis,y=Barratt.Score))+
     geom_boxplot( aes(fill=Diagnosis),width=0.5)+
     scale_fill_manual(values = c("royalblue","orange2" ))+
     theme_minimal()+theme(legend.position = "", text = element_text(size=20), plot.title = element_text(hjust=0.5, size=20, face = "bold")) + xlab("") + ylab("") + labs(title="Barratt Scores")
  
pp
}

#________________________________________________________________________
# Test sulle medie per Barratt scores
{
  n1 <- length(score_control)[1] # n1=3
  n2 <- length(score_schz)[1] # n2=4
  
  p=1
  
  score_control.mean <- mean(score_control, na.rm=T)
  score_schz.mean <- mean(score_schz, na.rm=T)
  score_control.cov  <-  var(score_control, na.rm=T)
  score_schz.cov  <-  var(score_schz, na.rm=T)
  Sp      <- ((n1-1)*score_control.cov + (n2-1)*score_schz.cov)/(n1+n2-2)
  # we compare the matrices
  list(S1=score_control.cov, S2=score_schz.cov, Spooled=Sp)
  
  # Test H0: mu1 == mu2  vs  H1: mu1 != mu2
  # i.e.,
  # Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
  
  alpha   <- .05
  delta.0 <- rep(0,p) #tutti zero
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (score_control.mean-score_schz.mean-delta.0) %*% Spinv %*% (score_control.mean-score_schz.mean-delta.0)
  
  cfr.chisq <- qchisq(1-alpha,p)
  T2 < cfr.chisq
}
P <- 1 - pchisq(T2, p)
P  

#___________________________________

var.test(score_control, score_schz)
t.test(score_control, score_schz, alternative = "greater", var.equal = F)
#________________________________________________________________________
# Shapiro test
shapiro.test(score_control)
shapiro.test(score_schz)


#________________________________________________________________________
#LEVENE TEST SULLE VARIANZE

library("lawstat")
dati=c(score_control, score_schz)
gruppi=factor( c( rep("Control", 125), rep("Schz", 50)))

levene.test(dati, gruppi, location ="mean")


#________________________________________________________________________

#TEST tipico classico
fit = lm( times ~ total_score, data=barratt_raw_data)
summary(fit)
#1% di pvalue, posso anche farmelo andare bene

coef=coefficients(fit)
plot(barratt_raw_data$total_score,times,col=colors, xlab="BIS total score",ylab="Reaction Time")
abline(coef[1], coef[2], col="green")

#________________________________________________________________________

#TEST tipico diviso
fit = lm( times ~ total_score*part_list$diagnosis, data=barratt_raw_data)
summary(fit)
#1% di pvalue, posso anche farmelo andare bene

coef=coefficients(fit)
plot(barratt_raw_data$total_score,times,col=colors, xlab="BIS total score",ylab="Reaction Time")
abline(coef[1], coef[2], col="blue")
abline(coef[1]+coef[3], coef[2]+coef[4], col="red")

#________________________________________________________________________

#TEST tipico diviso
fit = lm( times ~ total_score+part_list$diagnosis, data=barratt_raw_data)
summary(fit)
#1% di pvalue, posso anche farmelo andare bene

coef=coefficients(fit)
plot(barratt_raw_data$total_score,times,col=colors, xlab="BIS total score",ylab="Reaction Time")
abline(coef[1], coef[2], col="blue")
abline(coef[1]+coef[3], coef[2], col="red")

{x11()
pp=NULL
pp  = ggplot(data=data.frame(Barratt.Score=barratt_raw_data$total_score, Reaction.Time=times, diag=part_list$diagnosis),aes(Barratt.Score,Reaction.Time))+
  geom_point(aes(colour=part_list$diagnosis))+            
  scale_colour_manual(values=c( "CONTROL" = "royalblue", "SCHZ" = "orange2"), name = "Diagnosis")+
  geom_abline(aes(intercept=coef[1],slope=coef[2],color="CONTROL"))+
  geom_abline(aes(intercept=coef[1]+coef[3],slope=coef[2],color="SCHZ"))+
  theme( legend.position = " ", axis.title = element_text(size=14, face = "bold"), legend.title = element_blank(), 
         legend.text = element_blank())
pp
}

#________________________________________________________________________
load("../workspaces/rec_2.RData")
times = c(t_control,t_schz) 
## Plot di RT e BIS 

colors=c(rep("blue",125),rep("red",50))
plot(barratt_raw_data$total_score,times,col=colors, xlab="BIS total score",ylab="Reaction Time")

library(MASS)
full.model = lm( times ~ NON_PLANNING*MOTOR*ATTENTIONAL*total_score, data=barratt_raw_data )
summary(full.model)
step.model = stepAIC( full.model, direction = "both", trace = FALSE)
summary(step.model)

full.model = lm( times ~ NON_PLANNING*MOTOR*ATTENTIONAL, data=barratt_raw_data )
summary(full.model)

step.model = stepAIC( full.model, direction = "both", trace = FALSE)
summary(step.model)

full.model = lm( times ~ NON_PLANNING:ATTENTIONAL, data=barratt_raw_data )
summary(full.model)

#full.model = lm( times ~ ATTENTIONAL, data=barratt_raw_data )
#summary(full.model)

###############################

coef = step.model$coefficients

## Plot di RT e BIS 
# prendi i dati da script_recording_2.R in Lorenzo
colors=c(rep("blue",125),rep("red",50))
plot(barratt_raw_data$total_score,c(t_control,t_schz),col=colors, xlab="BIS total score",ylab="Reaction Time")

colors=c(rep("blue",125),rep("red",50))
plot(barratt_raw_data$NON_PLANNING,c(t_control,t_schz),col=colors, xlab="BIS NON_PLANNNING score",ylab="Reaction Time")
abline(coef[1], coef[2])
colors=c(rep("blue",125),rep("red",50))
plot(barratt_raw_data$MOTOR,c(t_control,t_schz),col=colors, xlab="BIS MOTOR score",ylab="Reaction Time")

colors=c(rep("blue",125),rep("red",50))
plot(barratt_raw_data$ATTENTIONAL,c(t_control,t_schz),col=colors, xlab="BIS ATTENTIONAL score",ylab="Reaction Time")



#---------------------------------------------------------------
fit.att = lm(times ~ barratt_raw_data$ATTENTIONAL + part_list$diagnosis)
summary(fit.att)
coef.att=fit.att$coefficients

{
  x11()
  #quartz()
  plot(barratt_raw_data$ATTENTIONAL, times, col=colors, xlab="ATTENTIONAL", ylab="Reaction time", main="Results of Linear Regression")
  abline(coef.att[1], coef.att[2], col="blue", lwd=2)
  abline(coef.att[1] +coef.att[3], coef.att[2], col="red", lwd=2)
  legend( "topright", col = c("blue", "red"), legend = c("Control", "Schizophrenia"), lty=1, lw=2)
}

fit.motor = lm(times ~ barratt_raw_data$MOTOR * part_list$diagnosis)
summary(fit.motor)
coef.motor=fit.motor$coefficients
{
  x11()
  #quartz()
  plot(barratt_raw_data$MOTOR, times, col=colors, xlab="MOTOR", ylab="Reaction time", main="Results of Linear Regression")
  abline(coef.motor[1], coef.motor[2], col="blue", lwd=2)
  abline(coef.motor[1] +coef.motor[3], coef.motor[2], col="red", lwd=2)
  legend( "topright", col = c("blue", "red"), legend = c("Control", "Schizophrenia"), lty=1, lw=2)
}

#___________________________________________________________________


library(MASS)
library(car)
library(rgl)

library(glmnet)

#_______________________________________________________________________________
##### Problem of collinearity

full.model = lm( times ~ NON_PLANNING*MOTOR*ATTENTIONAL, data=barratt_raw_data )
summary(full.model)

# Variance inflation factor
vif(full.model)

dataset= cbind(barratt_raw_data$NON_PLANNING, barratt_raw_data$MOTOR, barratt_raw_data$ATTENTIONAL)
### A possible solution to collinearity: PCA
pc <- princomp( dataset, scores=TRUE)
summary(pc)
pc$load

sp1.pc <- pc$scores[,1]
sp2.pc <- pc$scores[,2]

fm.pc <- lm(times ~ sp1.pc + sp2.pc)
summary(fm.pc)

m1 <- mean(speed1)
m2 <- mean(speed2)
beta0 <- coefficients(fm.pc)[1] - 
  coefficients(fm.pc)[2]*pc$load[1,1]*m1 - 
  coefficients(fm.pc)[3]*pc$load[1,2]*m1 - 
  coefficients(fm.pc)[2]*pc$load[2,1]*m2 - 
  coefficients(fm.pc)[3]*pc$load[2,2]*m2
beta1 <- coefficients(fm.pc)[2]*pc$load[1,1] + 
  coefficients(fm.pc)[3]*pc$load[1,2] 
beta2 <- coefficients(fm.pc)[2]*pc$load[2,1] + 
  coefficients(fm.pc)[3]*pc$load[2,2] 

#RIPORTIAMO I VALORI DEI BETA NEL SISTEMA DI RIF DELLE COVARIATE INIZIALI
c(beta0=as.numeric(beta0),beta1=as.numeric(beta1),beta2=as.numeric(beta2))
fm$coefficients

x <- seq(0, 25, len=100)
plot(cars, xlab='Speed', ylab='Stopping times', las=1)
lines(x, beta0+beta1*x+beta2*x^2)

# Reduce the model:
fm.pc <- lm(times ~ sp1.pc)
summary(fm.pc) 

# We can re-write the model as:
# Model: y = b0 + b1*      PC1                 + eps =
#          = b0 + b1*(e11*(X1-m1)1+e21*(X2-m2)) + eps =
#          = b0 - b1*e11*m1 - b2*e21*m2 + b1*e11*X1 + b1*e21*X2 + eps
beta0 <- coefficients(fm.pc)[1] - 
  coefficients(fm.pc)[2]*pc$load[1,1]*m1 - 
  coefficients(fm.pc)[2]*pc$load[2,1]*m2 
beta1 <- coefficients(fm.pc)[2]*pc$load[1,1]
beta2 <- coefficients(fm.pc)[2]*pc$load[2,1]

c(beta0=as.numeric(beta0),beta1=as.numeric(beta1),beta2=as.numeric(beta2))
fm$coefficients

plot(sp1.pc,times, xlab='PC1', ylab='Reaction time', las=1)
x <- seq(-250,361,by=1)
b <- coef(fm.pc)
lines(x, b[1]+b[2]*x)

plot(speed1,times, ylab='Stopping times', las=1, ylim=c(-5,130))
x <- seq(0,25,by=1)
lines(x, beta0 + beta1*x + beta2*x^2)

# diagnostics of the residuals: da fare per controllare che i riultati siano attendibili
par(mfrow=c(2,2))
plot(fm.pc)

shapiro.test(residuals(fm.pc))

dev.off()


############################################################

###Another possible solution to collinearity: ridge regression

lambda <- 0.5 # Fix lambda: penalization parameter
fit.ridge <- lm.ridge(times ~ speed1 + speed2, lambda = lambda)
# Note: R automatically centers X and Y with respect to their mean.

coef.ridge <- coef(fit.ridge)
yhat.lm <- cbind(rep(1,n), speed1, speed2)%*%coef(fm)  # LM fitted values
yhat.r <- cbind(rep(1,n), speed1, speed2)%*%coef.ridge # ridge fitted values

plot(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',xlab='Speed')
points(speed1, times, pch=1, cex=.8)
matlines(speed1, yhat.r, type='l', lty=1,col=grey.colors(length(lambda)), lwd=2)
legend("topleft",c("lm","ridge"),lty=c(4,1),col=c("black",grey.colors(length(lambda))),lwd=2)


# Repeat for a grid of lambda's
lambda.c <- seq(0,10,0.01)
fit.ridge <- lm.ridge(times ~ speed1 + speed2, lambda = lambda.c)

{x11(width=14, height=5)
  par(mfrow=c(1,3))
  plot(lambda.c,coef(fit.ridge)[,1], type='l', xlab=expression(lambda),
       ylab=expression(beta[0]))
  abline(h=coef(fm)[1], lty=2)
  plot(lambda.c,coef(fit.ridge)[,2], type='l', xlab=expression(lambda),
       ylab=expression(beta[1]))
  abline(h=coef(fm)[2], lty=2)
  plot(lambda.c,coef(fit.ridge)[,3], type='l', xlab=expression(lambda),
       ylab=expression(beta[2]))
  abline(h=coef(fm)[3], lty=2)
}
dev.off()

yhat.lm <- cbind(rep(1,n), speed1, speed2)%*%coef(fm)

plot(speed1, yhat.lm, type='l', lty=1, lwd=2, ylab='Distance',
     xlab='Speed')
points(speed1, times, pch=1, cex=.8)
yhat.r <- NULL
for(i in 1:length(lambda.c))
  yhat.r=cbind(yhat.r, cbind(rep(1,n), speed1, speed2)%*%coef(fit.ridge)[i,])
matlines(speed1, yhat.r, type='l', lty=1,
         col=grey.colors(length(lambda.c)))
lines(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',
      xlab='Speed')




# Choice of the optimal lambda, e.g., via cross-validation
select(fit.ridge)

# or
lambda.opt <- lambda.c[which.min(fit.ridge$GCV)]  #GENERALIZED CROSS VALIDATION MI D?  IL MIGLIOR LAMBDA
lambda.opt

{x11(width=14, height=5)
  par(mfrow=c(1,3))
  plot(lambda.c,coef(fit.ridge)[,1], type='l', xlab=expression(lambda),
       ylab=expression(beta[0]))
  abline(h=coef(fm)[1], lty=1, col='grey')
  abline(v=lambda.opt, col=2, lty=2)
  plot(lambda.c,coef(fit.ridge)[,2], type='l', xlab=expression(lambda),
       ylab=expression(beta[1]))
  abline(h=coef(fm)[2], lty=1, col='grey')
  abline(v=lambda.opt, col=2, lty=2)
  plot(lambda.c,coef(fit.ridge)[,3], type='l', xlab=expression(lambda),
       ylab=expression(beta[2]))
  abline(h=coef(fm)[3], lty=1, col='grey')
  abline(v=lambda.opt, col=2, lty=2)
}
dev.off()

{
  x11()
  plot(speed1, times, pch=1, cex=.8, ylab='Distance',
       xlab='Speed')
  matlines(speed1, yhat.r, type='l', lty=1,
           col=grey.colors(length(lambda.c)))
  lines(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',
        xlab='Speed')
  lines(speed1, yhat.r[,which.min(fit.ridge$GCV)], type='l', lty=1, lwd=2,
        col=2, ylab='Distance', xlab='Speed')
  legend("topleft", c('LM', 'Ridge opt.' ), lty=c(4,1), col=c(1,2), lwd=2)
  
  coef.ridge <- coef(fit.ridge)[which.min(fit.ridge$GCV),]
  coef.ridge
}


###Another possible solution to collinearity: lasso regression using glmnet())

#glmnet implementa l'elastic net. con alpha=1 abbiamo il lasso, con alpha=0 abbiamo il ridge
# Build the matrix of predictors
x <- model.matrix(times ~ speed1+speed2)[,-1]
# la funzione model.matrix ? utile nel caso di variabili categoriche perch? 
# crea gi? la design matrix con le dummy variables

# Build the vector of response
y <- times

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- 10^seq(5,-3,length=100)
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 => lasso

plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV
#cv.glmnet fa una cross validation

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)
#in alto ho il numero di regressori che rimangono nel modello se 
#prendo quello specifico lambda


# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:3,]
coef.lasso 

