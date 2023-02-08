##    ANALISI SUI DATI DEL BARRATT TEST

library("ggplot2")

setwd("C:/Users/lofer/OneDrive/Documenti/GitHub/Brain-Connectivity")
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

load("../workspaces/times.RData")
times = c(t_control,t_schz) 

#TEST tipico diviso
fit = lm( times ~ total_score+part_list$diagnosis, data=barratt_raw_data)
summary(fit)
coef=coefficients(fit)

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
## Plot di RT e BIS 

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
