
rm(list=ls())

setwd("G:/Il mio Drive/Brain Connectivity")

####

setwd("./materiale")
load("./workspaces/times.RData")
load("./workspaces/recording_iti.RData")

{library(nlmeU)
  library(corrplot)
  library(nlme)
  library(lattice)
  library(plot.matrix)
  library(lme4)
  library(insight)
}

# Model 1. Random intercept, homoscedastic residuals

tempi.iti = c( t_sani_ITIShort, t_schz_ITIShort, t_sani_ITIMedium, t_schz_ITIMedium, t_sani_ITILong, t_schz_ITILong, 
               t_sani_ITIVeryLong, t_schz_ITIVeryLong)

ITI.label = c(rep("SHORT", 175), rep("MEDIUM", 175), rep("LONG", 175), rep("VERYLONG", 175))
ITI.duration= c(rep(1, 175), rep(2, 175), rep(3, 175), rep(4, 175)) 
diagn_iti = c( rep("CONTROL", 125), rep("SCHZ", 50))
diagn_iti = rep(diagn_iti, 4)
#diagn_iti = as.factor(diagn_iti)

{df = matrix(nrow = length(diagn_iti), ncol = 4, data = 0)
  df[,1] = tempi.iti
  df[,2] = diagn_iti
  df[,3] = ITI.label
  df[,4] = ITI.duration
  df=as.data.frame(df)
  names(df) = c("tempi.iti", "diagn_iti", "ITI.label", "ITI.duration")
  df
}

{
  x11()
  par(mfrow=c(2,3))
  boxplot(df_sani, main = "Contrl Boxplot")
  boxplot(df_schz, main = "Schz Boxplot")
  boxplot( tempi.iti~ITI.label, main = "Total Boxplot")
  matplot(t(df_sani), type='l', main = 'Data', ylim=range(df_sani, na.rm = T))
  matplot(t(df_schz), type='l', main = 'Data', ylim=range(df_schz, na.rm = T))
}

{
  x11()
  par(mfrow=c(2,3))
  boxplot(df_sani, main = "Contrl Boxplot", center = T)
  boxplot(df_schz, main = "Schz Boxplot", center = T)
  boxplot( tempi.iti~ITI.label, main = "Total Boxplot")
  matplot(t(df_sani), type='l', main = 'Data', ylim=range(df_sani, na.rm = T))
  matplot(t(df_schz), type='l', main = 'Data', ylim=range(df_schz, na.rm = T))
}

iti.lm <- lm(tempi.iti ~ diagn_iti + ITI.label)
summary(iti.lm)
coef=iti.lm$coefficients

iti.lm <- lm(tempi.iti ~ diagn_iti + ITI.duration)
summary(iti.lm)
coef=iti.lm$coefficients
{x11()
  par(mfrow=c(1,2))
  plot(ITI.duration, tempi.iti, main="Retta di Regressione")
  abline(coef[1], coef[3], col="blue")
  abline(coef[1]+coef[2], coef[3], col="red")
  plot(ITI.duration[1:693], iti.lm$residuals, main="Residui vs valori fittati")
}
# gli errori sembrano omoschedastici

###############################################################################

#inizio con il modelllo Random Effects

iti.lmer <- lmer(tempi.iti ~ diagn_iti + (1|ITI.label) )
isSingular(iti.lmer, tol = 1e-8)

summary(iti.lmer)

confint(iti.lmer,oldNames=TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(iti.lmer)        
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(iti.lmer)), 5)
rownames(corb) <- nms

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(iti.lmer), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(iti.lmer))
sigma2_b <- as.numeric(get_variance_random(iti.lmer))

## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(iti.lmer)$sigma

A <- getME(iti.lmer, "A") # A  --> N x n, A represents the D (not italic)
I.n <- Diagonal(ncol(A)) # IN  --> n x n

## the conditional variance-covariance matrix of Y (diagonal matrix)
SigmaErr = sgma^2 * (I.n)
SigmaErr[3:6, 3:6]  ## visualization of individual 2
# Conditioned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic

## we visualize the first 20 rows/columns of the matrix
plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')

## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
V[3:6, 3:6]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 51% is very high!

dotplot(ranef(iti.lmer, condVar=T))

######################################################
# Prediction from mixed model on a test observation from a subject not present in the training set:

newiti = seq(from = 0.2, to = 1.2, by = (1.2-0.2)/20)
L=length(newiti)
newiti = c(newiti, newiti)
newdiagn_iti = c( rep("CONTROL", L), rep("SCHZ", L) )
color= c( rep("blue", L), rep("red", L) )
test.data= data.frame( ITI.duration = newiti, diagn_iti = newdiagn_iti)

# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(iti.lmer, newdata = test.data, re.form=NA)
predict_no_re # the same as before
{ x11()
  plot(ITI.duration, tempi.iti)
  points(newiti, predict_no_re, col=color)
}

# 2) With random effects
predict_re <- predict(fm16.1mer, newdata=test.data, allow.new.levels = T)
predict_re # the same as before, it uses the average of the random intercept, i.e. 0

#######################################################

iti.lmer <- lmer(tempi.iti ~ diagn_iti + (1|ITI.label))
summary(iti.lmer)

res=iti.lmer

############################################################

df4 = df[,c(1,4)]
df4$ITI.duration = as.numeric( df4$ITI.duration )
..pca.iti <- princomp(, scores=T)
pc.food
summary(pc.food)