rm(list=ls())

setwd("G:/Il mio Drive/Brain Connectivity")

####

setwd("./materiale")
load("./workspaces/times.RData")
load("./workspaces/recording_csi.RData")

{library(nlmeU)
  library(corrplot)
  library(nlme)
  library(lattice)
  library(plot.matrix)
  library(lme4)
  library(insight)
}

# Model 1. Random intercept, homoscedastic residuals

tempi.csi = c( t_sani_CsiShort, t_schz_CsiShort, t_sani_CsiLong, t_schz_CsiLong)

CSI.label = c(rep("SHORT", 175), rep("LONG", 175))
CSI.duration= c(rep(0.2, 175), rep(1.2, 175)) 
diagn_csi = c( rep("CONTROL", 125), rep("SCHZ", 50))
diagn_csi = c(diagn_csi, diagn_csi)
#diagn_csi = as.factor(diagn_csi)

{df = matrix(nrow = 350, ncol = 4, data = 0)
df[,1] = tempi.csi
df[,2] = diagn_csi
df[,3] = CSI.label
df[,4] = CSI.duration
df=as.data.frame(df)
names(df) = c("tempi.csi", "diagn_csi", "CSI.label", "CSI.duration")
df
}

{x11()
boxplot( tempi.csi ~ CSI.label + diagn_csi, xlab="")
}

csi.lm <- lm(tempi.csi ~ diagn_csi + CSI.label)
summary(csi.lm)
summary(csi.lm)$sigma^2
coef=csi.lm$coefficients

csi.lm <- lm(tempi.csi ~ diagn_csi + CSI.duration)
summary(csi.lm)
coef=csi.lm$coefficients
{x11()
  par(mfrow=c(1,2))
  plot(CSI.duration, tempi.csi, main="Retta di Regressione")
  abline(coef[1], coef[3], col="blue")
  abline(coef[1]+coef[2], coef[3], col="red")
  plot(CSI.duration, csi.lm$residuals, main="Residui vs valori fittati")
}
# gli errori sembrano omoschedastici
#bartlett()

#anova()

###############################################################################

#inizio con il modello Random Effects

csi.lmer <- lmer(tempi.csi ~ diagn_csi + CSI.label + (1|CSI.label))
summary(csi.lmer)

#confint(csi.lmer,oldNames=TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(csi.lmer)        
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(csi.lmer)), 5)
rownames(corb) <- nms

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(csi.lmer), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(csi.lmer))
sigma2_b <- as.numeric(get_variance_random(csi.lmer))

## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(csi.lmer)$sigma

A <- getME(csi.lmer, "A") # A  --> N x n, A represents the D (not italic)
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

x11()
dotplot(ranef(csi.lmer, condVar=T))

######################################################
# Prediction from mixed model on a test observation from a subject not present in the training set:

newcsi = seq(from = 0.2, to = 1.2, by = (1.2-0.2)/20)
L=length(newcsi)
newcsi = c(newcsi, newcsi)
newdiagn_csi = c( rep("CONTROL", L), rep("SCHZ", L) )
color= c( rep("blue", L), rep("red", L) )
test.data= data.frame( CSI.duration = newcsi, diagn_csi = newdiagn_csi)

# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(csi.lmer, newdata = test.data, re.form=NA)
predict_no_re # the same as before
{ x11()
plot(CSI.duration, tempi.csi)
points(newcsi, predict_no_re, col=color)
}

# 2) With random effects
predict_re <- predict(fm16.1mer, newdata=test.data, allow.new.levels = T)
predict_re # the same as before, it uses the average of the random intercept, i.e. 0

#######################################################

csi.lmer <- lmer(tempi.csi ~ diagn_csi + (1|CSI.label))
summary(csi.lmer)

res=csi.lmer

############################################################