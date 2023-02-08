
rm(list=ls())

setwd("G:/Il mio Drive/Brain Connectivity")

####

setwd("./materiale")
load("./workspaces/rec_2.RData")
load("./workspaces/recording_switch.RData")

{library(nlmeU)
  library(corrplot)
  library(nlme)
  library(lattice)
  library(plot.matrix)
  library(lme4)
  library(insight)
}
# Model 1. Random intercept, homoscedastic residuals

tempi = c(t_sani_switch,
               t_schz_switch,
               t_sani_noswitch,
               t_schz_noswitch)

switch.label = c(rep("SWITCH", 175), rep("NOSWITCH", 175))
diagn = c( rep("CONTROL", 125), rep("SCHZ", 50))
diagn = rep(diagn, 2)
#diagn = as.factor(diagn)

{df = matrix(nrow = length(diagn), ncol = 4, data = 0)
  df[,1] = tempi
  df[,2] = diagn
  df[,3] = switch.label
  df[,4] = elenco_copia$v
  df=as.data.frame(df)
  names(df) = c("tempi", "diagn", "switch.label", "subject")
  df
}

df = df[order(df$subject),]

lm.switch <- lm(tempi ~ diagn + switch.label)
summary(lm.switch)
summary(lm.switch)$sigma^2

coef=lm.switch$coefficients

###############################################################################

#inizio con il modello Random Effects

lmer.switch <- lmer(as.numeric(tempi) ~ diagn + switch.label + (1|subject), data = df)
summary(lmer.switch)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(lmer.switch)        
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(lmer.switch)), 5)
rownames(corb) <- nms

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(lmer.switch), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(lmer.switch))
sigma2_b <- as.numeric(get_variance_random(lmer.switch))

## Let's compute the condswitchonal and marginal var-cov matrix of Y
sgma <- summary(lmer.switch)$sigma

A <- getME(lmer.switch, "A") # A  --> N x n, A represents the D (not italic)
I.n <- Diagonal(ncol(A)) # IN  --> n x n

## the conditional variance-covariance matrix of Y (diagonal matrix)
SigmaErr = sgma^2 * (I.n)
SigmaErr[1:4, 1:4]  ## visualization of individual 1
# Condswitchoned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic

## we visualize the first 20 rows/columns of the matrix
plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Condswitchonal estimated Var-Cov matrix of Y')

## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
V[1:4, 1:4]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 84%

sigma2_eps # = 0.00564

{x11()
dot_object = dotplot(ranef(lmer.switch, condVar=T), by = diagn, ylab="Subjects",  )
dot_object
}

plot(ranef(lmer.switch, condVar=T), by = diagn, cex.Y.axis=0.1)


library(ggplot2)
{x11()
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), dati = ranef(lmer.switch, condVar=T)$subject[,1]),aes(x=Diagnosis,y=dati))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(text = element_text(size=20), legend.position = "", plot.title = element_text(hjust=0.5, size=17, face = "bold"))+
    xlab("") + ylab("") + labs(title = "Value of the Intercepts")
  pp
}

#AGGIUNGERE IL CONFRONOTO TRA SIGMA2EPS E LA SIGMA EPSIOLON DEL MODELLO ANOVA INIZIALE
#PERCHE COSI MOSTRIAMO CHE IL NOSTRO PPORCCIO HA FATTO IL SUO LAVORO, I RISULTATI RISPECCHIANO 
# CIO CHE CI ASPETTAVAMO RIGUARDO AL RANDOM EFFECT

#POI VABBE RINFORZRà LA TEORIA DELLA VASTA VARIABILITà

######################################################
# Prediction from mixed model on a test observation from a subject not present in the training set:

newswitch = seq(from = 0.2, to = 1.2, by = (1.2-0.2)/20)
L=length(newswitch)
newswitch = c(newswitch, newswitch)
newdiagn = c( rep("CONTROL", L), rep("SCHZ", L) )
color= c( rep("blue", L), rep("red", L) )
test.data= data.frame( switch.duration = newswitch, diagn = newdiagn)

# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(lmer.switch, newdata = test.data, re.form=NA)
predict_no_re # the same as before
{ x11()
  plot(switch.duration, tempi)
  points(newswitch, predict_no_re, col=color)
}

# 2) With random effects
predict_re <- predict(fm16.1mer, newdata=test.data, allow.new.levels = T)
predict_re # the same as before, it uses the average of the random intercept, i.e. 0

#######################################################

lmer.switch <- lmer(tempi ~ diagn + (1|switch.label))
summary(lmer.switch)

res=lmer.switch

############################################################

df4 = df[,c(1,4)]
df4$switch.duration = as.numeric( df4$switch.duration )
..pca.switch <- princomp(, scores=T)
pc.food
summary(pc.food)

############################################################

#uso il CorCompSym se ipotizzo che nei singoli gruppi i soggetti siano corellati da una correlazione costante
lmer.switch <- formula(as.numeric(tempi) ~ diagn + switch.label + (1|subject), data = df)
lm1.form <- formula(visual ~ -1 + visual0 + time.f + treat.f:time.f )
fm12.1 <- gls(lmer.switch,
              correlation = corCompSymm(form = ~1|subject), data = df)
summary(fm12.1)
