
setwd('G:/Il mio Drive/Brain Connectivity/')

###

setwd("./materiale")
load("./workspaces/zmap.RData")
elenco_copia <- read.delim("./events recording/elenco_copia.txt", header=T)
elenco <- read.delim("./events recording/elenco.txt", header=T)
p <- read.delim("./data/participants.csv")

library(class)
library(MASS)

##############################################################################################

diagnosis <- factor(p$diagnosis)

g=2 

i1 <- which(diagnosis=='CONTROL')
i2 <- which(diagnosis=='SCHZ')

n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2

m <-  colMeans(con_reg, na.rm = T)
m1 <- colMeans(con_reg[i1,], na.rm = T)
m2 <- colMeans(con_reg[i2,], na.rm = T)

S1 <- cov(con_reg[i1,])
S2 <- cov(con_reg[i2,])
Sp  <- ((n1-1)*S1+(n2-1)*S2)/(n-g)

# One-way MANOVA (See LAB 8)
fit <- manova(as.matrix(con_reg) ~ diagnosis)
summary.manova(fit,test="Wilks")

summary.aov(fit)

# Linear Discriminant Analysis (LDA)
lda.conreg <- lda(con_reg, diagnosis)

Lda.conreg <- predict(lda.conreg, con_reg)

# Estimate of AER (actual error rate):

table(class.true=diagnosis, class.assigned=Lda.conreg$class)

errors <- (Lda.conreg$class != diagnosis)
APER   <- sum(errors)/length(diagnosis)
APER
# 0.09

# Remark: this is correct only if we estimate the prior with the empirical  
#         frequencies!  
# e per noi va bene visto che abbiamo sceltoa mno il umero di sani e di schizo

LdaCV.conreg <- lda(con_reg, diagnosis, CV=TRUE)  # specify the argument CV
table(class.true=diagnosis, class.assignedCV=LdaCV.conreg$class)
errorsCV <- (LdaCV.conreg$class != diagnosis)

AERCV   <- sum(errorsCV)/length(diagnosis)
AERCV

##############################################################################################

scores3 = scores[,1:3]
diagnosis <- factor(p$diagnosis)

g=2 

i1 <- which(diagnosis=='CONTROL')
i2 <- which(diagnosis=='SCHZ')

n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2

m <-  colMeans(scores3, na.rm = T)
m1 <- colMeans(scores3[i1,], na.rm = T)
m2 <- colMeans(scores3[i2,], na.rm = T)

S1 <- cov(scores3[i1,])
S2 <- cov(scores3[i2,])
Sp  <- ((n1-1)*S1+(n2-1)*S2)/(n-g)

# One-way MANOVA (See LAB 8)
fit <- manova(as.matrix(scores3) ~ diagnosis)
summary.manova(fit,test="Wilks")

summary.aov(fit)

# Linear Discriminant Analysis (LDA)
lda.conreg <- lda(scores3, diagnosis)

Lda.conreg <- predict(lda.conreg, scores3)

# Estimate of AER (actual error rate):

table(class.true=diagnosis, class.assigned=Lda.conreg$class)

errors <- (Lda.conreg$class != diagnosis)
APER   <- sum(errors)/length(diagnosis)
APER
# 0.09

# Remark: this is correct only if we estimate the prior with the empirical  
#         frequencies!  
# e per noi va bene visto che abbiamo sceltoa mno il umero di sani e di schizo

LdaCV.conreg <- lda(scores3, diagnosis, CV=TRUE)  # specify the argument CV
table(class.true=diagnosis, class.assignedCV=LdaCV.conreg$class)
errorsCV <- (LdaCV.conreg$class != diagnosis)

AERCV   <- sum(errorsCV)/length(diagnosis)
AERCV

colors=c(rep("blue", 125), rep("red", 50))
library(rgl)

{points3d(scores3, col=colors)
box3d()
}

##############################################################################################

pc3 = scores[,3]

m <-  mean(pc3, na.rm = T)
m1 <- mean(pc3[i1,], na.rm = T)
m2 <- mean(pc3[i2,], na.rm = T)

S1 <- cov(pc3[i1,])
S2 <- cov(pc3[i2,])
Sp  <- ((n1-1)*S1+(n2-1)*S2)/(n-g)

# One-way MANOVA (See LAB 8)
fit <- manova(as.matrix(pc3) ~ diagnosis)
summary.manova(fit,test="Wilks")

summary.aov(fit)

# Linear Discriminant Analysis (LDA)
lda.conreg <- lda(pc3, diagnosis)

Lda.conreg <- predict(lda.conreg, pc3)

# Estimate of AER (actual error rate):

table(class.true=diagnosis, class.assigned=Lda.conreg$class)

errors <- (Lda.conreg$class != diagnosis)
APER   <- sum(errors)/length(diagnosis)
APER
# 0.09

# Remark: this is correct only if we estimate the prior with the empirical  
#         frequencies!  
# e per noi va bene visto che abbiamo sceltoa mno il umero di sani e di schizo

LdaCV.conreg <- lda(pc3, diagnosis, CV=TRUE)  # specify the argument CV
table(class.true=diagnosis, class.assignedCV=LdaCV.conreg$class)
errorsCV <- (LdaCV.conreg$class != diagnosis)

AERCV   <- sum(errorsCV)/length(diagnosis)
AERCV

#########################################################################################

