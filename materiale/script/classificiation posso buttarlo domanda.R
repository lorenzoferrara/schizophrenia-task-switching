
#_______________________________________________________________________________
### Example 2 (3 classes, bivariate)
###-------------------------------------------

dataset = as.data.frame(scores)
dataset$diagnosis = c(rep("Control", 125), rep("Schz", 50))
head(dataset)

diagnosis.name <- factor( dataset$diagnosis, labels=c('Control','Schz'))

g=2 
p=10

i1 <- which(diagnosis.name=='Control')
i2 <- which(diagnosis.name=='Schz')

n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2

dataset_cov <- dataset[,1:p]

# Jittering
set.seed(1)
dataset_cov <- dataset_cov + cbind(rnorm(150, sd=0.025))    # jittering, SMALL ERROR

# plot the data
{x11()
  plot(dataset_cov, main='dataset Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=19)
  points(dataset_cov[i1,], col='red', pch=19)
  points(dataset_cov[i2,], col='green', pch=19)
  legend("topright", legend=levels(diagnosis.name), fill=c('red','green','blue'))
}
dev.off()

m <-  colMeans(dataset_cov)
m1 <- colMeans(dataset_cov[i1,])
m2 <- colMeans(dataset_cov[i2,])

S1 <- cov(dataset_cov[i1,])
S2 <- cov(dataset_cov[i2,])
Sp  <- ((n1-1)*S1+(n2-1)*S2/(n-g))

# One-way MANOVA
fit <- manova(as.matrix(dataset_cov) ~ diagnosis.name)
summary.manova(fit,test="Wilks")

# Linear Discriminant Analysis (LDA)
lda.dataset <- lda(dataset_cov, diagnosis.name)
lda.dataset

# "coefficients of linear discriminants" and "proportion of trace":
# Fisher discriminant analysis. 
# In particular:
# - coefficients of linear discriminants: versors of the canonical directions
#   [to be read column-wise]
# - proportion of trace: proportion of variance explained by the corresponding 
#   canonical direction

Lda.dataset <- predict(lda.dataset, dataset_cov)
names(Lda.dataset)

# Estimate of AER (actual error rate):
# 1) APER (apparent error rate)
# 2) estimate of AER by cross-validation

# 1) Compute the APER
Lda.dataset$class   # assigned classes
diagnosis.name     # true labels
table(class.true=diagnosis.name, class.assigned=Lda.dataset$class)

errors <- (Lda.dataset$class != diagnosis.name)
errors
sum(errors)
length(diagnosis.name)

APER   <- sum(errors)/length(diagnosis.name)
APER

(3+43)/175

# Remark: this is correct only if we estimate the prior with the empirical  
#         frequencies! Otherwise:
# prior <- c(1/3,1/3,1/3)
# G <- 3
# misc <- table(class.true=diagnosis.name, class.assigned=Lda.dataset$class)
# APER <- 0
# for(g in 1:G)
#   APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  

# 2) Compute the estimate of the AER by leave-one-out cross-validation 

### Recall:

errors_CV <- 0
for(i in 1:150){
  LdaCV.i <- lda(dataset_cov[-i,], diagnosis.name[-i], prior=c(50,50)/100)
  errors_CV <- errors_CV + as.numeric(predict(LdaCV.i,dataset_cov[i,])$class != diagnosis.name[i])
}
errors_CV

AERCV   <- sum(errors_CV)/length(diagnosis.name)
AERCV

####

# with R:
LdaCV.dataset <- lda(dataset_cov, diagnosis.name, CV=TRUE)  # specify the argument CV

LdaCV.dataset$class
diagnosis.name
table(class.true=diagnosis.name, class.assignedCV=LdaCV.dataset$class)

errorsCV <- (LdaCV.dataset$class != diagnosis.name)
errorsCV
sum(errorsCV)

AERCV   <- sum(errorsCV)/length(diagnosis.name)
AERCV
# Remark: correct only if we estimate the priors through the sample frequencies!


# Plot the partition induced by LDA

{x11()
  plot(dataset_cov, main='dataset Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
  points(dataset_cov[i1,], col='red', pch=20)
  points(dataset_cov[i2,], col='green', pch=20)
  points(dataset_cov[i3,], col='blue', pch=20)
  legend("topright", legend=levels(diagnosis.name), fill=c('red','green','blue'), cex=.7)
  
  points(lda.dataset$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
}
x  <- seq(min(dataset[,1]), max(dataset[,1]), length=200)
y  <- seq(min(dataset[,2]), max(dataset[,2]), length=200)
xy <- expand.grid(Sepal.Length=x, Sepal.Width=y)

z  <- predict(lda.dataset, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2], z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1], z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
z3 <- z[,3] - pmax(z[,1], z[,2])  # P_3*f_3(x,y)-max{P_j*f_j(x,y)}

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)

library(rgl)
library(mvtnorm)
open3d()
points3d(dataset_cov[i1,1], dataset_cov[i1,2], 0, col='red', pch=15)
points3d(dataset_cov[i2,1], dataset_cov[i3,2], 0, col='green', pch=15)
points3d(dataset_cov[i3,1], dataset_cov[i2,2], 0, col='blue', pch=15)
surface3d(x,y,matrix(dmvnorm(xy, m1, Sp) / 3, 50), alpha=0.4, color='red')
surface3d(x,y,matrix(dmvnorm(xy, m2, Sp) / 3, 50), alpha=0.4, color='green', add=T)
surface3d(x,y,matrix(dmvnorm(xy, m3, Sp) / 3, 50), alpha=0.4, color='blue', add=T)
box3d()


### Quadratic Discriminand Analysis (QDA)
###---------------------------------------

qda.dataset <- qda(dataset_cov, diagnosis.name)
qda.dataset

Qda.dataset <- predict(qda.dataset, dataset_cov)

# compute the APER
Qda.dataset$class
diagnosis.name
table(class.true=diagnosis.name, class.assigned=Qda.dataset$class)

errorsq <- (Qda.dataset$class != diagnosis.name)
errorsq

APERq   <- sum(errorsq)/length(diagnosis.name)
APERq

# Remark: correct only if we estimate the priors through the sample frequencies!

# Compute the estimate of the AER by leave-out-out cross-validation 
QdaCV.dataset <- qda(dataset_cov, diagnosis.name, CV=T)
QdaCV.dataset$class
diagnosis.name
table(class.true=diagnosis.name, class.assignedCV=QdaCV.dataset$class)

errorsqCV <- (QdaCV.dataset$class != diagnosis.name)
errorsqCV

AERqCV   <- sum(errorsqCV)/length(diagnosis.name)
AERqCV
# Remark: correct only if we estimate the priors through the sample frequencies!


#LDA IS THE BEST OPTION IF THE COVARIANCES ARE SIMILAR/EQUAL
# QDA IS BETTER WHEN THE COVARIANCES ARE DIFFERENT


# Plot the partition induced by QDA
{x11()
  plot(dataset_cov, main='dataset Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
  points(dataset_cov[i1,], col='red', pch=20)
  points(dataset_cov[i2,], col='green', pch=20)
  points(dataset_cov[i3,], col='blue', pch=20)
  legend("topright", legend=levels(diagnosis.name), fill=c('red','green','blue'))
  
  points(qda.dataset$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)
  
  x  <- seq(min(dataset[,1]), max(dataset[,1]), length=200)
  y  <- seq(min(dataset[,2]), max(dataset[,2]), length=200)
  xy <- expand.grid(Sepal.Length=x, Sepal.Width=y)
  
  z  <- predict(qda.dataset, xy)$post  
  z1 <- z[,1] - pmax(z[,2], z[,3])    
  z2 <- z[,2] - pmax(z[,1], z[,3])    
  z3 <- z[,3] - pmax(z[,1], z[,2])
  
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
}

{library(rgl)
  open3d()
  points3d(dataset_cov[i1,1], dataset_cov[i1,2], 0, col='red', pch=15)
  points3d(dataset_cov[i2,1], dataset_cov[i3,2], 0, col='green', pch=15)
  points3d(dataset_cov[i3,1], dataset_cov[i2,2], 0, col='blue', pch=15)
  surface3d(x,y,matrix(dmvnorm(xy, m1, S1) / 3, 50), alpha=0.4, color='red')
  surface3d(x,y,matrix(dmvnorm(xy, m2, S2) / 3, 50), alpha=0.4, color='green', add=T)
  surface3d(x,y,matrix(dmvnorm(xy, m3, S3) / 3, 50), alpha=0.4, color='blue', add=T)
  box3d()
}



### knn-classifier
###----------------
# Plot the partition induced by knn

k <- 7

{x11()
  plot(dataset_cov, main='dataset.Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
  points(dataset_cov[i1,], col=2, pch=20)
  points(dataset_cov[i3,], col=4, pch=20)
  points(dataset_cov[i2,], col=3, pch=20)
  legend("topright", legend=levels(diagnosis.name), fill=c(2,3,4))
  
  x  <- seq(min(dataset[,1]), max(dataset[,1]), length=200)
  y  <- seq(min(dataset[,2]), max(dataset[,2]), length=200)
  xy <- expand.grid(Sepal.Length=x, Sepal.Width=y)
  
  dataset.knn <- knn(train = dataset_cov, test = xy, cl = dataset$diagnosis, k = k)
  
  z  <- as.numeric(dataset.knn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}
graphics.off()

#_______________________________________________________________________________
##### Problem 2 of 9/09/2009
#####--------------------------
# The vending machines of the Exxon fuel contain optical detectors able
# to measure the size of the banknotes inserted. Knowing that 0.1% of the 
# 10$ banknotes in circulation are counterfeit, Exxon would like to implement a
# software to identify false 10$ banknotes, as to minimize the economic losses. 
# Assuming that:
#  - both the populations of real and false banknotes follow a normal 
#    distribution (with different mean and covariance matrices);
#  - accepting a false banknote leads to an economic loss of 10$;
#  - rejecting a true banknote brings a economic loss quantifiable in 5 cents;
# satisfy the following requests of the Exxon:
# a) build an appropriate classifier, estimating the unknown parameters
#    starting from the two datasets moneytrue.txt and moneyfalse.txt, containing
#    data about 100 true banknotes and 100 counterfeit banknotes (in mm). 
#    Qualitatively show the two classification regions in a graph;
# b) calculate the APER of the classifier and, based on the APER, estimate the 
#    expected economic damage of the classifier;
# c) what is the estimated probability that the first 10$ banknote inserted in the
#    machine is rejected? 

true <- read.table('moneytrue.txt',header=TRUE)
false <- read.table('moneyfalse.txt',header=TRUE)

banknotes <- rbind(true,false)
vf <- factor(rep(c('true','false'),each=100), levels=c('true','false'))

{x11()
  plot(banknotes[,1:2], main='Banknotes', xlab='V1', ylab='V2', pch=20)
  points(false, col='red', pch=20)
  points(true, col='blue', pch=20)
  legend('bottomleft', legend=levels(vf), fill=c('blue','red'), cex=.7)
}

# question a)
mcshapiro.test(true)
mcshapiro.test(false)

# misclassification costs
c.vf <- 10
c.fv <- 0.05

#prior probabilities
pf <- 0.001
pt <- 1-0.001

# Prior modified to account for the misclassification costs
prior.c <- c(pt*c.fv/(c.vf*pf+c.fv*pt), pf*c.vf/(c.vf*pf+c.fv*pt))
prior.c

# QDA
library(MASS)
qda.m <- qda(banknotes, vf, prior=prior.c)
qda.m

{x11()
  plot(banknotes[,1:2], main='Banknotes', xlab='V1', ylab='V2', pch=20)
  points(false, col='red', pch=20)
  points(true, col='blue', pch=20)
  legend('bottomleft', legend=levels(vf), fill=c('blue','red'), cex=.7)
  
  points(qda.m$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)
  
  x  <- seq(min(banknotes[,1]), max(banknotes[,1]), length=200)
  y  <- seq(min(banknotes[,2]), max(banknotes[,2]), length=200)
  xy <- expand.grid(V1=x, V2=y)
  
  z  <- predict(qda.m, xy)$post  
  z1 <- z[,1] - z[,2] 
  z2 <- z[,2] - z[,1]  
  
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  
  m1 <- colMeans(true)
  m2 <- colMeans(false)
  
  S1 <- cov(true)
  S2 <- cov(false)
  
  library(rgl)
  open3d()
  points3d(true[,1], true[,2], 0, col='blue', pch=15)
  points3d(false[,1], false[,2], 0, col='red', pch=15)
  surface3d(x,y,matrix(dmvnorm(xy, m1, S1)*prior.c[1]/100 , 200), alpha=0.6, color='blue')
  surface3d(x,y,matrix(dmvnorm(xy, m2, S2)*prior.c[2]/100 , 200), alpha=0.6, color='red', add=T)
  box3d()
  
  # question b)
  
  # APER
  Qda.m <- predict(qda.m)
  table(class.true=vf, class.assigned=Qda.m$class)
  
  APER  <- (2*pt+80*pf)/100
  APER
  
  # Expected economic loss:
  2/100*pt*c.fv+80/100*pf*c.vf
  
  # question c)
  # P[rejected] = P[rejected | true]P[true] + P[rejected | false]P[false]
  2/100*pt + 20/100*pf
  
  
  ###-------------------------------------------------------------------------
  ### FISHER DISCRIMINANT ANALYSIS
  ###-----------------------------------
  ### Let's change viewpoint: we look for the directions that highlight
  ### the discrimination among groups
  ### -> we look for the canonical directions
  
  # Remark. Assumptions: homogeneity of the covariance structure
  # [we relax the normality assumption]
  
  # Let's consider again the dataset dataset
  attach(dataset)
  
  diagnosis.name <- factor(diagnosis, labels=c('setosa','versicolor','virginica'))
  
  g <- 3 
  
  i1 <- which(diagnosis.name=='setosa')
  i2 <- which(diagnosis.name=='versicolor')
  i3 <- which(diagnosis.name=='virginica')
  
  n1 <- length(i1)
  n2 <- length(i2)
  n3 <- length(i3)
  n <- n1+n2+n3
  
  detach(dataset)
  
  dataset_cov <- dataset[,1:2]
  head(dataset_cov)
  
  set.seed(1)
  dataset_cov <- dataset_cov + cbind(rnorm(150, sd=0.025))    # jittering
  
  m <-  colMeans(dataset_cov)
  m1 <- colMeans(dataset_cov[i1,])
  m2 <- colMeans(dataset_cov[i2,])
  m3 <- colMeans(dataset_cov[i3,])
  
  S1 <- cov(dataset_cov[i1,])
  S2 <- cov(dataset_cov[i2,])
  S3 <- cov(dataset_cov[i3,])
  Sp  <- ((n1-1)*S1+(n2-1)*S2+(n3-1)*S3)/(n-g)
  
  # covariance between groups (estimate)
  B <- 1/g*(cbind(m1 - m) %*% rbind(m1 - m) +
              cbind(m2 - m) %*% rbind(m2 - m) +
              cbind(m3 - m) %*% rbind(m3 - m))
  B
  
  # covariance within groups (estimate)
  Sp
  
  # how many coordinates?
  g <- 3
  p <- 2
  s <- min(g-1,p)
  s
  
  # Matrix Sp^(-1/2)
  val.Sp <- eigen(Sp)$val
  vec.Sp <- eigen(Sp)$vec
  invSp.2 <- 1/sqrt(val.Sp[1])*vec.Sp[,1]%*%t(vec.Sp[,1]) + 1/sqrt(val.Sp[2])*vec.Sp[,2]%*%t(vec.Sp[,2])
  invSp.2 
  
  # spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
  spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)
  
  # first canonical coordinate
  a1 <- invSp.2 %*% spec.dec$vec[,1]
  a1
  
  # second canonical coordinate
  a2 <- invSp.2 %*% spec.dec$vec[,2]
  a2
  
  # compare with the output of lda():
  lda.dataset <- lda(dataset_cov, diagnosis.name)
  lda.dataset
  a1
  a2
  spec.dec$val/sum(spec.dec$val)
  
  ### How are the data classified?
  # Compute the canonical coordinates of the data     ?? COSA SONO?
  
  cc1.dataset <- as.matrix(dataset_cov)%*%a1
  cc2.dataset <- as.matrix(dataset_cov)%*%a2
  
  coord.cc <- cbind(cc1.dataset,cc2.dataset)
  
  # Compute the coordinates of the mean within groups along the canonical directions
  cc.m1 <- c(m1%*%a1, m1%*%a2)
  cc.m2 <- c(m2%*%a1, m2%*%a2)
  cc.m3 <- c(m3%*%a1, m3%*%a2)
  
  # Assign data to groups
  f.class=rep(0, n)
  for(i in 1:n) # for each datum
  {
    # Compute the Euclidean distance of the i-th datum from mean within the groups
    dist.m=c(d1=sqrt(sum((coord.cc[i,]-cc.m1)^2)),
             d2=sqrt(sum((coord.cc[i,]-cc.m2)^2)),
             d3=sqrt(sum((coord.cc[i,]-cc.m3)^2)))
    # Assign the datum to the group whose mean is the nearest
    f.class[i]=which.min(dist.m)
  }
  f.class
  table(class.true=diagnosis.name, class.assigned=f.class)
  
  errors <- 150 - sum(diag(table(class.true=diagnosis.name, class.assigned=f.class)))
  errors
  length(diagnosis.name)
  
  APERf   <- errors/length(diagnosis.name)
  APERf
  
  ### How do I classify a new observation?
  x.new <- c(5.85, 2.90)
  # compute the canonical coordinates
  cc.new <- c(x.new%*%a1, x.new%*%a2)
  # compute the distance from the means
  dist.m <- c(d1=sqrt(sum((cc.new-cc.m1)^2)),
              d2=sqrt(sum((cc.new-cc.m2)^2)),
              d3=sqrt(sum((cc.new-cc.m3)^2)))
  # assign to the nearest mean
  which.min(dist.m)
  
  color.diagnosis=rep(c('red','green','blue'), each=50)
  
  # visually
  {x11(width=14, height=7)
    par(mfrow=c(1,2))
    plot(dataset_cov[,1], dataset_cov[,2], main='Plane of original coordinates', 
         xlab='Sepal.Length', ylab='Sepal.Width', pch=20, col=as.character(color.diagnosis))
    legend("topleft", legend=levels(diagnosis.name), fill=c('red','green','blue'), cex=.7)
    points(x.new[1], x.new[2], col='gold', pch=19)
    points(m1[1], m1[2], pch=4,col='red' , lwd=2, cex=1.5)
    points(m2[1], m2[2], pch=4,col='green' , lwd=2, cex=1.5)
    points(m3[1], m3[2], pch=4,col='blue' , lwd=2, cex=1.5)
    
    plot(cc1.dataset, cc2.dataset, main='Plane of canonical coordinates', xlab='first canonical coordinate', ylab='second canonical coordinate', pch=20, col=as.character(color.diagnosis))
    legend("topleft", legend=levels(diagnosis.name), fill=c('red','green','blue'), cex=.7)
    
    points(cc.m1[1], cc.m1[2], pch=4,col='red' , lwd=2, cex=1.5)
    points(cc.m2[1], cc.m2[2], pch=4,col='green' , lwd=2, cex=1.5)
    points(cc.m3[1], cc.m3[2], pch=4,col='blue' , lwd=2, cex=1.5)
    
    points(cc.new[1], cc.new[2], col='gold', pch=19)
    
    segments(cc.m1[1], cc.m1[2], cc.new[1], cc.new[2])
    segments(cc.m2[1], cc.m2[2], cc.new[1], cc.new[2])
    segments(cc.m3[1], cc.m3[2], cc.new[1], cc.new[2])
  }
  dev.off()
  
  ### We plot the partition generated by the canonical coordinates
  color.diagnosis <- diagnosis.name
  levels(color.diagnosis) <- c('red','green','blue')
  {
    x11()
    plot(cc1.dataset, cc2.dataset, main='Fisher discriminant analysis', xlab='first canonical coordinate', ylab='second canonical coordinate', pch=20, col=as.character(color.diagnosis))
    legend("topleft", legend=levels(diagnosis.name), fill=c('red','green','blue'), cex=.7)
    
    points(cc.m1[1], cc.m1[2], pch=4,col='red' , lwd=2, cex=1.5)
    points(cc.m2[1], cc.m2[2], pch=4,col='green' , lwd=2, cex=1.5)
    points(cc.m3[1], cc.m3[2], pch=4,col='blue' , lwd=2, cex=1.5)
    
    x.cc  <- seq(min(cc1.dataset),max(cc1.dataset),len=200)
    y.cc  <- seq(min(cc2.dataset),max(cc2.dataset),len=200)
    xy.cc <- expand.grid(cc1=x.cc, cc2=y.cc)
    
    z  <- cbind( sqrt(rowSums(scale(xy.cc,cc.m1,scale=FALSE)^2)), sqrt(rowSums(scale(xy.cc,cc.m2,scale=FALSE)^2)), sqrt(rowSums(scale(xy.cc,cc.m3,scale=FALSE)^2)))
    z1.cc <- z[,1] - pmin(z[,2], z[,3])    
    z2.cc <- z[,2] - pmin(z[,1], z[,3])    
    z3.cc <- z[,3] - pmin(z[,1], z[,2])
    
    contour(x.cc, y.cc, matrix(z1.cc, 200), levels=0, drawlabels=F, add=T)
    contour(x.cc, y.cc, matrix(z2.cc, 200), levels=0, drawlabels=F, add=T)
    contour(x.cc, y.cc, matrix(z3.cc, 200), levels=0, drawlabels=F, add=T)
  }
  dev.off()
  
  # Plot LDA
  {x11()
    plot(dataset_cov, main='dataset Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
    points(dataset_cov[i1,], col='red', pch=20)
    points(dataset_cov[i2,], col='green', pch=20)
    points(dataset_cov[i3,], col='blue', pch=20)
    legend("topright", legend=levels(diagnosis.name), fill=c('red','green','blue'), cex=.7)
    
    points(rbind(m1,m2,m3), pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
    
    x  <- seq(min(dataset[,1]), max(dataset[,1]), length=200)
    y  <- seq(min(dataset[,2]), max(dataset[,2]), length=200)
    xy <- expand.grid(Sepal.Length=x, Sepal.Width=y)
    
    z  <- predict(lda.dataset, xy)$post  # these are P_i*f_i(x,y)  
    z1 <- z[,1] - pmax(z[,2], z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
    z2 <- z[,2] - pmax(z[,1], z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
    z3 <- z[,3] - pmax(z[,1], z[,2])  # P_3*f_3(x,y)-max{P_j*f_j(x,y)}
    
    # Plot the contour line of level (levels=0) of z1, z2, z3: 
    # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
    # where j realizes the max.
    contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
    contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
    contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
    
  }
  dev.off()
  
  ###########################################################################
  # Plot of the projections on the canonical directions (not orthogonal!)
  
  {x11()
    plot(dataset_cov, main='Projection on the canonical directions', xlab='Sepal.Length', ylab='Sepal.Width', pch=20, xlim=c(-3,8), ylim=c(-3,7))
    points(dataset_cov[i1,], col='red', pch=20)
    points(dataset_cov[i2,], col='green', pch=20)
    points(dataset_cov[i3,], col='blue', pch=20)
    legend('topleft', legend=levels(diagnosis.name), fill=c('red','green','blue'), cex=.7)
    
    points(rbind(m1,m2,m3), pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
    
    abline(h=0,v=0, col='grey35')
    
    arrows(x0=0, y0=0, x1=a1[1], y1=a1[2], length=.1)
    arrows(x0=0, y0=0, x1=a2[1], y1=a2[2], length=.1)
    
    text(a1[1], a1[2], 'a1',pos=3)
    text(a2[1], a2[2], 'a2',pos=2)
    
    abline(coef=c(0,(a1[2]/a1[1])), col='grey55',lty=2)
    abline(coef=c(0,(a2[2]/a2[1])), col='grey55',lty=2)
    
    points(cc1.dataset*a1[1]/(sum(a1^2)),cc1.dataset*a1[2]/(sum(a1^2)),col=as.character(color.diagnosis))
    points(cc2.dataset*a2[1]/(sum(a2^2)),cc2.dataset*a2[2]/(sum(a2^2)),col=as.character(color.diagnosis))
  }
  {
    x11()
    plot(cc1.dataset, cc2.dataset, main='Coordinate system of the canonical coordinates', xlab='first canonical coordinate', ylab='second canonical coordinate', pch=20, col=as.character(color.diagnosis))
    legend('topleft', legend=levels(diagnosis.name), fill=c('red','green','blue'), cex=.7)
    
    cc.m1 <- c(m1%*%a1, m1%*%a2)
    cc.m2 <- c(m2%*%a1, m2%*%a2)
    cc.m3 <- c(m3%*%a1, m3%*%a2)
    
    points(cc.m1[1], cc.m1[2], pch=4,col='red' , lwd=2, cex=1.5)
    points(cc.m2[1], cc.m2[2], pch=4,col='green' , lwd=2, cex=1.5)
    points(cc.m3[1], cc.m3[2], pch=4,col='blue' , lwd=2, cex=1.5)
    
    graphics.off()
    
    
    
    ################################################################################################
    ### Support Vector Machines
    
    library(e1071)
    help(svm)
    
    ###########
    ### Linear case
    
    # Generate the data
    set.seed (123)
    x <- matrix (rnorm (20*2) , ncol =2)
    y <- c(rep (-1,10) , rep (1 ,10) )
    x[y==1,] <- x[y==1,] + 1
    
    # The classes are not separable
    {x11()
      plot(x, col =ifelse(y==1, 'blue', 'red'), 
           pch=19, xlab='x1', ylab='x2', asp=1)
    }
    # Fit the Support Vector Classifier (kernel = "linear")
    # given a cost C
    dat <- data.frame(x=x, y=as.factor (y))
    svmfit <- svm(y~., data=dat , kernel ='linear', cost =10, scale =FALSE )
    summary(svmfit)
    
    {x11()
      par(mfrow=c(1,2))
      plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)
    }
    # support vectors are indicated with crosses
    # they are:
    svmfit$index
    
    ###
    n.g <- 100
    
    xgrid <- expand.grid(x.1=seq(from=range(dat$x.1)[1],to=range(dat$x.1)[2],length=n.g),
                         x.2=seq(from=range(dat$x.2)[1],to=range(dat$x.2)[2],length=n.g))
    ygrid <- predict(svmfit,xgrid)
    {x11()
      plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
      points(x,col=c("red","blue")[as.numeric(dat$y)],pch=19)
      points(x[svmfit$index,],pch=5,cex=2)
    }
    
    {x11()
      plot(x,col=c("red","blue")[as.numeric(dat$y)],pch=19)
      contour(seq(from=range(dat$x.1)[1],to=range(dat$x.1)[2],length=n.g),
              seq(from=range(dat$x.2)[1],to=range(dat$x.2)[2],length=n.g),
              matrix(as.numeric(ygrid),n.g,n.g),level=1.5,add=TRUE,
              drawlabels=F)
    }
    ###
    
    # If we try to change the cost parameter we get more support points
    # (higher bias, lower variance)
    
    svmfit <- svm(y~., data=dat , kernel ='linear', cost =0.1, scale =FALSE )
    x11()
    plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)
    
    # To set the parameter C we can use the function tune(),
    # which is based on cross-validation (10-fold)
    set.seed (1)
    tune.out <- tune(svm,y~.,data=dat ,kernel = 'linear',
                     ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
    summary(tune.out)
    
    # Extract the best model from the result of tune
    bestmod <- tune.out$best.model
    summary(bestmod)
    
    plot(bestmod , dat, col =c('salmon', 'light blue'), pch=19, asp=1)
    
    # Prediction for a new observation (command predict())
    xtest <- matrix(rnorm (20*2) , ncol =2)
    ytest <- sample(c(-1,1) , 20, rep=TRUE)
    xtest[ytest ==1 ,] <- xtest[ytest ==1,] + 1
    testdat <- data.frame(x=xtest , y=as.factor (ytest))
    
    plot(xtest, col =ifelse(ytest==1, 'light blue', 'salmon'), 
         pch=19, xlab='x1', ylab='x2', asp=1)
    
    ypred <- predict(bestmod,testdat)
    table(true.label=testdat$y, assigned.label =ypred )
    
    # If the classes are separable, setting a high value for the cost function
    # leads to the maximal margin classifier (i.e., it returns the classification
    # provided by the best separating hyperplane)
    
    diagnosis.name <- factor(dataset$diagnosis, labels=c('setosa','versicolor','virginica'))
    set.seed(1)
    dataset_cov <- dataset[,1:2] + cbind(rnorm(150, sd=0.025))    # jittering
    
    # setosa VS versicolor+virginica
    y <- rep(0,150)
    y[which(diagnosis.name=='setosa')] <- 1
    
    x11()
    plot(dataset_cov[,1], dataset_cov[,2], xlab='Sepal.Length', ylab='Sepal.Width', pch=20, col=as.character(y+1))
    
    dat <- data.frame(x=dataset_cov[,c(2,1)], y=as.factor (y))
    svmfit <- svm(y~., data=dat , kernel ='linear', cost =100, scale =FALSE )
    summary(svmfit)
    
    x11()
    par(mfrow=c(1,2))
    plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19)
    
    
    dat <- data.frame(x=dataset_cov[,c(2,1)], y=as.factor (y))
    svmfit <- svm(y~., data=dat , kernel ='linear', cost =1, scale =FALSE )
    summary(svmfit)
    
    x11()
    par(mfrow=c(1,2))
    plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19)
    
    
    ###########
    ### Non-linear case
    
    # Generate the data
    set.seed (1)
    x <- matrix (rnorm (200*2) , ncol =2)
    x[1:100 ,] <- x[1:100 ,]+2
    x[101:150 ,] <- x[101:150 ,] -2
    y <- c(rep (1 ,150) ,rep (2 ,50) )
    dat <- data.frame(x=x,y=as.factor (y))
    
    # The classes are not separable
    x11()
    plot(x, col =ifelse(y==1, 'salmon', 'light blue'), pch=19, xlab='x1', ylab='x2', asp=1)
    
    # Randomly split in train and test
    train <- sample (200 ,100)
    
    # Fit a Support Vector Machine (kernel = "radial") given a cost C
    svmfit <- svm(y~., data=dat [train ,], kernel ='radial', gamma =1, cost =1)
    summary(svmfit)
    
    # Plot the SVM
    x11()
    plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)
    
    # Misclassification error on the training set
    table(true=dat[train ,"y"], pred=predict (svmfit ,
                                              newdata =dat[train ,]))
    (3+4)/100
    
    # Misclassification error on the test set
    table(true=dat[-train ,"y"], pred=predict (svmfit ,
                                               newdata =dat[-train ,]))
    (3+10)/100
    
    # Increasing the cost decreases the errors on the training set,
    # at the expense of a more irregular boundary
    svmfit <- svm(y~., data=dat [train ,], kernel ='radial',gamma =1,cost=1e5)
    
    x11()
    plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)
    
    # Misclassification error on the training set
    table(true=dat[train ,"y"], pred=predict (svmfit ,
                                              newdata =dat[train ,]))
    0
    
    # Misclassification error on the test set
    table(true=dat[-train ,"y"], pred=predict (svmfit ,
                                               newdata =dat[-train ,]))
    (5+20)/100
    
    # Set parameters via CV (CROSS VALIDATION):
    set.seed (1)
    tune.out <- tune(svm , y~., data=dat[train ,], kernel ='radial',
                     ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                                  gamma=c(0.5,1,2,3,4) ))
    summary(tune.out)
    
    # Misclassification error with best model on the training set
    table(true=dat[train ,"y"], pred=predict (tune.out$best.model ,
                                              newdata =dat[train ,]))
    (2+4)/100
    
    # Misclassification error with best model on the test set
    table(true=dat[-train ,"y"], pred=predict (tune.out$best.model ,
                                               newdata =dat[-train ,]))
    (2+10)/100
    
    
    
    ###########
    ### Application to Gene Expression Data (multiclass classification)
    library (ISLR)
    
    help(Khan)
    
    is(Khan)
    names(Khan)
    
    dim(Khan$xtrain)
    dim(Khan$xtest)
    length (Khan$ytrain)
    length (Khan$ytest)
    
    table(Khan$ytrain)
    table(Khan$ytest)
    
    dat <- data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain))
    out <- svm(y~., data=dat , kernel ="linear",cost =10)
    summary (out)
    
    table(out$fitted , dat$y)
    
    dat.te <- data.frame(x=Khan$xtest , y=as.factor (Khan$ytest ))
    pred.te <- predict (out , newdata =dat.te)
    table(pred.te , dat.te$y)
    
    