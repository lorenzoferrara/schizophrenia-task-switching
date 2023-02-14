rm(list=ls())

load("./materiale/workspaces/zmap.RData")

N=36035
ns=3

nomi_soggetti=row.names(z_map)
num_sani=125

# Dataframe 

## Prepare the dataframe for the connectivity map in the regions: con_reg is a 175x83 matrix
con_reg=matrix(0, 175, 83)
con_reg=as.data.frame(con_reg)
row.names(con_reg) = row.names(z_map)

## names of the regions
nomi_reg=c()
for( j  in 1:83){
  nomi_reg = c(nomi_reg,paste("reg",j, sep=""))
}
names(con_reg)=nomi_reg

## fill the dataframe
for (row in 1:175) {
  regioni=matrix(0,83,2)
  
  for( i in 1:N ){
    j=labels[i]
    if( ! is.na(z_map[row,i])){
      regioni[j,1] =regioni[j,1]+z_map[row,i]
      regioni[j,2] = regioni[j,2]+1
    }
  }
  for( j  in 1:83){
    con_reg[row,j] = regioni[j,1]/regioni[j,2]
  }
}

## means of the regions
medie<-colMeans(con_reg)

# PRINCIPAL COMPONENT ANALYSIS
zmap.pc <- princomp(con_reg, scores=TRUE)
plot(1:83,cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2))

directions<-zmap.pc$loadings
nomi<-NULL

## 1st PC with respect to the means
draw_load<-directions[,1]-medie

## identify the regions with anomalous loadings
M_loads<-mean(draw_load)
SD_loads<-sd(draw_load)
high_pc = which( draw_load > M_loads+2*SD_loads | draw_load < M_loads-2*SD_loads)

{
  colors = rep('grey', 83)
  colors[high_pc]='red'
  x11()
  barplot(draw_load, ylim=c(-0.10,0.10), col=colors)
  dev.copy(jpeg, 'loadings_pc1.jpeg')
}

for ( i in high_pc ){
  for ( j in 1:43){
    if (is.na(names_regions[j,1])) {names_regions[j,1]=0}
    if (is.na(names_regions[j,2])) {names_regions[j,2]=0}
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(draw_load[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

# 2nd PC

draw_load<-directions[,2]

M_loads<-mean(draw_load)
SD_loads<-sd(draw_load)
high_pc = which( draw_load > M_loads+1.75*SD_loads | draw_load < M_loads-1.75*SD_loads)

{
  colors = rep('grey', 83)
  colors[high_pc]='red'
  x11()
  barplot(draw_load, ylim=c(-0.3,0.3), col=colors)
  dev.copy(jpeg, 'loadings_pc1.jpeg')
}

for ( i in high_pc ){
  for ( j in 1:43){
    if (is.na(names_regions[j,1])) {names_regions[j,1]=0}
    if (is.na(names_regions[j,2])) {names_regions[j,2]=0}
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(draw_load[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

{
x11()
boxplot(zmap.pc$scores[1:125,2],zmap.pc$scores[126:175,2], main='scores on 2nd pc', xlab='diagnosis')
}

## 3rd PC

draw_load<-directions[,3]

M_loads<-mean(draw_load)
SD_loads<-sd(draw_load)
high_pc = which( draw_load > M_loads+1.75*SD_loads | draw_load < M_loads-1.75*SD_loads)

{
  colors = rep('grey', 83)
  colors[high_pc]='red'
  x11()
  barplot(draw_load, ylim=c(-0.3,0.3), col=colors)
  dev.copy(jpeg, 'loadings_pc1.jpeg')
}

for ( i in high_pc ){
  for ( j in 1:43){
    if (is.na(names_regions[j,1])) {names_regions[j,1]=0}
    if (is.na(names_regions[j,2])) {names_regions[j,2]=0}
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(draw_load[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

{
x11()
boxplot(zmap.pc$scores[1:125,3],zmap.pc$scores[126:175,3], main='scores on 3rd pc', xlab='diagnosis')
}

library(plot3D)
library(car)
plot(zmap.pc$scores[,2],zmap.pc$scores[,3],col=c(rep('blue',125),rep('red',25)))
scatter3D(zmap.pc$scores[,1],zmap.pc$scores[,2],zmap.pc$scores[,3],col=c(rep('blue',125),rep('red',25)),cex=2.5)

open3d()
plot3d(zmap.pc$scores[,1:3], size=3, col=c(rep('blue',125),rep('red',25)), aspect = F) 

# pc 4
draw_load<-directions[,4]
barplot(draw_load)

M_loads<-mean(draw_load)
SD_loads<-sd(draw_load)
high_pc = which( draw_load > M_loads+1.75*SD_loads | draw_load < M_loads-1.75*SD_loads)

{
  colors = rep('grey', 83)
  colors[high_pc]='red'
  x11()
  barplot(draw_load, ylim=c(-0.3,0.3), col=colors)
  dev.copy(jpeg, 'loadings_pc1.jpeg')
}


for ( i in high_pc ){
  for ( j in 1:43){
    if (is.na(names_regions[j,1])) {names_regions[j,1]=0}
    if (is.na(names_regions[j,2])) {names_regions[j,2]=0}
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(draw_load[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}
x11()
boxplot(zmap.pc$scores[1:125,4],zmap.pc$scores[126:175,4], main='scores on 3rd pc', xlab='diagnosis')
plot(1:175,zmap.pc$scores[,3], main='scores 2nd PC', xlab='subjects', ylab='score')
points(1:125,zmap.pc$scores[1:125,3],col='red')
abline(h=mean(zmap.pc$scores[1:125,2]))


plot(zmap.pc$scores[,3],zmap.pc$scores[,4],col=c(rep('blue',125),rep('red',50)))

library(plot3D)
library(car)
plot(zmap.pc$scores[,2],zmap.pc$scores[,3],col=c(rep('blue',125),rep('red',25)))
scatter3D(zmap.pc$scores[,1],zmap.pc$scores[,2],zmap.pc$scores[,3],col=c(rep('blue',125),rep('red',25)),cex=2.5)

open3d()
plot3d(zmap.pc$scores[,2:4], size=3, col=c(rep('blue',125),rep('red',25)), aspect = F) 

# manova
Y<-as.matrix(zmap.pc$scores[,1:4])
fit <- manova(Y ~ allparticipants$diagnosis)
summary.manova(fit, test="Wilks")




media_pc<-matrix(0,2,4)
media_pc[1,1]<-mean(zmap.pc$scores[1:125,1])
media_pc[2,1]<-mean(zmap.pc$scores[126:175,1])


media_pc[1,2]<-mean(zmap.pc$scores[1:125,2])
media_pc[2,2]<-mean(zmap.pc$scores[126:175,2])

media_pc[1,3]<-mean(zmap.pc$scores[1:125,3])
media_pc[2,3]<-mean(zmap.pc$scores[126:175,3])

media_pc[1,4]<-mean(zmap.pc$scores[1:125,4])
media_pc[2,4]<-mean(zmap.pc$scores[126:175,4])


colnames(media_pc)<-c('pc 1','pc 2', 'pc 3', 'pc 4')


# non normalit?

alpha<-0.05
pval<-matrix(0,4,1)
for (i in 1:4)
{avg.mean <- (media_pc[1,i]*125+media_pc[2,i]*50)/(175) 
var_pooled<-var(zmap.pc$scores[,i])
z.i <- (media_pc[1,i]-media_pc[2,i])/sqrt(var_pooled*(1/125+1/50))
p.i <- ifelse(z.i<0, 2*pnorm(z.i),2*(1-pnorm(z.i)))
print(p.i)
}


# varianza di popolazioni non normali?
for (i in 1:4)
# {chisq.test(zmap.pc$scores[1:125,i],zmap.pc$scores[126:175,i])}


scores_pc<-as.data.frame(zmap.pc$scores[,1:4])
barratt_total<-barratt_raw_data[,12]
barratt_pca<-lm(barratt_total~scores_pc[,1]*scores_pc[,2]*scores_pc[,3]*scores_pc[,4])
summary(barratt_pca)


scores_pc<-as.data.frame(zmap.pc$scores[,1:3])
barratt_total<-barratt_raw_data[,12]
barratt_pca<-lm(barratt_total~scores_pc[,1]*scores_pc[,2]*scores_pc[,3])
summary(barratt_pca)

barratt_pca_2<-lm(barratt_total~scores_pc[,2]:scores_pc[,3]:scores_pc[,4])
summary(barratt_pca_2)

setwd("./materiale/events recording")

elenco_copia <- read.delim("./elenco_copia.txt", header=T)
elenco <- read.delim("./elenco.txt", header=T)
p <- read.delim("../data/participants.csv")

t_sani = c()
t_schz = c()
#CARICO UNO AD UNO I RECORDINGS DEI SOGGETTI
for ( i in 1:175) {
  val <- elenco$v[i]
  s = read.delim(paste("./", val, sep="" ))
  
  s$ReactionTime = as.numeric(s$ReactionTime)
  meantime <- mean(s$ReactionTime, na.rm=T)
  if (p$diagnosis[i]=="CONTROL"){
    t_sani <- c(t_sani, meantime)
  } else {
    t_schz <- c(t_schz, meantime)
  }
}

rtime<-append(t_sani,t_schz)

time_pca<-lm(rtime~scores_pc[,1]*scores_pc[,2]*scores_pc[,3])
summary(time_pca)

vif(time_pca)

time_pca<-lm(rtime~scores_pc[,1]+scores_pc[,2]+scores_pc[,3])
summary(time_pca)

time_pca<-lm(rtime~scores_pc[,2]+scores_pc[,3])
summary(time_pca)

time_pca<-lm(rtime~scores_pc[,2])
summary(time_pca)

time_pca<-lm(rtime~scores_pc[,1]+scores_pc[,2])
summary(time_pca)

time_pca<-lm(rtime~scores_pc[,2]*allparticipants$diagnosis)
summary(time_pca)

time_pca<-lm(rtime~scores_pc[,2]+allparticipants$diagnosis)
summary(time_pca)

err<-time_pca$residuals
plot(rtime,err,col=c(rep('blue',125),rep('red',50)))

boxplot(err~allparticipants$diagnosis)

plot(1:175,scores_pc[,2])

library(MASS)
boxCox(time_pca)
plot(1:175,(scores_pc[,2])^(1/3))
scores_modificati<-(scores_pc[,2])^(1/3)

time_pca<-lm(rtime~scores_modificati*allparticipants$diagnosis)
summary(time_pca)

time_pca<-lm(rtime~scores_modificati+allparticipants$diagnosis)
summary(time_pca)

time_pca<-lm(rtime~scores_modificati)
summary(time_pca)

err<-time_pca$residuals
da_escludere<-time_pca$na.action
plot(rtime,err,col=c(rep('blue',125),rep('red',50)))
plot(rtime[-da_escludere],err)
boxplot(err~allparticipants$diagnosis)

shapiro.test(rtime)
boxCox(rtime)
plot(1:175,rtime)
install.packages("forecast")
library(forecast)
rtime1<-BoxCox.lambda(rtime)
plot(1:175,rtime^3/2)
hist(rtime)
shapiro.test(rtime)

mean(rtime)
sd(rtime)
rtime_std<-(rtime-mean(rtime))/sd(rtime)
plot(1:175,rtime_std)
hist(rtime_std)
shapiro.test(rtime_std)


time_pca<-lm(rtime_std~scores_modificati)
summary(time_pca)

err<-time_pca$residuals
da_escludere<-time_pca$na.action
plot(rtime_std,err,col=c(rep('blue',125),rep('red',50)))
plot(rtime_std[-da_escludere],err)
boxplot(err~allparticipants$diagnosis)


#genera jpeg in cartella di lavoro di prime 10 componenti PCA (sono sul drive in plots)

jpeg(filename="Screeplot.jpeg")
plot(1:83,cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2), xlab="Number of PCs", ylab="Cumulative variance")
abline(0.8, 0, col="grey")
minima_pc = which(cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2)>0.8)[1]
abline(v = minima_pc, col="red")
dev.off()

setwd("../plots/PCA loadings")
for (i in 1:10)
{nomi= c(nomi,paste("loads component",i, sep=" ", paste(".jpeg")))}

for (i in 1:10)  
{
  jpeg(nomi[i], width = 350, height = 350)
  barplot(directions[,i])
  dev.off()}

{x11()
  par(mfrow=c(1,2))
  m=min(min(directions[,1]), min(directions[,2]))
  M=max(max(directions[,1]), max(directions[,2]))
  barplot(directions[,1], ylim=c(m,M))
  barplot(directions[,2], ylim=c(m,M))
}
{x11()
  par(mfrow=c(1,3))
  m=min(min(directions[,1]), min(directions[,2]), min(directions[,3]))
  M=max(max(directions[,1]), max(directions[,2]), max(directions[,3]))
  barplot(directions[,1], ylim=c(m,M))
  barplot(directions[,2], ylim=c(m,M))
  barplot(directions[,3], ylim=c(m,M))
}
{x11()
  par(mfrow=c(1,4))
  m=min(min(directions[,1]), min(directions[,2]), min(directions[,3]),min(directions[,4]))
  M=max(max(directions[,1]), max(directions[,2]), max(directions[,3]), max(directions[,4]))
  barplot(directions[,1], ylim=c(m,M))
  barplot(directions[,2], ylim=c(m,M))
  barplot(directions[,3], ylim=c(m,M))
  barplot(directions[,4], ylim=c(m,M))
}
########## fine Costanza

con_reg_sani_median = rep(0,83)
for(i in 1:83){
  con_reg_sani_median[i] = median(con_reg[1:125,i])
}
con_reg_sani_median

con_reg_schz_median = rep(0,83)
for(i in 1:83){
  con_reg_schz_median[i] = median(con_reg[126:175,i])
}
con_reg_schz_median





