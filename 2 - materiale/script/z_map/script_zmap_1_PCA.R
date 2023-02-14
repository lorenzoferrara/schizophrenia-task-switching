
rm(list=ls())

setwd("./materiale")
load("./workspaces/zmap.RData")

N=length(labels)
ns=3

# Introduction

## Names
subject.names = row.names(z_map)
num_control = 125
{
  region.names=c()
  for( j  in 1:83){
    region.names = c(region.names,paste("reg",j, sep=""))
  }
}

## Prepare the dataframe of connectivity: matrice 175x83
con_reg=matrix(0, 175, 83)
con_reg=as.data.frame(con_reg)
row.names(con_reg) = row.names(z_map)
colnames(con_reg)=region.names

## Fill the dataframe
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
save(list=c("con_reg"), file = "workspaces/connectivity.map.RData")

# Principal Components Analysis

zmap.pc <- princomp(con_reg)

{
  par(mfrow=c(1,1))
  plot(1:83,cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2), xlab = "Number of PCs", ylab = "Variance explained", main = "Screeplot")
}

directions<-zmap.pc$loadings
names<-NULL

########### genera jpeg in cartella di lavoro di prime 10 componenti PCA (sono sul drive in plots)

setwd("./plots/PCA loadings")

jpeg(filename="Screeplot.jpeg")
plot(1:83,cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2), xlab="Number of PCs", ylab="Cumulative variance")
abline(0.8, 0, col="grey")
minima_pc = which(cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2)>0.8)[1]
abline(v = minima_pc, col="red")
dev.off()

for (i in 1:10)
  names= c(names,paste("loads component",i, sep=" ", paste(".jpeg")))

for (i in 1:10){
  jpeg(names[i], width = 350, height = 350)
  barplot(directions[,i])
  dev.off()
}

{
  x11()
  par(mfrow=c(1,3))
  m=min(min(directions[,1]), min(directions[,2]), min(directions[,3]))
  M=max(max(directions[,1]), max(directions[,2]), max(directions[,3]))
  barplot(directions[,1], ylim=c(m,M))
  barplot(directions[,2], ylim=c(m,M))
  barplot(directions[,3], ylim=c(m,M))
}

for (i in 1:175){
  if(var(t(con_reg[i,]))==0)
    print(i)
}

