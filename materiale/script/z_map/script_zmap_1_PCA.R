
rm(list=ls())

setwd("./materiale")
load("./workspaces/zmap.RData")

N=36035
ns=3

nomi_soggetti=row.names(z_map)
num_sani=125

#Preparo il dataframe per connettivit? nelle diverse regioni: matrice 175x83
if(F) {
  con_reg=matrix(0, 175, 83)
  con_reg=as.data.frame(con_reg)
  row.names(con_reg) = row.names(z_map)
  
  {nomi_reg=c()
  for( j  in 1:83){
    nomi_reg = c(nomi_reg,paste("reg",j, sep=""))
  }}
  names(con_reg)=nomi_reg

#riempio il dataframe
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
}

zmap.pc <- princomp(con_reg)
summary(zmap.pc)
zmap.pc$sd^2/sum(zmap.pc$sd^2)
plot(1:83,cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2))

directions<-zmap.pc$loadings
nomi<-NULL

########### genera jpeg in cartella di lavoro di prime 10 componenti PCA (sono sul drive in plots)

jpeg(filename="Screeplot.jpeg")
plot(1:83,cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2), xlab="Number of PCs", ylab="Cumulative variance")
abline(0.8, 0, col="grey")
minima_pc = which(cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2)>0.8)[1]
abline(v = minima_pc, col="red")
dev.off()

setwd("./plots/PCA loadings")
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






for (i in 1:175){
  if(var(t(con_reg[i,]))==0)
    print(i)
}

graphics.off()