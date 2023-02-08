rm(list=ls())

setwd("G:/Il mio Drive/Brain Connectivity")

load("./materiale/workspaces/zmap_creazione.RData")

library(fdaPDE)

N=36035
ns=3

nomi_soggetti=row.names(z_map)
num_schz=125

zmap_schz.pc <- prcomp(con_reg[126:175,])
summary(zmap_schz.pc)
zmap_schz.pc$sd^2/sum(zmap_schz.pc$sd^2)
plot(1:50,cumsum(zmap_schz.pc$sd^2)/sum(zmap_schz.pc$sd^2))

directions_schz<-zmap_schz.pc$rotation
nomi<-NULL

#genera jpeg di prime 10 componenti PCA

setwd("./materiale/plots/PCA loadings schz")

jpeg(filename="Screeplot.jpeg")
plot(1:83,cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2), xlab="Number of PCs", ylab="Cumulative variance")
abline(0.8, 0, col="grey")
minima_pc = which(cumsum(zmap.pc$sd^2)/sum(zmap.pc$sd^2)>0.8)[1]
abline(v = minima_pc, col="red")
dev.off()

for (i in 1:10){
  nomi= c(nomi,paste("loads component",i, sep=" ", paste(".jpeg")))}

for (i in 1:10) {
  jpeg(nomi[i], width = 350, height = 350)
  barplot(directions_schz[,i])
  dev.off()}

{x11()
  par(mfrow=c(1,2))
  m=min(min(directions_schz[,1]), min(directions_schz[,2]))
  M=max(max(directions_schz[,1]), max(directions_schz[,2]))
  barplot(directions_schz[,1], ylim=c(m,M))
  barplot(directions_schz[,2], ylim=c(m,M))
}

setwd("../../")
names_regions <- read.csv("./data/names_regions.txt", header=FALSE, sep=";")
names_regions$V2[is.na(names_regions$V2)]=0

pc=directions_schz[,]
n_pcs=4
zmap_schz_first3pc=matrix(0, nrow = n_pcs, ncol = N)
for( nodo in 1:N ){
  zona = labels[nodo]
  if(zona != 0){
    for( ind in 1:n_pcs){
      zmap_schz_first3pc[ind, nodo] = pc[zona, ind]
    }
  }
}
############
#PC1
dir1_schz=directions_schz[,1]
m=min(dir1_schz)
M=max(dir1_schz)
sigma=sd(dir1_schz)
threshold= 3.5*sigma
alti_pc1 = which(dir1_schz > threshold)

#plot(FEM(zmap_schz_first3pc[1,], FEMbasis))

for ( i in alti_pc1 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir1_schz[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

# [1] "0.187 : Region 24 is  Cingulate gyrus (gyrus cinguli), anterior part"
# [1] "0.183 : Region 25 is  Cingulate gyrus (gyrus cinguli), anterior part"
# [1] "0.198 : Region 26 is  Cingulate gyurs (gyrus cinguli), posterior part"
# [1] "0.18  : Region 27 is  Cingulate gyurs (gyrus cinguli), posterior part"
# [1] "0.189 : Region 36 is  Nucleus accumbens"
# [1] "0.186 : Region 48 is  Lateral ventricle, temporal horn"
# [1] "0.187 : Region 79 is  Subcallosal area"


#############
#PC2

dir2_schz=directions_schz[,2]
mean = mean(dir2_schz)
sigma=sd(dir2_schz)
threshold= 3*sigma
alti_pc2 = which( abs(dir2_schz - mean) > threshold)

#plot(FEM(zmap_schz_first3pc[2,], FEMbasis))

{print("PC2")
  for ( i in alti_pc2 ){
    for ( j in 1:dim(names_regions)[1]){
      if( i == names_regions[j,1] || i == names_regions[j,2]){
        print( paste( round(dir2_schz[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
      }
    }
  }
}

#############
#PC3

dir3_schz=directions_schz[,3]
mean = mean(dir3_schz)
threshold=0.15
alti_pc3 = which( abs(dir3_schz - mean) > threshold)

plot(FEM(zmap_schz_first3pc[3,], FEMbasis))

print("PC3")
for ( i in alti_pc3 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir3_schz[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

#########
#PCult

dir_ult_schz=directions_schz[,50]
mean = mean(dir_ult_schz)
sigma = sd(dir_ult_schz)
threshold =  1.95* sigma
alti_pcult = which( abs(dir_ult_schz - mean) > threshold)

#plot(FEM(zmap_sani_first3pc[3,], FEMbasis))

{
  colors = rep('grey', 83)
  colors[alti_pcult]='red'
  x11()
  m=min(dir_ult_schz)
  M=max(dir_ult_schz)
  barplot(dir_ult_schz, ylim=c(m,M), col=colors, main = "Ultima - Schz")
}

print("PCult")
for ( i in alti_pcult ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir_ult_schz[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

#########
#PCpenult

dir_penult_schz=directions_schz[,49]
mean = mean(dir_penult_schz)
sigma = sd(dir_penult_schz)
threshold =  2* sigma
alti_pcult = which( abs(dir_penult_schz - mean) > threshold)

#plot(FEM(zmap_sani_first3pc[3,], FEMbasis))

print("PCpenult")
for ( i in alti_pcult ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir_penult_schz[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

#########
#PCterzult

dir_terzult_schz=directions_schz[,48]
mean = mean(dir_terzult_schz)
sigma = sd(dir_terzult_schz)
threshold =  2* sigma
alti_pcult = which( abs(dir_terzult_schz - mean) > threshold)

#plot(FEM(zmap_sani_first3pc[3,], FEMbasis))

print("PCterzult")
for ( i in alti_pc_terzult ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir_terzult_schz[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}
