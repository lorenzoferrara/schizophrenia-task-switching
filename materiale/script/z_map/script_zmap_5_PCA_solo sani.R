rm(list=ls())

setwd("G:/Il mio Drive/Brain Connectivity")

load("./materiale/workspaces/zmap_creazione.RData")

library(fdaPDE)

N=36035
ns=3

nomi_soggetti=row.names(z_map)
num_sani=125

zmap_sani.pc <- princomp(con_reg[1:125,])
summary(zmap_sani.pc)
zmap_sani.pc$sd^2/sum(zmap_sani.pc$sd^2)
plot(1:83,cumsum(zmap_sani.pc$sd^2)/sum(zmap_sani.pc$sd^2))

directions_sani<-zmap_sani.pc$loadings
nomi<-NULL

setwd("./materiale")
names_regions <- read.csv("./data/names_regions.txt", header=FALSE, sep=";")
names_regions$V2[is.na(names_regions$V2)]=0

#genera jpeg di prime 10 componenti PCA

setwd("./materiale/plots/PCA loadings sani")

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
  barplot(directions_sani[,i])
  dev.off()}



{x11()
  par(mfrow=c(1,2))
  m=min(min(directions_sani[,1]), min(directions_sani[,2]))
  M=max(max(directions_sani[,1]), max(directions_sani[,2]))
  barplot(directions_sani[,1], ylim=c(m,M))
  barplot(directions_sani[,2], ylim=c(m,M))
}
dev.off()

setwd("../../")
names_regions <- read.csv("./data/names_regions.txt", header=FALSE, sep=";")
names_regions$V2[is.na(names_regions$V2)]=0

pc=directions_sani[,]
n_pcs=4
zmap_sani_first3pc=matrix(0, nrow = n_pcs, ncol = N)
for( nodo in 1:N ){
  zona = labels[nodo]
  if(zona != 0){
    for( ind in 1:n_pcs){
      zmap_sani_first3pc[ind, nodo] = pc[zona, ind]
    }
  }
}
############
#PC1
dir1_sani=directions_sani[,1]
m=min(dir1_sani)
M=max(dir1_sani)
sigma=sd(dir1_sani)
threshold= 3.5*sigma
alti_pc1 = which(dir1_sani > threshold)

plot(FEM(zmap_sani_first3pc[1,], FEMbasis))

print("PC1 Sani")
for ( i in alti_pc1 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir1_sani[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

# [1] "0.221 : Region 24 is  Cingulate gyrus (gyrus cinguli), anterior part"
# [1] "0.222 : Region 25 is  Cingulate gyrus (gyrus cinguli), anterior part"
# [1] "0.183 : Region 26 is  Cingulate gyurs (gyrus cinguli), posterior part"
# [1] "0.181 : Region 27 is  Cingulate gyurs (gyrus cinguli), posterior part"
# [1] "0.192 : Region 34 is  Caudate nucleus"
# [1] "0.193 : Region 35 is  Caudate nucleus"
# [1] "0.169 : Region 58 is  Superior frontal gyrus"
# [1] "0.18 : Region 59 is  Superior frontal gyrus"
# [1] "0.176 : Region 81 is  Pre-subgenual frontal cortex"

#############
#PC2

dir2_sani=directions_sani[,2]
mean = mean(dir2_sani)
sigma=sd(dir2_sani)
threshold = 2 * sigma
alti_pc2 = which( abs(dir2_sani - mean) > threshold)

#plot(FEM(zmap_sani_first3pc[2,], FEMbasis))

  for ( i in alti_pc2 ){
    for ( j in 1:dim(names_regions)[1]){
      if( i == names_regions[j,1] || i == names_regions[j,2]){
        print( paste( round(dir2_sani[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
      }
    }
  }


#############
#PC3

dir3_sani=directions_sani[,3]
mean = mean(dir3_sani)
threshold=0.15
alti_pc3 = which( abs(dir3_sani - mean) > threshold)

plot(FEM(zmap_sani_first3pc[3,], FEMbasis))

print("PC3")
for ( i in alti_pc3 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir3_sani[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}


#plottare valori scores degli pc divisi per impulsivity?

#############
#PCult

dir_ult_sani=directions_sani[,83]
mean = mean(dir_ult_sani)
sigma = sd(dir_ult_sani)
threshold =  2* sigma
alti_pc_ult = which( abs(dir_ult_sani - mean) > threshold)

#plot(FEM(zmap_sani_first3pc[3,], FEMbasis))

{
  colors = rep('grey', 83)
  colors[alti_pc_ult]='red'
  x11()
  m=min(dir_ult_sani)
  M=max(dir_ult_sani)
  barplot(dir_ult_sani, ylim=c(m,M), col=colors, main = "Ultima - Sani")
}

print("PCult")
for ( i in alti_pc_ult ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir_ult_sani[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

#############
#PCpenult

dir_penult_sani=directions_sani[,82]
mean = mean(dir_penult_sani)
sigma = sd(dir_penult_sani)
threshold =  2* sigma
alti_pc_penult = which( abs(dir_penult_sani - mean) > threshold)

#plot(FEM(zmap_sani_first3pc[3,], FEMbasis))
{
  colors = rep('grey', 83)
  colors[alti_pc_penult]='red'
  x11()
  m=min(dir_penult_sani)
  M=max(dir_penult_sani)
  barplot(dir_penult_sani, ylim=c(m,M), col=colors, main="Penultima - Sani")
}

print("PCpenult")
for ( i in alti_pc_penult ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir_penult_sani[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

#############
#PCpterzult

dir_terzult_sani=directions_sani[,81]
mean = mean(dir_terzult_sani)
sigma = sd(dir_terzult_sani)
threshold =  2* sigma
alti_pc_terzult = which( abs(dir_terzult_sani - mean) > threshold)

#plot(FEM(zmap_sani_first3pc[3,], FEMbasis))

{
  colors = rep('grey', 83)
  colors[alti_pc_terzult]='red'
  x11()
  m=min(dir_terzult_sani)
  M=max(dir_terzult_sani)
  barplot(dir_terzult_sani, ylim=c(m,M), col=colors)
}
print("PCult")
for ( i in alti_pc_terzult ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir_terzult_sani[i], digits = 2), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}
