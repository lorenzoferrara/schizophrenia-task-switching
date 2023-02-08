
library(gridExtra)
library(ggplot2)

setwd("G:/Il mio Drive/Brain Connectivity")

load("./materiale/workspaces/zmap.RData")
setwd("./materiale")

names_regions <- read.csv("./data/names_regions.txt", header=FALSE, sep=";")
names_regions$V2[is.na(names_regions$V2)]=0


massimo = max(directions[,1:3])
minimo = min(directions[,1:3])

##################
#PC1

dir1=directions[,1]

m=min(dir1)   #0.02
M=max(dir1)   #0,21
threshold= 3/4*M
alti_pc1 = which(dir1 > threshold)

{
  colors1 = rep('grey', 83)
  colors1[alti_pc1]='red3'
  colors11 = rep('grey55', 83)
  colors11[alti_pc1]='red4'
  #x11()
  barplot(dir1, ylim=c(0,M), col=colors1)
  dev.copy(jpeg, 'loadings_pc1.jpeg')
}


{x11()
  p2 = ggplot(data.frame(regione=dir1),aes(x=names(dir1), y=regione))+
    geom_bar(stat="identity", colour=colors11, fill=colors1, ) + xlab("1st PC")+ylab("")+
    theme_minimal(base_size = 15)+theme(legend.position = "", axis.text.x = element_blank()) + ylim(minimo, massimo)
  p2
}

quale_pc=1
plot(FEM(zmap_first3pc[quale_pc,], FEMbasis))
boxplot(scores[1:125, quale_pc], scores[126:175,quale_pc], names = c("sani", "schz"))

var.test(scores[1:125, quale_pc], scores[126:175,quale_pc])
test_result = t.test(scores[1:125, quale_pc], scores[126:175,quale_pc], var.equal = T)
test_result

if(F){
  #x11()
  m=min(dir1)
  M=max(dir1)
  barplot(dir1[ alti_pc1], ylim=c(0,M))
}

for ( i in alti_pc1 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir1[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}


##################
#PC1 modificata

medie<-colMeans(con_reg)

dir1=directions[,1] - medie

m=min(dir1)   #0.02
M=max(dir1)   #0,21
threshold= 3/4*M
M_loads<-mean(dir1)
SD_loads<-sd(dir1)
alti_pc1 = which( dir1 > M_loads+2*SD_loads | dir1 < M_loads-2*SD_loads)

{
  colors1 = rep('grey', 83)
  colors1[alti_pc1]='red3'
  colors11 = rep('grey55', 83)
  colors11[alti_pc1]='red4'
  x11()
  barplot(dir1, ylim=c(0,M), col=colors1)
  dev.copy(jpeg, 'loadings_pc1.jpeg')
}


names(dir1)

{x11()
  p2 = ggplot(data.frame(regione=dir1),aes(x=as.factor(1:83), y=regione))+
    geom_bar(stat="identity", colour=colors11, fill=colors1) + xlab("1st PC") + ylab("")+
    theme_minimal(base_size = 15) + theme(legend.position = "", axis.text.x = element_blank()) + ylim(minimo, massimo)
  p2
}

quale_pc=1
plot(FEM(zmap_first3pc[quale_pc,], FEMbasis))
boxplot(scores[1:125, quale_pc], scores[126:175,quale_pc], names = c("sani", "schz"))

var.test(scores[1:125, quale_pc], scores[126:175,quale_pc])
test_result = t.test(scores[1:125, quale_pc], scores[126:175,quale_pc], var.equal = T)
test_result

if(F){
  #x11()
  m=min(dir1)
  M=max(dir1)
  barplot(dir1[ alti_pc1], ylim=c(0,M))
}

for ( i in alti_pc1 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir1[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

#############
#PC2

dir2=directions[,2]

mean = mean(dir2)
threshold=0.15
alti_pc2 = which( abs(dir2 - mean) > threshold)

{
  colors2 = rep('grey', 83)
  colors2[alti_pc2]='red3'
  colors22 = rep('grey55', 83)
  colors22[alti_pc2]='red4'
#  x11()
  m=min(dir2)    #-0.28
  M=max(dir2)    # 0.22
  barplot(dir2, ylim=c(m,M), col=colors2)
}

{x11()
  p2 = ggplot(data.frame(regione=dir2),aes(x=names(dir2), y=regione))+
    geom_bar(stat="identity", colour=colors22, fill=colors2, ) + xlab("2nd PC")+ylab("")+
    theme_minimal(base_size = 15)+theme(legend.position = "", axis.text.x = element_blank()) + ylim(minimo, massimo)
  p2
}

quale_pc=2
plot(FEM(zmap_first3pc[quale_pc,], FEMbasis))
boxplot(scores[1:125, quale_pc], scores[126:175,quale_pc], names = c("sani", "schz"))

var.test(scores[1:125, quale_pc], scores[126:175,quale_pc])
test_result = t.test(scores[1:125, quale_pc], scores[126:175,quale_pc], var.equal = F)
test_result

{print("Region 2")
for ( i in alti_pc2 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir2[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
   }
  }
}


#############
#PC3

dir3=directions[,3]

mean = mean(dir3)
threshold=0.15
alti_pc3 = which( abs(dir3 - mean) > threshold)
alti_pc3 = which( disegno_load > M_loads+1.75*SD_loads | disegno_load < M_loads-1.75*SD_loads)

{
  colors3 = rep('grey', 83)
  colors3[alti_pc3]='red3'
  colors33 = rep('grey55', 83)
  colors33[alti_pc3]='red4'
  #x11()
  m=min(dir3)    #-0.26
  M=max(dir3)    # 0.47
  barplot(dir3, ylim=c(m,M), col=colors3)
}

{x11()
  p2 = ggplot(data.frame(regione=dir3),aes(x=names(dir3), y=regione))+
    geom_bar(stat="identity", colour=colors33, fill=colors3, ) + xlab("")+ylab("")+
    theme_minimal(base_size = 15)+theme(legend.position = "", axis.text.x = element_blank(), plot.title = element_text(hjust=0.5)) +
    ylim(minimo, massimo) + labs(title="1st PC")
  p2
}

quale_pc=3
plot(FEM(zmap_first3pc[quale_pc,], FEMbasis))
boxplot(scores[1:125, quale_pc], scores[126:175,quale_pc], names = c("sani", "schz"))

var.test(scores[1:125, quale_pc], scores[126:175,quale_pc])
test_result = t.test(scores[1:125, quale_pc], scores[126:175,quale_pc], var.equal = F)
test_result

print("Region 3")
for ( i in alti_pc3 ){
  for ( j in 1:dim(names_regions)[1]){
    if( i == names_regions[j,1] || i == names_regions[j,2]){
      print( paste( round(dir3[i], digits = 3), ": Region", i, "is", names_regions[j,3], sep=" "))
    }
  }
}

write.vtu(FEM(zmap_first3pc[3,], FEMbasis), file = "zmap_first3pc3.vtu")

#########################################

#PLOT DI TUTTI I LOADING

{x11()
  p1 = ggplot(data.frame(regione=dir1),aes(x=names(dir1), y=regione))+
    geom_bar(stat="identity", colour=colors11, fill=colors1, ) + xlab("")+ ylab("")+
    theme_minimal(base_size = 15) + theme(legend.position = "", axis.text.x = element_blank(), plot.title = element_text(hjust=0.5)) +
    ylim(minimo, massimo) + labs(title="1st PC")
  p1
  
  p2 = ggplot(data.frame(regione=dir2),aes(x=names(dir2), y=regione))+
    geom_bar(stat="identity", colour=colors22, fill=colors2, ) + xlab("")+ylab("")+
    theme_minimal(base_size = 15)+theme(legend.position = "", axis.text.x = element_blank(), plot.title = element_text(hjust=0.5)) +
    ylim(minimo, massimo) + labs(title="2nd PC")
  
  p3 = ggplot(data.frame(regione=dir3),aes(x=names(dir3), y=regione))+
    geom_bar(stat="identity", colour=colors33, fill=colors3, ) + xlab("")+ylab("")+
    theme_minimal(base_size = 15)+theme(legend.position = "", axis.text.x = element_blank(), plot.title = element_text(hjust=0.5)) +
    ylim(minimo, massimo) + labs(title="3rd PC")
  
  grid.arrange(p1,p2, p3, nrow=1,top=" ")
}

graphics.off()

#PLOTTO GLI SCORES SULLA TERZA PC

library(ggplot2)
{x11()
pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Scores = scores[,3]),aes(x=Diagnosis,y=Scores))+
  geom_boxplot(aes(fill=Diagnosis),width=0.5)+
  scale_fill_manual(values = c("royalblue","orange2"))+
  theme_minimal()+theme(legend.position = "")
pp
}

##################################################

#Plotto sani e schizo lungo PC
{
x11(height=350, width=600)
par(mfrow=c(2,5))
for( j in 1:10){
  boxplot(scores[1:125, j], scores[126:175,j], names = c("sani", "schz"), main = paste("Score on PC", j, sep="") )
}

x11(height=350, width=600)
par(mfrow=c(2,5))
for( j in 1:10){
  boxplot(scores[impuls_low_index, j], scores[impuls_medium_index,j],  scores[impuls_high_index,j], names = c("low", "medium", "high"), main = paste("Score on PC", j, sep="") )
}

x11(height=350, width=600)
par(mfrow=c(2,5))
for( j in 1:10){
  plot(scores[,j], times, xlab = "scores", main = paste("Score on PC", j, sep=""))
}

}

###################

setwd("./materiale/plots/boxplot regioni")
#Plotto sani e schizo lungo regioni
for( i in 0:5){
  #jpeg
  x11( height=500, width = 900)
  par(mfrow=c(2,8))
  for( j in 0:15){
    indice = i*16 + j + 1
    boxplot(con_reg[1:125, indice], con_reg[126:175,indice], names = c("sani", "schz"), main = paste("Score on region", indice, sep="") )
  }
  dev.copy(jpeg, paste('plot', i, '.jpeg', sep=""))
}

for( i in 0:5){
  x11( height=500, width = 900)
  par(mfrow=c(2,8))
  for( j in 0:15){
    indice = i*16 + j + 1
    boxplot(con_reg[impuls_low_index, indice], con_reg[impuls_medium_index,indice], con_reg[impuls_high_index,indice], names = c("low", "medium", "high"), main = paste("Score on region", indice, sep="") )
  }
}

graphics.off()

var.test(scores[1:125, 3], scores[126:175,3])
shapiro.test(scores[1:125, 3])
shapiro.test(scores[126:175,3])
)
t.test(scores[1:125, 3], scores[126:175,3])
chisq.test(scores[1:125, 3], scores[126:175,3])

###################################

dat1 = scores[1:125, 3]
dat2 = scores[126:175,3]
n1 <- length(dat1)
n2 <- length(dat2)

dat1.mean <- mean(dat1)
dat2.mean <- mean(dat2)
dat1.cov  <-  var(dat1)
dat2.cov  <-  var(dat2)
Sp      <- ((n1-1)*dat1.cov + (n2-1)*dat2.cov)/(n1+n2-2)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == 0  vs  H1: mu1-mu2 != 0

alpha   <- .01
delta.0 <- 0

t0 <- (dat1.mean-dat2.mean-delta.0)^2 / (Sp* (1/n1 + 1/n2))

cfr <- qchisq(1-alpha,n1+n2-2)
t0 < cfr 

P <- 1 - pchisq(t0, n1+n2-2)
P 


#############################################

#PLOTTO GLI SCORES SULLA prima PC

library(ggplot2)
{x11()
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Scores = scores[,1]),aes(x=Diagnosis,y=Scores))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "",text = element_text(size=16, face = c( "bold")), plot.title = element_text(hjust=0.5, size=18, face = "bold"))+
    xlab("") + ylab("")+ labs(title="Scores on 1st PC")
  pp
}


#PLOTTO GLI SCORES SULLA seconda PC

library(ggplot2)
{x11()
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Scores = scores[,2]),aes(x=Diagnosis,y=Scores))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "",text = element_text(size=16, face = c( "bold")), plot.title = element_text(hjust=0.5, size=18, face = "bold"))+
    xlab("") + ylab("")+ labs(title="Scores on 2nd PC")
  pp
}

library(ggplot2)
{x11()
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Scores = scores[,3]),aes(x=Diagnosis,y=Scores))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "",text = element_text(size=16, face = c( "bold")), plot.title = element_text(hjust=0.5, size=18, face = "bold"))+
    xlab("") + ylab("")+ labs(title="Scores on 3rd PC")
  pp
}

