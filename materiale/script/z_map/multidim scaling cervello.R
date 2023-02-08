

load("./materiale/workspaces/zmap_creazione.RData")
regioni_sani = con_reg[1:125, ]
regioni_schz = con_reg[126:175,]
save(regioni_sani,regioni_schz,file="./materiale/workspaces/medie regioni.Rdata")


#install.packages("philentropy")
library(philentropy)

sani_mappa<-distance(as.data.frame(regioni_sani[,1]))

help(cmdscale)

location_sani <- cmdscale(sani_mappa, k=2)
location_sani

{x11()
plot(location_sani[,1], location_sani[,2], col='white')
text(location_sani[,1], location_sani[,2], labels=rownames(location_sani))
}
###
# per schz
schz_mappa<-distance(as.data.frame(regioni_schz[,1]))

location_schz <- cmdscale(schz_mappa, k=2)
location_schz

{x11()
plot(location_schz[,1], location_schz[,2], col='white')
text(location_schz[,1], location_schz[,2], labels=rownames(location_schz))
}

dist=matrix(data = 0, 83, 83)
for(i in 1:83){
  for (j in (i+1):83){
    dist_sani[i,j]<-regioni_sani[paste("reg",i,sep=''),1]-regioni_sani[paste("reg",j,sep=''),1]
  }
}
rownames(regioni_sani)
