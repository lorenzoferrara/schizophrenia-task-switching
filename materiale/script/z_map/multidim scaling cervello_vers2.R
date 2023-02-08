

load("./materiale/workspaces/zmap_creazione.RData")
regioni_sani = con_reg[1:125, ]
regioni_schz = con_reg[126:175,]
save(regioni_sani,regioni_schz,con_reg,file="dati per mds.csv")




########
# tutti
tutti<-colMeans(con_reg)
tutti_mappa<-dist(tutti, method='euclidean')

location_tutti <- cmdscale(tutti_mappa, k=2)
location_tutti
all<-c(location_tutti[,1],location_tutti[,2])
range = c(min(all), max(all))


{x11()
  plot(location_tutti[,1], location_tutti[,2], col='white', xlim=range,ylim=range, main="all")
  text(location_tutti[,1], location_tutti[,2], labels=rownames(location_tutti))
}
#######
# sani
sani<-colMeans(regioni_sani)
sani_mappa<-dist(sani, method='euclidean')

location_sani <- cmdscale(sani_mappa, k=2)
location_sani
all<-c(location_sani[,1],location_sani[,2])
range = c(min(all), max(all))


{x11()
  plot(location_sani[,1], location_sani[,2], col='white', xlim=range,ylim=range, main="sani")
  text(location_sani[,1], location_sani[,2], labels=rownames(location_tutti))
}
########
# schz
schz<-colMeans(regioni_schz)
schz_mappa<-dist(schz, method='euclidean')
image(as.matrix(schz_mappa))

location_schz <- cmdscale(schz_mappa, k=2)
location_schz
all<-c(location_schz[,1],-location_schz[,2])
range = c(min(all), max(all))


{x11()
  plot(location_schz[,1], -location_schz[,2], col='white', xlim=range,ylim=range, main="schz")
  text(location_schz[,1], -location_schz[,2], labels=rownames(location_tutti))
}

