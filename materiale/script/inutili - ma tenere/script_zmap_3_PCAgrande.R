rm(list=ls())

load("./materiale/workspaces/zmap_creazione.RData")

N=36035
ns=3

nomi_soggetti=row.names(z_map)
num_sani=125

z_map_pulita = z_map[,complete.cases(t(z_map))]
dim(z_map_pulita)
z_map_pulita.pc <- prcomp(z_map_pulita)
directions <- t( z_map_pulita.pc$rotation )
dim(directions)

a = colnames(directions)
for ( i in 1:length(a)){
  a[i] = gsub("V", "", a[i])
}
a = as.numeric(a)
ind=1
for (i in 1:N){
  if( a[ind] != i) {
    
  } 
  else{
    ind = ind+1
  }
}

plot(1:length(z_map_pulita.pc$sdev),cumsum(z_map_pulita.pc$sd^2)/sum(z_map_pulita.pc$sd^2))

nomi<-NULL

