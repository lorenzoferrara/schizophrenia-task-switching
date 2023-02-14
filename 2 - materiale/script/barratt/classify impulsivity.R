###    ANALISI SUI DATI DEL BARRATT TEST
##

# setwd("G:/Il mio Drive/Brain Connectivity")

barratt_raw_data <- read.csv("./materiale/script/barratt/barratt_raw_data.txt", sep="")
barratt_score=barratt_raw_data$total_score
barratt_score_control=barratt_score[1:125]
barratt_score_schz = barratt_score[126:175]

load("./materiale/workspaces/times.RData")

# CLASSIFICAZIONE dei soggetti in 3 "categorie":
#   BIS > 72 HIGH impulsivity
#   BIS in [52,72] MEDIUM impulsivity
#   BIS < 52 LOW impulsivity

{
impuls_control = as.character(c())
low_control <- 0
medium_control <- 0
high_control <- 0
impuls_schz = as.character(c())
low_schz <- 0 
medium_schz <- 0
high_schz <- 0

for (i in 1:125) {
  if (barratt_score_control[i] <= 65) {
    impuls_control <- c(impuls_control,"low")
    low_control <- low_control + 1
  }
  else if (barratt_score_control[i] > 65 & barratt_score_control[i] <= 80) {
    impuls_control <- c(impuls_control,"medium")
    medium_control <- medium_control + 1
  }
  else if ( barratt_score_control[i] > 80) {
    impuls_control<- c(impuls_control,"high")
    high_control <- high_control + 1
  }
}


x11()
barplot(c(low_control,medium_control,high_control), names.arg = c("LOW","MEDIUM","HIGH"), main = "Impulsivity scale - CONTROL")
pie(c(low_control,medium_control,high_control)/125, col = c("yellow","orange","red"),labels = c("LOW","MEDIUM","HIGH"), main = "Impulsivity scale - CONTROL")
}

{
for (i in 1:50) {
  if (barratt_score_schz[i] <= 65) {
    impuls_schz <- c(impuls_schz,"low")
    low_schz <- low_schz + 1
  }
  else if (barratt_score_schz[i] > 65 & barratt_score_schz[i] <= 80) {
    impuls_schz <- c(impuls_schz,"medium")
    medium_schz <- medium_schz + 1
  }
  else if ( barratt_score_schz[i] > 80) {
    impuls_schz<- c(impuls_schz,"high")
    high_schz <- high_schz + 1
  }
}

x11()
barplot(c(low_schz,medium_schz,high_schz)/50, names.arg = c("LOW","MEDIUM","HIGH"), main = "Impulsivity scale - SCHIZOPHRENIC")
pie(c(low_schz,medium_schz,high_schz)/50, col = c("yellow","orange","red"), labels = c("LOW","MEDIUM","HIGH"), main = "Impulsivity scale - SCHIZOPHRENIC")
}

impuls = c(impuls_control, impuls_schz)
impuls_low_index = which(impuls=="low")
impuls_medium_index = which(impuls=="medium")
impuls_high_index = which(impuls=="high")

##############################################################################################

#KMEANS SUI BARRATT SCORES

result.k <- kmeans(barratt_score, centers=3) # Centers: fixed number of clusters

t=c(1,2,3)
ind_max=which(result.k$centers == max(result.k$centers))
ind_min=which(result.k$centers == min(result.k$centers))
ind_mid=t[t!=ind_max & t!=ind_min]
nomi=c()
nomi[ind_min]="low"
nomi[ind_mid]="mid"
nomi[ind_max]="high"

linea_lowmid=max(barratt_score[result.k$cluster==ind_min])  #69
linea_midhigh=max(barratt_score[result.k$cluster==ind_mid]) #86
{x11()
  plot(barratt_score, col = result.k$cluster+1)
  legend("topleft", nomi, col = result.k$cluster+1, lwd = 2)
  colors=c(1,2,3)
  abline(linea_lowmid, 0, col=ind_min+1, lwd=3)
  abline(linea_midhigh, 0, col=ind_mid+1, lwd=3)
}

##############################################################################################

#UNIVARIATE HIERARCICAL CLUSTERING

x=barratt_score
dx <- dist(x)
hcx.single<- hclust(dx, method='single')
hcx.complete<- hclust(dx, method='complete')
hcx.average<- hclust(dx, method='average')

{x11()
  par(mfrow=c(1,3))
  plot(hcx.single, labels=F, cex=0.5, hang=-0.1, xlab='', sub='x', main="single")
  rect.hclust(hcx.single, k=3)
  plot(hcx.complete, labels=F, cex=0.5, hang=-0.1, xlab='', sub='x', main="complete")
  rect.hclust(hcx.complete, k=3)
  plot(hcx.average, labels=F, cex=0.5, hang=-0.1, xlab='', sub='x', main="average")
  rect.hclust(hcx.average, k=3)
}
#=> choose average

cluster.dendrogram <- cutree(hcx.average, k=3)

t=c(1,2,3)
ind_max=cluster.dendrogram[which(x == max(x))]
ind_min=cluster.dendrogram[which(x == min(x))]
ind_mid=t[t!=ind_max & t!=ind_min]
nomi=c()
nomi[ind_min]="low"
nomi[ind_mid]="mid"
nomi[ind_max]="high"

linea_lowmid=max(barratt_score[cluster.dendrogram==ind_min])  #69
linea_midhigh=max(barratt_score[cluster.dendrogram==ind_mid]) #86
{x11()
  plot(barratt_score, col = cluster.dendrogram+1)
  legend("topleft", nomi, col = cluster.dendrogram+1, lwd = 2)
  colors=c(1,2,3)
  abline(linea_lowmid, 0, col=ind_min+1, lwd=3)
  abline(linea_midhigh, 0, col=ind_mid+1, lwd=3)
}

