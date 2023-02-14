load("C:/Users/Costanza/Downloads/SubjData/SubjData/directions.txt")
ordered_dir<-order(directions[,1])
plot(directions[ordered_dir,1])

##
ordered_dir_2<-order(directions[,2])
plot(directions[ordered_dir_2,2])
##

ordered_dir_3<-order(directions[,3])
plot(directions[ordered_dir_3,3])
##

library(ClusterR)
library(cluster)

#### gruppi da hammers_mith_atlas
temporal_lobe<-c(1,2,3,4,5,6,7,8,9,10,11, 12, 13, 14, 15, 16, 30, 31 ,82,83)
count=1
for (ii in temporal_lobe)
{ temporal_lobe[count]<-paste("reg",ii, sep="")
count=count+1}

posterior_fossa<-c(17, 18, 19)
count=1
for (ii in posterior_fossa)
{ posterior_fossa[count]<-paste("reg",ii, sep="")
count=count+1}


insula_cingulate_gyri<-c(20, 21, 24, 25, 26, 27)
count=1
for (ii in insula_cingulate_gyri)
{ insula_cingulate_gyri[count]<-paste("reg",ii, sep="")
count=count+1}

frontal_lobe<-c(28, 29, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 68, 69, 70, 71, 72, 73, 76, 77, 78, 79, 80, 81)
count=1
for (ii in frontal_lobe)
{ frontal_lobe[count]<-paste("reg",ii, sep="")
count=count+1}

occipital_lobe<-c(64,65, 66, 67, 22, 23)
count=1
for (ii in occipital_lobe)
{ occipital_lobe[count]<-paste("reg",ii, sep="")
count=count+1}

parietal_lobe<-c(60, 61, 62, 63, 32, 33)
count=1
for (ii in parietal_lobe)
{ parietal_lobe[count]<-paste("reg",ii, sep="")
count=count+1}

central_structures<-c(34, 35, 36, 37, 38, 39,  40, 41, 42, 43,44, 74, 75)
count=1
for (ii in central_structures)
{ central_structures[count]<-paste("reg",ii, sep="")
count=count+1}


ventricles<-c(45,46,47,48, 49)
count=1
for (ii in ventricles)
{ ventricles[count]<-paste("reg",ii, sep="")
count=count+1}

save(list=ls(all.names = TRUE),file = "regions.txt")

### algoritmo kmeans

set.seed(240) # Setting seed
kmeans.re <- kmeans(directions[,1], centers = 3, nstart = 20)
kmeans.re

kmeans.re$cluster
plot(kmeans.re$cluster)
low_1<-kmeans.re$cluster[kmeans.re$cluster==3] # abbinamento a mano guardando valori loadings
high_1<-kmeans.re$cluster[kmeans.re$cluster==2]
middle_1<-kmeans.re$cluster[kmeans.re$cluster==1]

# a colori
color_pallete_function <- colorRampPalette(
  colors = c("red", "orange", "blue"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)
num_colors <- 3
level_colors <- color_pallete_function(num_colors)
df<-data.frame(ordered_dir)
df$grouping<-c(high_1,middle_1,low_1)
df$grouping<-as.factor(df$grouping)

plot(
  directions[ordered_dir,1],
  pch = 20, # solid dots increase the readability of this data plot
  col = level_colors[df$grouping]
)
legend(
  x ="topleft",
  legend = paste("Color", c("middle","low","high")), # for readability of legend
  col = level_colors,
  pch = 19, # same as pch=20, just smaller
  cex = .7 # scale the legend to look attractively sized
)
##


high_1<-names(high_1)
low_1<-names(low_1)
middle_1<-names(middle_1)

high_distr_1<-rep("0",length(high_1))
for (ii in 1:length(high_1))
{
  if (high_1[ii] %in% temporal_lobe)
  {high_distr_1[ii]<-"temporal_lobe"}
  
  if (high_1[ii] %in% posterior_fossa)
  {high_distr_1[ii]<-"posterior_fossa"}
  
  if (high_1[ii] %in% insula_cingulate_gyri)
  {high_distr_1[ii]<-"insula_cingulate_gyri"}
  
  if (high_1[ii] %in% frontal_lobe)
  {high_distr_1[ii]<-"frontal_lobe"}
  
  if (high_1[ii] %in% occipital_lobe)
  {high_distr_1[ii]<-"occipital_lobe"}
  
  if (high_1[ii] %in% parietal_lobe)
  {high_distr_1[ii]<-"parietal_lobe"}
  
  if (high_1[ii] %in% central_structures)
  {high_distr_1[ii]<-"central_structures"}
  
  if (high_1[ii] %in% ventricles)
  {high_distr_1[ii]<-"ventricles"}
}
high_distr_1
table(high_distr_1)



# 1st principal component a colori

color_pallete_function <- colorRampPalette(
  colors = c("red", "orange", "blue","green","yellow", "pink","purple", "black", "brown"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)

num_colors <- 9
level_colors <- color_pallete_function(num_colors)


names<-c(temporal_lobe, posterior_fossa, insula_cingulate_gyri, frontal_lobe, occipital_lobe, parietal_lobe, central_structures, ventricles)
df2<-data.frame(rep("0",83), row.names=names)
df2$grouping<-rep("0",83)
df2$grouping<-c(rep("temporal_lobe",20), rep("posterior_fossa",3), rep("insula_cingulate_gyri",6), rep("frontal_lobe",24), rep("occipital_lobe", 6), rep("parietal_lobe",6), rep("central_structures",13), rep("ventricles",5))
as.factor(df2$grouping)


# ora vanno abbinati loadings e plottati, vedere se esce qualcosa