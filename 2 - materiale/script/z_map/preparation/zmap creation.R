rm(list=ls())

# change according to your needs
setwd("./Lorenzo/script/z_map")
source('utils/fdaPDE.write.vtk.R')


# install from GitHub at: https://github.com/fdaPDE/fdaPDE
library(fdaPDE)  

# import and create mesh, plus the Finite Element object to visualize it, and the Hammer's atlas labels
nodes <-read.csv('../../data/mesh/node_spaziodati_onlygm.csv', header = F)
elem <-read.csv('../../data/mesh/elem_onlygm.csv', header = F)
mesh <- create.mesh.3D(nodes = nodes, tetrahedrons = elem[,1:4])
FEMbasis <- create.FEM.basis(mesh = mesh)
# plot(mesh)
labels <-as.numeric(read.csv('../../data/mesh/labels_onlygm.csv', header = F))

# !!! IMPORTANT: use the file name according to your data
# import your connectivity maps 
z_map <- read.csv2('../../data/z_maps_TASKSWITCH_SCHZ_NAMES.csv', 
                    header = T, 
                    sep = ',',
                    dec = '.')
# use the first column as rownames and then drop it
rownames(z_map) <- z_map[,1]
z_map <- z_map[,-1]
# substitute NaN (Matlab) with NA (R)
z_map <-as.matrix(z_map)
z_map[is.nan(z_map)]<-NA

# import all participants data 
allparticipants <- read.csv('../../data/participants.csv', header = T, sep = '\t')

# extract the name of the participants of your interest
sbj<-rownames(z_map)
part_ofinterest <-allparticipants[allparticipants$participant_id %in% sbj,]


# visualize the abbreviations used in the dataset 
unique(allparticipants$diagnosis)
# !!! IMPORTANT: insert the pathology of your interest (otherwise it won't work)
# and compute mean maps for each group 
ctrl_mean <- colMeans(z_map[(part_ofinterest$diagnosis == 'CONTROL'), ], na.rm = T)
schz_mean <- colMeans(z_map[(part_ofinterest$diagnosis == 'SCHZ'), ], na.rm = T)

# and plot them on Paraview format
FEM_ctrl <- FEM(ctrl_mean, FEMbasis)
write.vtu(FEM_ctrl, file = 'ctrl_mean.vtu')
FEM_schz <- FEM(schz_mean, FEMbasis)
write.vtu(FEM_schz, file = 'schz_mean.vtu')

# to visualize directly in R:
plot(FEM_ctrl)
plot(FEM_schz)

