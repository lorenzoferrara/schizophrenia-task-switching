
phenotype.demographics <- read.delim("./materiale/script/fumo/phenotype.demographics.tsv", sep=" ")
y = with(phenotype.demographics, data.frame(id=participant_id,ever_smoked=cigs, packs_day=cigs_pack, years_smoking=cigs_yrs, months_smoking=cigs_mons,years_quit=cigs_past))

{
# x11()
# barplot(data.frame(factor(y$ever_smoked)))
hist(y$ever_smoked)
}

indexes_not = (y$ever_smoked==0)
not_smokers = y[indexes_not,]

y2<-y[y$ever_smoked>0,]

y2$years_smoking<-as.numeric(y2$years_smoking)
y2$months_smoking<-as.numeric(y2$months_smoking)
for (ii in 1:length(y2$months_smoking)){ 
  if (is.na(y2$years_smoking[ii]))
     {y2$years_smoking[ii]<-y2$months_smoking[ii]/12}
}

if("months_smoking" %in% names(y2))
  y2=y2[,names(y2)!="months_smoking"]

y2$packs_day<-as.numeric(y2$packs_day)

y2$ever_smoked=as.factor(y2$ever_smoked)
current_smokers<-y2[y2$ever_smoked=="1",]
ex_smokers<-y2[y2$ever_smoked=="2",]


cols<-na.omit(current_smokers) #cols ha senza na
plot(current_smokers$years_smoking,current_smokers$cigs_day)
boxplot(cols[c(3,4)])

s<-cov(cols[c(3,4)])
eigen(s)


current_smokers$index<-rep(-1,length(current_smokers$id))
cols$index<-rep(-1,length(cols$id))

#cols$index ? 52

for (ii in 1:length(cols$id))
{if(scaled_t[ii]<=-0.5) {cols$index[ii]=0}
  else if(scaled_t[ii]>-0.5 && scaled_t[ii]<0.5) {cols$index[ii]=1}
  else {cols$index[ii]=2}}

na_to_add<-subset(current_smokers,is.na(current_smokers$years_smoking))
cur_final<-rbind(cols,na_to_add)

not_smokers$ever_smoked<-as.factor(not_smokers$ever_smoked)
not_smokers$months_smoking<-NULL
not_smokers$packs_day<-NULL
not_smokers$index<-rep(-1,length(not_smokers$id))
not_smokers$years_smoking<-as.numeric(not_smokers$years_smoking)

all_except_past<-rbind(cur_final,not_smokers)
all_except_past$index<-as.factor(all_except_past$index)


## per ex

cols<-na.omit(ex_smokers)#sto togliendo righe dove sigarette al giorno/anni non disponibili

plot(ex_smokers$years_smoking,ex_smokers$cigs_day)
boxplot(cols[c(3,4)])

s<-cov(cols[c(3,4)])
eigen(s) #come prima, pca sostanzialmente riporta solo una dimensione (anni)

cols$years_quit<-as.numeric(cols$years_quit)
sum(is.na(cols)) #18 osservazioni su 51 non riportano da quanto hanno smesso di fumare
# possiamo escludere queste osservazioni per fare pca e calcolare indici? Ma poi indici di queste 18 osservazioni
# come li otteniamo? (abbiamo anche gi? escluso alcuni NA su years_smoking e cigs_day

# Provo a procedere come prima 

tab<-cols$cigs_day*365*cols$years_smoking
hist(tab)
scaled_t<-scale(tab)
hist(scaled_t) 

ex_smokers$index<-rep(-1,length(ex_smokers$id))
cols$index<-rep(-1,length(cols$id))

for (ii in 1:length(cols$id))
{if(scaled_t[ii]<=-0.5) {cols$index[ii]=0}
  else if(scaled_t[ii]>-0.5 && scaled_t[ii]<0.5) {cols$index[ii]=1}
  else {cols$index[ii]=2}}


na_to_add_1<-subset(ex_smokers,is.na(ex_smokers$cigs_day))
na_to_add_1$index<-as.factor(na_to_add_1$index)

tot1<-rbind(cols,na_to_add_1)

smoking_profile<-rbind(all_except_past, tot1)


###############################################################################################
########################################################################
# considerazioni geometriche?
current_smokers$years_quit = as.numeric(current_smokers$years_quit)
matplot(t(current_smokers), type='l', main = 'Data', ylim=range(current_smokers))

meanF <- colMeans(cols)
matplot(meanF, type='l', main = '0 PC', lwd=2, ylim=range(cols))
for(i in 1:2)
{
  projection <- matrix(meanF, dim(cols)[[1]], dim(cols)[[2]], byrow=T) + cur_scores[,i] %*% t(cur_load[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(cols))
  matplot(meanF, type='l', lwd=2, add=T)
}

save(list=c("ex_smokers", "current_smokers", "not_smokers"), file = "smoking.RData")
