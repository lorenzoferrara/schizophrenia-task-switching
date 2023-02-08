require(data.table)
require(tidyr)
#setwd("C:/Users/Costanza/Downloads/SubjData/SubjData")
setwd("./materiale/SubjData")

# import recordings file names and participants.csv
BigList = list.files(pattern="*.tsv")
BigList

# elenco_copia <- read.delim("./elenco_copia.txt", header=T)
# elenco <- read.delim("./elenco.txt", header=T)
# BigList = elenco$v

participants<-read.table("../data/participants.csv", header = T)
p<-read.table("../data/participants.csv", header = T)
participants <- participants[,1:2] 

#same order
ii=0
numero_switch=rep(0, 175)
v<-rep()
for (individui in BigList){
  ii=ii+1
  da_leggere<-read.table(individui)   # import sub-*****_task-taskswitch_events.tsv in SubjData
  da_leggere$ReactionTime<-as.numeric(da_leggere$ReactionTime)    
  
  numero_switch[ii] = sum(da_leggere$Switching == "SWITCH")
  
  # save only the variables corresponding to CONGRUENT and SWITCH
  righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",] 
  righe<-righe[righe$Switching=="SWITCH",]
  

  # create a new column in participants with the mean RT of CONGRUENT and SWITCH trials of individuo
  participants$Congruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

  # save only the variables corresponding to INCONGRUENT and SWITCH of individuo
  righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
  righe<-righe[righe$Switching=="SWITCH",]
  

  # create a new column in participants with the mean RT of INCONGRUENT and SWITCH trials of individuo
  participants$Incongruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

  # save only the variables corresponding to CONGRUENT and NO SWITCH of individuo
  righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",]
  righe<-righe[righe$Switching=="NOSWITCH",]

  # create a new column in participants with the mean RT of CONGRUENT and NO SWITCH trials of individuo
  participants$Congruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

  # save only the variables corresponding to INCONGRUENT and NO SWITCH of individuo
  righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
  righe<-righe[righe$Switching=="NOSWITCH",]

  # create a new column in participants with the mean RT of INCONGRUENT and NO SWITCH trials of individuo
  participants$Incongruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)
}
## COMMENTI: 1. quando fa il primo giro di for, nelle colonne create viene inserito per tutte le righe il mean RT del primo individui in BigList. va bene? 
##           2. prima nel ciclo for veniva inizalizzata dopo ogni comando una variabile (media_singola<-NULL) senza mai essere usata quindil'ho eliminata. Serviva a qualcosa?

# BOXPLOT of the mean of each pair Congruent/Incongruent - Switch/NoSwitch relatively to the disgnosis
colore <- rep(rainbow(3), each = 50)
{
  x11(width=13)
  par(mfrow=c(1,4))
  boxplot(participants$Congruent_switch~participants$diagnosis, xlab = "Diagnosis", ylab = "Mean RT-Congruent_Switch", cex.lab=1.3)
  boxplot(participants$Incongruent_switch~participants$diagnosis, xlab = "Diagnosis", ylab = "Mean RT-Incongruent_Switch", cex.lab=1.3)
  boxplot(participants$Congruent_Noswitch~participants$diagnosis, xlab = "Diagnosis", ylab = "Mean RT-Congruent_NoSwitch", cex.lab=1.3)
  boxplot(participants$Incongruent_Noswitch~participants$diagnosis, xlab = "Diagnosis", ylab = "Mean RT-Incongruent_NoSwitch", cex.lab=1.3)
}

# Shapiro tests for Control
shapiro.test(participants[1:125,3])   # p-value = 0.001116
shapiro.test(participants[1:125,4])   # p-value = 0.1671
shapiro.test(participants[1:125,5])   # p-value = 0.006437
shapiro.test(participants[1:125,6])   # p-value = 0.0166

# Shapiro tests for Schizo
shapiro.test(participants[126:175,3])   # p-value = 0.1145
shapiro.test(participants[126:175,4])   # p-value = 0.2752
shapiro.test(participants[126:175,5])   # p-value = 0.4856
shapiro.test(participants[126:175,6])   # p-value = 0.6523

# Homogeneity of the variance
attach(participants)
rows_per_anova<-NULL

rows_per_anova<-data.frame(participants)

k=0
for(jj in 1:175)
{
  rows_per_anova[jj+k,]<-participants[jj,]
  rows_per_anova$type[jj+k]<-1                # type 1 = CONGRUENT and SWITCH
  rows_per_anova[jj+1+k,]<-participants[jj,]
  rows_per_anova$type[jj+k+1]<-2              # type 2 = INCONGRUENT and SWITCH
  rows_per_anova[jj+2+k,]<-participants[jj,]
  rows_per_anova$type[jj+k+2]<-3              # type 3 = CONGRUENT and NOSWITCH
  rows_per_anova[jj+3+k,]<-participants[jj,]
  rows_per_anova$type[jj+k+3]<-4              # type 4 = INCONGRUENT and NOSWITCH
  k=k+3
}

rows_per_anova$time<-rows_per_anova$Congruent_switch
rows_per_anova$type<-as.numeric(rows_per_anova$type)
for (jj in 1:700)
{
  # COMMENTO: Questo primo if credo si possa evitare perchè i valori sono già stati inseriti
  if (rows_per_anova$type[jj]==1)
    rows_per_anova$time[jj]<-rows_per_anova$Congruent_switch[jj]
  
  if (rows_per_anova$type[jj]==2)
    rows_per_anova$time[jj]<-rows_per_anova$Incongruent_switch[jj]
  
  if (rows_per_anova$type[jj]==3)
    rows_per_anova$time[jj]<-rows_per_anova$Congruent_Noswitch[jj]
  
  if (rows_per_anova$type[jj]==4)
    rows_per_anova$time[jj]<-rows_per_anova$Incongruent_Noswitch[jj]
}

rows_per_anova$diagnosis<-as.factor(rows_per_anova$diagnosis)

rows_per_anova$type[501:700]<-rows_per_anova$type[501:700]+4
bartlett.test(rows_per_anova$time,rows_per_anova$type)      # p-value = 0.1156

rows_per_anova$type[501:700]<-rows_per_anova$type[501:700]-4

rows_per_anova$type<-as.numeric(rows_per_anova$type)

#### ANOVA

rows_per_anova$switch<-1
rows_per_anova$congruent<-1

for(jj in 1:700)
{
  if (rows_per_anova$type[jj]==2)
    rows_per_anova$congruent[jj]<-0
  
  if (rows_per_anova$type[jj]==3)
    rows_per_anova$switch[jj]<-0
  
  if (rows_per_anova$type[jj]==4)
  {
    rows_per_anova$congruent[jj]<-0
    rows_per_anova$switch[jj]<-0
  }
}

rows_per_anova$type<-as.factor(rows_per_anova$type)
rows_per_anova$congruent<-as.factor(rows_per_anova$congruent)
rows_per_anova$switch<-as.factor(rows_per_anova$switch)
fit <- aov(rows_per_anova$time ~ rows_per_anova$diagnosis * rows_per_anova$congruent * rows_per_anova$switch)

summary(fit)

# provo a semplificare il modello
fit <- aov(rows_per_anova$time ~ rows_per_anova$diagnosis + rows_per_anova$congruent + rows_per_anova$switch + rows_per_anova$diagnosis*rows_per_anova$congruent +
             rows_per_anova$diagnosis*rows_per_anova$switch)

summary(fit)

# semplifichiamo ancora
fit <- aov(rows_per_anova$time ~ rows_per_anova$diagnosis + rows_per_anova$congruent + rows_per_anova$switch +
             rows_per_anova$diagnosis*rows_per_anova$switch)

summary(fit)

# and again
fit1 <- aov(rows_per_anova$time ~ rows_per_anova$diagnosis + rows_per_anova$congruent + rows_per_anova$switch)

summary(fit1)

# possiamo provare senza congruent
fit2 <- aov(rows_per_anova$time ~ rows_per_anova$diagnosis + rows_per_anova$switch)

summary(fit2)

# capiamo come interagiscono diagnosi e switch
fit_domanda<- aov(rows_per_anova$time ~ rows_per_anova$diagnosis + rows_per_anova$switch+  rows_per_anova$diagnosis*rows_per_anova$switch)
summary(fit_domanda)

model1<-lm(rows_per_anova$time ~ rows_per_anova$diagnosis + rows_per_anova$switch)
summary(model1)

# risposte sbagliate influenzano?
errori <- participants[,1:2]
errori<-as.data.frame(errori)
errori$count<-0
errori$NSwitch<-0
errori$MediaRisposte<-0

ii=0
for (individui in BigList) {
  ii=ii+1
  da_leggere<-read.table(individui)
  da_leggere$CorrectResp<-as.numeric(da_leggere$CorrectResp)

  for (jj in 1:96)
  {
    if (da_leggere$CorrectResp[jj]==0)
    {errori$count[ii]=errori$count[ii]+1
    if (da_leggere$Switching[jj]=="SWITCH")
    {errori$NSwitch[ii]=errori$NSwitch[ii]+1}}
  }
  
  errori$MediaRisposte[ii]<-mean(da_leggere$CorrectResp, na.rm = FALSE)
}


dev.off()
{x11()
boxplot(errori$MediaRisposte~errori$diagnosis, xlab = "Diagnosis", ylab = "Mean Value Correct Responses", cex.lab=1.2)
}
errori$performance_on_switch<-0
errori$performance1_on_switch<-0
errori$performance2_on_switch
for (ii in 1:175) {
  if (errori$count[ii]!=0){
    errori$performance_on_switch[ii] <- errori$NSwitch[ii]/errori$count[ii]
    errori$performance1_on_switch[ii] <- errori$NSwitch[ii]/24  # No. of errors in Switch trials/total Switch trials
    errori$performance1_on_switch[ii] <- errori$NSwitch[ii]/numero_switch[ii]  # No. of errors in Switch trials/total Switch trials
    errori$performance1_on_nonswitch[ii] <- ( errori$count[ii] - errori$NSwitch[ii]) / (96 - numero_switch[ii])  # No. of errors in Switch trials/total Switch trials
  }
}
##correzione riga 217 
#  errori$performance1_on_switch[ii] <- errori$NSwitch[ii]/numero_switch[ii]  # No. of errors in Switch trials/total Switch trials

{
  x11()
  par(mfrow=c(1,2))
  boxplot(errori$MediaRisposte~errori$diagnosis, xlab = "Diagnosis", ylab = "Mean Value Correct Responses")
  boxplot(errori$performance_on_switch~errori$diagnosis, xlab = "Diagnosis", ylab = "No. errors on Switch / No. total errors")
}
# NUOVO BOXPLOT: esce strano pleeeease look at this!! AIUTO :o
{
  x11()
  par(mfrow=c(1,2))
  boxplot(errori$performance1_on_switch~errori$diagnosis, xlab = "Diagnosis", ylab = "No. errors on Switch / No. Switch trials", cex.lab=1.2)
  boxplot(errori$performance1_on_nonswitch~errori$diagnosis, xlab = "Diagnosis", ylab = "No. errors on nonSwitch / No. nonSwitch trials", cex.lab=1.2)
}

rows_per_anova$Mediarisposte<-0
k=0
for(jj in 1:175)
{
  rows_per_anova$Mediarisposte[jj+k]<-errori$MediaRisposte[jj]
  rows_per_anova$Mediarisposte[jj+k+1]<-errori$MediaRisposte[jj]
  rows_per_anova$Mediarisposte[jj+k+2]<-errori$MediaRisposte[jj]
  rows_per_anova$Mediarisposte[jj+k+3]<-errori$MediaRisposte[jj]
  k=k+3
}

# anova che spiega n risposte corrette o sbagliate a seconda di diagnosis e switch
fit <- aov(rows_per_anova$Mediarisposte ~ rows_per_anova$diagnosis + rows_per_anova$congruent + rows_per_anova$switch + rows_per_anova$diagnosis*rows_per_anova$congruent +
             rows_per_anova$diagnosis*rows_per_anova$switch + rows_per_anova$congruent*rows_per_anova$switch)

summary(fit) #gia sapevamo

########
#anova solo su tempi con risposte corrette

ii=0
participants_corr<-participants
for (individui in BigList)
{ii=ii+1
da_leggere<-read.table(individui)
da_leggere$ReactionTime<-as.numeric(da_leggere$ReactionTime)

da_leggere<-da_leggere[!(da_leggere$CorrectResp==0),]

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",]
righe<-righe[righe$Switching=="SWITCH",]

participants_corr$Congruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
righe<-righe[righe$Switching=="SWITCH",]

participants_corr$Incongruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",]
righe<-righe[righe$Switching=="NOSWITCH",]

participants_corr$Congruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
righe<-righe[righe$Switching=="NOSWITCH",]

participants_corr$Incongruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)}


rows_per_anova_corr<-NULL

rows_per_anova_corr<-data.frame(participants_corr)

k=0
for(jj in 1:175) {
  rows_per_anova_corr[jj+k,]<-participants_corr[jj,]
  rows_per_anova_corr$type[jj+k]<-1
  rows_per_anova_corr[jj+1+k,]<-participants_corr[jj,]
  rows_per_anova_corr$type[jj+k+1]<-2
  rows_per_anova_corr[jj+2+k,]<-participants_corr[jj,]
  rows_per_anova_corr$type[jj+k+2]<-3
  rows_per_anova_corr[jj+3+k,]<-participants_corr[jj,]
  rows_per_anova_corr$type[jj+k+3]<-4
  k=k+3
}

rows_per_anova_corr$time<-rows_per_anova_corr$Congruent_switch
rows_per_anova_corr$type<-as.numeric(rows_per_anova_corr$type)
for (jj in 1:700) {
  if (rows_per_anova_corr$type[jj]==1)
    rows_per_anova_corr$time[jj]<-rows_per_anova_corr$Congruent_switch[jj]
  
  if (rows_per_anova_corr$type[jj]==2)
    rows_per_anova_corr$time[jj]<-rows_per_anova_corr$Incongruent_switch[jj]
  
  if (rows_per_anova_corr$type[jj]==3)
    rows_per_anova_corr$time[jj]<-rows_per_anova_corr$Congruent_Noswitch[jj]
  
  if (rows_per_anova_corr$type[jj]==4)
    rows_per_anova_corr$time[jj]<-rows_per_anova_corr$Incongruent_Noswitch[jj]
}

rows_per_anova_corr$diagnosis<-as.factor(rows_per_anova_corr$diagnosis)
rows_per_anova_corr$switch<-1
rows_per_anova_corr$congruent<-1

for(jj in 1:700) {
  if (rows_per_anova_corr$type[jj]==2)
    rows_per_anova_corr$congruent[jj]<-0
  
  if (rows_per_anova_corr$type[jj]==3)
    rows_per_anova_corr$switch[jj]<-0
  
  if (rows_per_anova_corr$type[jj]==4)
  {
    rows_per_anova_corr$congruent[jj]<-0
    rows_per_anova_corr$switch[jj]<-0
  }
}

rows_per_anova_corr$type<-as.factor(rows_per_anova_corr$type)
rows_per_anova_corr$congruent<-as.factor(rows_per_anova_corr$congruent)
rows_per_anova_corr$switch<-as.factor(rows_per_anova_corr$switch)
fit <- aov(rows_per_anova_corr$time ~ rows_per_anova_corr$diagnosis + rows_per_anova_corr$congruent + rows_per_anova_corr$switch + rows_per_anova_corr$diagnosis*rows_per_anova_corr$congruent +
             rows_per_anova_corr$diagnosis*rows_per_anova_corr$switch + rows_per_anova_corr$congruent*rows_per_anova_corr$switch)

summary(fit)

#semplifico come sopra

fit <- aov(rows_per_anova_corr$time ~ rows_per_anova_corr$diagnosis + rows_per_anova_corr$congruent + rows_per_anova_corr$switch + rows_per_anova_corr$diagnosis*rows_per_anova_corr$congruent +
             rows_per_anova_corr$diagnosis*rows_per_anova_corr$switch)

summary(fit)

# semplifichiamo ancora
fit <- aov(rows_per_anova_corr$time ~ rows_per_anova_corr$diagnosis + rows_per_anova_corr$congruent + rows_per_anova_corr$switch +
             rows_per_anova_corr$diagnosis*rows_per_anova_corr$switch)

summary(fit)

# and again
fit1 <- aov(rows_per_anova_corr$time ~ rows_per_anova_corr$diagnosis + rows_per_anova_corr$congruent + rows_per_anova_corr$switch)

summary(fit1)

# possiamo provare senza congruent
fit2 <- aov(rows_per_anova_corr$time ~ rows_per_anova_corr$diagnosis + rows_per_anova_corr$switch)

summary(fit2)

# capiamo come interagiscono diagnosi e switch
fit_domanda<- aov(rows_per_anova_corr$time ~ rows_per_anova_corr$diagnosis + rows_per_anova_corr$switch+  rows_per_anova_corr$diagnosis*rows_per_anova_corr$switch)
summary(fit_domanda)

model1<-lm(rows_per_anova_corr$time ~ rows_per_anova_corr$diagnosis + rows_per_anova_corr$switch)
summary(model1)

