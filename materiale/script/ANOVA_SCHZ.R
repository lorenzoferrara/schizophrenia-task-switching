
require(data.table)
require(tidyr)

setwd("./materiale/SubjData")
# import files names and participants.csv
BigList = list.files(pattern="*.tsv")
BigList

#importo a mano participants_originale

p <- read.delim("../data/participants.csv")

# etichette<-read.table("participants_originale.tsv", path="C:/Users/Costanza/Downloads")

 

c<-participants_originale[!(participants_originale$diagnosis=="BIPOLAR"),]
participants_originale<-participants_originale[!(participants_originale$diagnosis=="ADHD"),]
participants_originale<-participants_originale[!(participants_originale$taskswitch=="n/a"),]
participants_originale<-participants_originale[,1:2]


#stesso ordine
ii=0

for (individui in BigList)
  {ii=ii+1
  da_leggere<-read.table(individui)
  da_leggere$ReactionTime<-as.numeric(da_leggere$ReactionTime)
  
  media_singola<-NULL
  righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",]
  righe<-righe[righe$Switching=="SWITCH",]

  participants_originale$Congruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)
  
  media_singola<-NULL
  righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
  righe<-righe[righe$Switching=="SWITCH",]
  
  participants_originale$Incongruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)
  
  media_singola<-NULL
  righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",]
  righe<-righe[righe$Switching=="NOSWITCH",]
  
  participants_originale$Congruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)
  
  media_singola<-NULL
  righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
  righe<-righe[righe$Switching=="NOSWITCH",]
  
  participants_originale$Incongruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)
  
  }
  
  
colore <- rep(rainbow(3), each = 50)

x11(width=13)
par(mfrow=c(1,4))
boxplot(participants_originale$Congruent_switch~participants_originale$diagnosis)
boxplot(participants_originale$Incongruent_switch~participants_originale$diagnosis)
boxplot(participants_originale$Congruent_Noswitch~participants_originale$diagnosis)
boxplot(participants_originale$Incongruent_Noswitch~participants_originale$diagnosis)

#shapiro tests

shapiro.test(participants_originale[1:130,3])
shapiro.test(participants_originale[1:130,4])
shapiro.test(participants_originale[1:130,5])
shapiro.test(participants_originale[1:130,6])

shapiro.test(participants_originale[131:175,3])
shapiro.test(participants_originale[131:175,4])
shapiro.test(participants_originale[131:175,5])
shapiro.test(participants_originale[131:175,6])


#omogeneit? varianza
attach(participants_originale)
rows_per_anova<-NULL

rows_per_anova<-data.frame(participants_originale)

k=0
for(jj in 1:175)
{
  rows_per_anova[jj+k,]<-participants_originale[jj,]
  rows_per_anova$type[jj+k]<-1
  rows_per_anova[jj+1+k,]<-participants_originale[jj,]
  rows_per_anova$type[jj+k+1]<-2
  rows_per_anova[jj+2+k,]<-participants_originale[jj,]
  rows_per_anova$type[jj+k+2]<-3
  rows_per_anova[jj+3+k,]<-participants_originale[jj,]
  rows_per_anova$type[jj+k+3]<-4
  k=k+3
}

rows_per_anova$time<-rows_per_anova$Congruent_switch
rows_per_anova$type<-as.numeric(rows_per_anova$type)
for (jj in 1:700)
{
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
bartlett.test(rows_per_anova$time,rows_per_anova$type)

rows_per_anova$type[501:700]<-rows_per_anova$type[501:700]-4

rows_per_anova$type<-as.numeric(rows_per_anova$type)


#### facciamo anova

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
fit <- aov(rows_per_anova$time ~ rows_per_anova$diagnosis + rows_per_anova$congruent + rows_per_anova$switch + rows_per_anova$diagnosis*rows_per_anova$congruent +
           rows_per_anova$diagnosis*rows_per_anova$switch + rows_per_anova$congruent*rows_per_anova$switch)

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

#risposte sbagliate influenzano?
errori <- participants_originale[,1:2]
errori<-as.data.frame(errori)
errori$count<-0
errori$NSwitch<-0
errori$MediaRisposte<-0

ii=0
for (individui in BigList)
{ii=ii+1
da_leggere<-read.table(individui)
da_leggere$CorrectResp<-as.numeric(da_leggere$CorrectResp)


for (jj in 1:96)
{
  if (da_leggere$CorrectResp[jj]==0)
    {errori$count[ii]=errori$count[ii]+1
      if (da_leggere$Switching[jj]=="SWITCH")
        {errori$NSwitch[ii]=errori$NSwitch[ii]+1}}}

errori$MediaRisposte[ii]<-mean(da_leggere$CorrectResp, na.rm = FALSE)
    
}

dev.off()
boxplot(errori$MediaRisposte~errori$diagnosis)

errori$performance_on_switch<-0
for (ii in 1:175)
{
  if (errori$count[ii]!=0)
    errori$performance_on_switch[ii]<- errori$NSwitch[ii]/errori$count[ii]
}

x11()
par(mfrow=c(1,2))
boxplot(errori$MediaRisposte~errori$diagnosis)
boxplot(errori$performance_on_switch~errori$diagnosis)

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

summary(fit) #gi? sapevamo

########
#anova solo su tempi con risposte corrette

ii=0
participants_originale_corr<-participants_originale
for (individui in BigList)
{ii=ii+1
da_leggere<-read.table(individui)
da_leggere$ReactionTime<-as.numeric(da_leggere$ReactionTime)

da_leggere<-da_leggere[!(da_leggere$CorrectResp==0),]

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",]
righe<-righe[righe$Switching=="SWITCH",]

participants_originale_corr$Congruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
righe<-righe[righe$Switching=="SWITCH",]

participants_originale_corr$Incongruent_switch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="CONGRUENT",]
righe<-righe[righe$Switching=="NOSWITCH",]

participants_originale_corr$Congruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)

media_singola<-NULL
righe<-da_leggere[da_leggere$Congruency=="INCONGRUENT",]
righe<-righe[righe$Switching=="NOSWITCH",]

participants_originale_corr$Incongruent_Noswitch[ii]<-mean(righe$ReactionTime, na.rm = TRUE)}


rows_per_anova_corr<-NULL

rows_per_anova_corr<-data.frame(participants_originale_corr)

k=0
for(jj in 1:175)
{
  rows_per_anova_corr[jj+k,]<-participants_originale_corr[jj,]
  rows_per_anova_corr$type[jj+k]<-1
  rows_per_anova_corr[jj+1+k,]<-participants_originale_corr[jj,]
  rows_per_anova_corr$type[jj+k+1]<-2
  rows_per_anova_corr[jj+2+k,]<-participants_originale_corr[jj,]
  rows_per_anova_corr$type[jj+k+2]<-3
  rows_per_anova_corr[jj+3+k,]<-participants_originale_corr[jj,]
  rows_per_anova_corr$type[jj+k+3]<-4
  k=k+3
}

rows_per_anova_corr$time<-rows_per_anova_corr$Congruent_switch
rows_per_anova_corr$type<-as.numeric(rows_per_anova_corr$type)
for (jj in 1:700)
{
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

for(jj in 1:700)
{
  
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
