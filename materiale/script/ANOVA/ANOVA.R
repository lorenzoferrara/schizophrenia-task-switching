require(data.table)
require(tidyr)

setwd("./materiale/SubjData")
setwd("./materiale/SubjData")

# import recordings file names and participants.csv
BigList = list.files(pattern="*.tsv")
BigList

participants<-read.table("../data/participants.csv", header = T)
p<-read.table("../data/participants.csv", header = T)
participants <- participants[,1:2] 

#same order
ii=0
numero_switch=rep(0, 175)
for (individui in BigList){
  ii=ii+1
  da_leggere<-read.table(individui, header = T)   # import sub-*****_task-taskswitch_events.tsv in SubjData
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

# BOXPLOT of the mean of each pair Congruent/Incongruent - Switch/NoSwitch relatively to the disgnosis
colore <- rep(rainbow(3), each = 50)

{Congruent_switch_Sani = participants$Congruent_switch[1:125]
Congruent_switch_Schz = participants$Congruent_switch[126:175]
Incongruent_switch_Sani = participants$Incongruent_switch[1:125]
Incongruent_switch_Schz = participants$Incongruent_switch[126:175]
Congruent_Noswitch_Sani = participants$Congruent_Noswitch[1:125]
Congruent_Noswitch_Schz = participants$Congruent_Noswitch[126:175]
Incongruent_Noswitch_Sani = participants$Incongruent_Noswitch[1:125]
Incongruent_Noswitch_Schz = participants$Incongruent_Noswitch[126:175]

massimo = max( participants$Congruent_switch, participants$Incongruent_switch, participants$Congruent_Noswitch, participants$Incongruent_Noswitch )
minimo = min( participants$Congruent_switch, participants$Incongruent_switch, participants$Congruent_Noswitch, participants$Incongruent_Noswitch )
}

color=c("lightskyblue1", "tomato3")       
{
  x11(width=13)
  boxplot(Congruent_switch_Sani, Congruent_switch_Schz, Incongruent_switch_Sani, Incongruent_switch_Schz,
          Congruent_Noswitch_Sani, Congruent_Noswitch_Schz, Incongruent_Noswitch_Sani, Incongruent_Noswitch_Schz,
          col=color, names=c("Congruent \nSwitch", "Congruent \nSwitch" , "Incongruent \nSwitch", "Incongruent \nSwitch",
                             "Congruent \nNoswitch", "Congruent \nNoswitch", "Incongruent \nNoswitch", "Incongruent \nNoswitch"), cex.axis=0.9, ylab="Mean Reaction Time")
  legend( "topright", col=color, c("Control","Schizophrenic"),lty = 19, lwd=4)
}

{x11()
  library(ggplot2)
  library(gridExtra)
  p1 = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Congruent_switch=c(Congruent_switch_Sani,Congruent_switch_Schz)),aes(x=Diagnosis,y=Congruent_switch))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "")+
    xlab( "Congruent_switch" ) + ylab("") + ylim(minimo, massimo)
  p2 = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Incongruent_switch=c(Incongruent_switch_Sani,Incongruent_switch_Schz)),aes(x=Diagnosis,y=Incongruent_switch))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "", axis.text.y = element_blank())+
    xlab( "Incongruent_switch" ) + ylab("") + ylim(minimo, massimo)
  p3 = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Congruent_Noswitch=c(Congruent_Noswitch_Sani,Congruent_Noswitch_Schz)),aes(x=Diagnosis,y=Congruent_Noswitch))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "", axis.text.y = element_blank())+
    xlab( "Congruent_Noswitch" ) + ylab("") + ylim(minimo, massimo)
  p4 = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), Incongruent_Noswitch=c(Incongruent_Noswitch_Sani,Incongruent_Noswitch_Schz)),aes(x=Diagnosis,y=Incongruent_Noswitch))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "", axis.text.y = element_blank())+
    xlab( "Incongruent_Noswitch" ) + ylab("") + ylim(minimo, massimo)
  
  grid.arrange(p1,p2,p3,p4, nrow=1,top=" Reactions times")
}

######################################################


Diagnosis =  rep(c(rep("Control",125),rep("Schz",50)),4)
trattamento = c( rep("Congruent_switch",175), rep("Incongruent_switch",175), rep("Congruent_Noswitch",175), rep("Incongruent_Noswitch",175))
dati = c(Congruent_switch_Sani,Congruent_switch_Schz, Incongruent_switch_Sani,Incongruent_switch_Schz, Congruent_Noswitch_Sani,
  Congruent_Noswitch_Schz, Incongruent_Noswitch_Sani,Incongruent_Noswitch_Schz)

dataframe = data.frame( Diagnosis=Diagnosis,
                        trattamento=trattamento,
                        dati=dati
                        )
{x11(width=9)
ggplot(dataframe, aes(x=trattamento, y=dati, fill=Diagnosis)) + geom_boxplot() + scale_fill_manual(values = c("royalblue","orange2"))+
  theme( panel.background = element_rect(fill="white"), text = element_text(size=16, face ="bold"), legend.position = "", 
         plot.title = element_text(hjust=0.5, size=20, face = "bold")) + 
  xlab("") + ylab("") + labs(title="Reaction Time")
}

######################################################

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

setwd("./SubjData")


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

rispost = errori$MediaRisposte
{library(ggplot2)
  x11()
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), MediaRisposte=rispost),aes(x=Diagnosis,y=MediaRisposte))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5, outlier.stroke = 0.1)+
    scale_fill_manual(values = c("royalblue","orange2"))+
    theme_minimal()+theme(legend.position = "", plot.title = element_text(hjust=0.5)) + xlab("") + ylab("") + labs(title = "General accuracy")
  pp
}


#OPPURE

which(rispost<0.4)
rispost = rispost[rispost>0.4]
{library(ggplot2)
x11(width=4)
pp = ggplot(data.frame(Diagnosis=c(rep("Control",123),rep("Schz",48)), MediaRisposte=rispost),aes(x=Diagnosis,y=MediaRisposte))+
  geom_boxplot(aes(fill=Diagnosis),width=0.5, outlier.stroke = 0.1)+
  scale_fill_manual(values = c("royalblue","orange2"))+ ylim(0,1)+
  theme_minimal()+theme(text = element_text(size=18), legend.position = "", plot.title = element_text(hjust=0.5, size=18, face = "bold")) +
  xlab("") + ylab("") + labs(title = "General accuracy")
pp
}
errori$performance_on_switch<-0
errori$performance1_on_switch<-0
errori$performance2_on_switch
for (ii in 1:175) {
  if (errori$count[ii]!=0){
    errori$performance_on_switch[ii] <- errori$NSwitch[ii]/errori$count[ii]
    errori$performance1_on_switch[ii] <- errori$NSwitch[ii]/24  # No. of errors in Switch trials/total Switch trials
    errori$performance1_on_switch[ii] <- errori$NSwitch[ii]/numero_switch[ii]  # No. of errors in Switch trials/total Switch trials
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

{x11( width = 4.3)
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), MediaRisposte=errori$performance_on_switch),aes(x=Diagnosis,y=MediaRisposte))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5, outlier.stroke = 0.1)+
    scale_fill_manual(values = c("royalblue","orange2"))+ xlab("")+ylab("")+
    theme_minimal()+theme(legend.position = "" , text = element_text(size=20), 
                          plot.title = element_text(hjust=0.5, size=17, face = "bold")) + 
    labs(title = "Percentage of switch errors \n over total trials")
  pp
}

{x11( width = 4.3)
  pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), MediaRisposte= 1 - errori$performance_on_switch),aes(x=Diagnosis,y=MediaRisposte))+
    geom_boxplot(aes(fill=Diagnosis),width=0.5, outlier.stroke = 0.1)+
    scale_fill_manual(values = c("royalblue","orange2"))+ xlab("")+ylab("")+
    theme_minimal()+theme(legend.position = "" , text = element_text(size=20), 
                          plot.title = element_text(hjust=0.5, size=17, face = "bold")) + 
    labs(title = "Switch accuracy rate")
  pp
}

shapiro.test(errori$performance_on_switch[1:125])
t.test(errori$performance_on_switch[1:125], errori$performance_on_switch[126:175], alternative = "less")

{x11( width = 4.3)
pp = ggplot(data.frame(Diagnosis=c(rep("Control",125),rep("Schz",50)), MediaRisposte=errori$performance1_on_switch),aes(x=Diagnosis,y=MediaRisposte))+
  geom_boxplot(aes(fill=Diagnosis),width=0.5, outlier.stroke = 0.1)+
  scale_fill_manual(values = c("royalblue","orange2"))+ xlab("")+ylab("")+
  theme_minimal()+theme(legend.position = "", plot.title = element_text(hjust=0.5,, size=17, face = "bold"),text = element_text(size=20) ) + 
  labs(title = "Percentage of switch errors \n over total switch trials")
pp
}

shapiro.test(log(errori$performance1_on_switch[errori$performance1_on_switch!=0]))
t.test(errori$performance1_on_switch[1:125], errori$performance1_on_switch[126:175], alternative = "greater")

# NUOVO BOXPLOT: esce strano pleeeease look at this!! AIUTO :o
{
  x11()
  boxplot(errori$performance1_on_switch~errori$diagnosis, xlab = "Diagnosis", ylab = "No. errors on Switch / No. Switch trials", cex.lab=1.2)
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
