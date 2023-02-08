rm(list=ls())

load("./Erica/script/Participants_wd.RData")

# Import sub-*****_task.taskswitch_events.tsv from the folder events recording (CONTROL)
nam <- NA
setwd("./materiale/events recording")
for(i in 1 : n_control){
  nam <- paste ("",sub_control[i], sep = "")
  nam <- gsub("-","_",nam)  # renamed the participant_id because "-" gives problem
  filename <- paste0 (sub_control[i], "_task-taskswitch_events.tsv")
  data <- read.table (filename, header = T)
  assign (nam,data)
}

# Import sub-*****_task.taskswitch_events.tsv from the folder events recording (SCHZ)
# nam <- NA
for(i in 1 : n_schz){
  nam <- paste ("",sub_schz[i], sep = "")
  nam <- gsub("-","_",nam)
  filename <- paste0 (sub_schz[i], "_task-taskswitch_events.tsv")
  data <- read.table (filename, header = T)
  assign (nam,data)
}

##############################################################################################
### EXAMPLE1: Consider a subject of control and verify if it's Gaussian

RT_sub_10159_Switch<-sub_10159$ReactionTime[sub_10159$Switching=="SWITCH"]
RT_sub_10159_NoSwitch<-sub_10159$ReactionTime[sub_10159$Switching=="NOSWITCH"]

# 1. Verify if the RT_switch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_10159_Switch.jpg")
qqnorm(RT_sub_10159_Switch, main = "Normal Q-Q Plot (SWITCH)")  # quantile-quantile plot
qqline(RT_sub_10159_Switch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot it seems to be gaussian with some outliers 

# SHAPIRO-WILK TEST to quantitatively verify the Gaussian assumption 
# H0: normality of data vs H1: not-normality
# We compare the p-value of the Shapiro test with alpha = 0.05 
# if p-value < alpha -> reject H0, otherwise we can't reject H0

#NormalityRecode <- NA
alpha <- 0.05
if (shapiro.test(RT_sub_10159_Switch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We can't reject H0: the variable is normally distributed. 
## p-value = 0.07850193

# 2. Verify if the RT_NoSwitch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_10159_NoSwitch.jpg")
qqnorm(RT_sub_10159_NoSwitch,main = "Normal Q-Q Plot (NO SWITCH)")  # quantile-quantile plot
qqline(RT_sub_10159_NoSwitch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot doen't seem to be Gaussian distributed (confirmed by the Shapiro test)

# SHAPIRO-WILK TEST
if (shapiro.test(RT_sub_10159_NoSwitch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We reject H0:the variable is not normally distributed.
## p-value = 9.737361e-05

##############################################################################################
### EXAMPLE2: Consider sub_11112 of control and verify if it's Gaussian

RT_sub_11112_Switch <- as.numeric(sub_11112$ReactionTime[sub_11112$Switching=="SWITCH"])
RT_sub_11112_Switch <- as.numeric(na.omit(RT_sub_11112_Switch))
RT_sub_11112_NoSwitch <- as.numeric(sub_11112$ReactionTime[sub_11112$Switching=="NOSWITCH"])
RT_sub_11112_NoSwitch <- as.numeric(na.omit(RT_sub_11112_NoSwitch))

# 1. Verify if the RT_switch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_11112_Switch.jpg")
qqnorm(RT_sub_11112_Switch, main = "Normal Q-Q Plot (SWITCH)")  # quantile-quantile plot
qqline(RT_sub_11112_Switch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot it seems not to be Gaussian (confirmed by the Shapiro test)

# SHAPIRO-WILK TEST
alpha <- 0.05
if (shapiro.test(RT_sub_11112_Switch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We reject H0:the variable is not normally distributed.
## p-value = 0.007998964

# 2. Verify if the RT_NoSwitch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_11112_NoSwitch.jpg")
qqnorm(RT_sub_11112_NoSwitch,main = "Normal Q-Q Plot (NO SWITCH)")  # quantile-quantile plot
qqline(RT_sub_11112_NoSwitch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot it seems to be Gaussian distributed

# SHAPIRO-WILK TEST
if (shapiro.test(RT_sub_11112_NoSwitch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We can't reject H0: the variable is normally distributed.
## p-value =  0.1062755

##############################################################################################
### EXAMPLE3: Consider a subject with schizophrenia and verify if it's Gaussian

RT_sub_50004_Switch <- as.numeric(sub_50004$ReactionTime[sub_50004$Switching=="SWITCH"])
RT_sub_50004_Switch <- as.numeric(na.omit(RT_sub_50004_Switch))
RT_sub_50004_NoSwitch <- as.numeric(sub_50004$ReactionTime[sub_50004$Switching=="NOSWITCH"])
RT_sub_50004_NoSwitch <- as.numeric(na.omit(RT_sub_50004_NoSwitch))

# 1. Verify if the RT_switch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_50004_Switch.jpg")
qqnorm(RT_sub_50004_Switch, main = "Normal Q-Q Plot (SWITCH)")  # quantile-quantile plot
qqline(RT_sub_50004_Switch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot it seems to be gaussian with some outliers 

# SHAPIRO-WILK TEST 
alpha <- 0.05
if (shapiro.test(RT_sub_50004_Switch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We can't reject H0: the variable is normally distributed.
## p-value = 0.202278

# 2. Verify if the RT_NoSwitch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_50004_NoSwitch.jpg")
qqnorm(RT_sub_50004_NoSwitch,main = "Normal Q-Q Plot (NO SWITCH)")  # quantile-quantile plot
qqline(RT_sub_50004_NoSwitch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot it seems to be Gaussian distributed

# SHAPIRO-WILK TEST 
if (shapiro.test(RT_sub_50004_NoSwitch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We can't reject H0: the variable is normally distributed.
## p-value = 0.08089552

##############################################################################################
### EXAMPLE4: Consider sub_50069 with schizophrenia and verify if it's Gaussian

RT_sub_50069_Switch <- as.numeric(sub_50069$ReactionTime[sub_50069$Switching=="SWITCH"])
RT_sub_50069_Switch <- as.numeric(na.omit(RT_sub_50069_Switch))
RT_sub_50069_NoSwitch <- as.numeric(sub_50069$ReactionTime[sub_50069$Switching=="NOSWITCH"])
RT_sub_50069_NoSwitch <- as.numeric(na.omit(RT_sub_50069_NoSwitch))

# 1. Verify if the RT_switch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_50069_Switch.jpg")
qqnorm(RT_sub_50069_Switch, main = "Normal Q-Q Plot (SWITCH)")  # quantile-quantile plot
qqline(RT_sub_50069_Switch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot it seems to be Gaussian 

# SHAPIRO-WILK TEST 
alpha <- 0.05
if (shapiro.test(RT_sub_50069_Switch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We can't reject H0: the variable is normally distributed.
## p-value = 0.7831439

# 2. Verify if the RT_NoSwitch has a Gaussian distribution
jpeg(filename = "../events recording/plots/qqplot_sub_50069_NoSwitch.jpg")
qqnorm(RT_sub_50069_NoSwitch,main = "Normal Q-Q Plot (NO SWITCH)")  # quantile-quantile plot
qqline(RT_sub_50069_NoSwitch, col='red') # theoretical line
dev.off()
#OBS: from the qqplot it seems not to be Gaussian distributed (confirmed by the Shapiro test)

# SHAPIRO-WILK TEST 
if (shapiro.test(RT_sub_50069_NoSwitch)$p.value < alpha){
  print("We reject H0:the variable is not normally distributed.")
}else{
  print("We can't reject H0: the variable is normally distributed.")
}
## => We reject H0:the variable is not normally distributed.
## p-value = 4.074574e-06