
setwd('G:/Il mio Drive/Brain Connectivity/')

###

setwd("./materiale")

library(rpart)
library(rpart.plot)
load("./workspaces/rec_2.RData")
load("./workspaces/zmap_creazione.RData")
p <- read.delim("./data/participants.csv")
diagnosis <- factor(p$diagnosis)

set.seed(110)
order = sample(1:length(diagnosis))
diagnosis_shuffled = diagnosis[order]
scores_shuffled = scores[order,] 
training_set = 1:round(175/10*9)
test_set = (round(175/10*9)+1):175

test = data.frame( "diagnosis" = diagnosis_shuffled[test_set], 
                      scores_shuffled[test_set,1:3])
train = data.frame( "diagnosis" = diagnosis_shuffled[training_set], 
                    scores_shuffled[training_set,1:3])

fit = rpart( diagnosis ~ Comp.1 + Comp.2 + Comp.3, data=train,  method = "class")

printcp(fit)
plotcp(fit)
summary(fit)
{x11()
#plot(fit, uniform = T)
prp(fit)
}
###

PredictCART_train = predict(fit, newdata = train, type = "class")
t = table( train$diagnosis, PredictCART_train)
(t[1,1] + t[2,2] ) / sum(t) #77% di accuratezza

PredictCART_test = predict(fit, newdata = test, type = "class")
t_test = table( test$diagnosis, PredictCART_test)
(t_test[1,1] + t_test[2,2] ) / sum(t_test) #70% di accuratezza

######

pfit = prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"] * 4/5)
prp(pfit)
plot(pfit, uniform = T, main = "Pruned Classificatio Tree")

PredictCART_train = predict(pfit, newdata = train, type = "class")
t = table( train$diagnosis, PredictCART_train)
(t[1,1] + t[2,2] ) / sum(t) #77% di accuratezza

PredictCART_test = predict(pfit, newdata = test, type = "class")
t_test = table( test$diagnosis, PredictCART_test)
(t_test[1,1] + t_test[2,2] ) / sum(t_test) #70% di accuratezza
