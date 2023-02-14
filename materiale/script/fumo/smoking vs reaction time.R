
load("./materiale/workspaces/times.RData")
load("./materiale/workspaces/smoking.RData")

times = c(t_control,t_schz) 

colors=c(rep("blue",125),rep("red",50))

indexes_ex = as.numeric(row.names(ex_smokers))
indexes_cur = as.numeric(row.names(current_smokers))
indexes_not = as.numeric(row.names(not_smokers))
times_ex = times[indexes_ex]
times_cur = times[indexes_cur]
times_not = times[indexes_not]

{
  # x11()
  par(mfrow=c(1,3))
  boxplot(times_cur, xlab="Current smokers")
  boxplot(times_ex,xlab="Ex smokers")
  boxplot(times_not, xlab="Not smokers")
}

univariate.test(times_ex, times_not)

univariate.test(times_cur, times_not)

# RISULTATI: il tempo medio di ex smokers e non smoker ? lo stesso
# mentre il tempo medio di current smokers e non smokers ? diverso

#########
#lo rifaccio unendo ex_smokers e non smokers

times_noncur = c(times_ex, times_not)

{
  # x11()
  par(mfrow=c(1,2))
  boxplot(times_cur, xlab="Current smokers")
  boxplot(times_noncur, xlab="Not current smokers")
}

univariate.test(times_cur, times_noncur, CI = T)

#PERFETTO, SONO DIVERSI


#################
#GIA CHE CI SONO VEDO COME SI DISTRIBUISCNO I FUMATORI TRA SHCIZO E control

current_smoking_bool = rep(0,175)
for(i in as.numeric(row.names(current_smokers))){
  current_smoking_bool[i]=1
}
sum(current_smoking_bool[1:125])/125  #percentuale di fumatori tra i control
sum(current_smoking_bool[126:175])/50 #percentuale di fumatori tra gli schz

sum(indexes_cur<=125)/125
#The 8.8% of the control group is currently smoking

sum(indexes_cur>125)/50
#The 40% of the schizophrenic group is currently smoking

# CONCLUSIVE PLOT
data=cbind(c(sum(current_smoking_bool[1:125]), sum(current_smoking_bool[126:175])), c(125-sum(current_smoking_bool[1:125]), 50-sum(current_smoking_bool[126:175]))) #percentuale di fumatori tra gli schz)
data=as.table(t(data))
colnames(data)=c("Control", "Schz")
rownames(data)=c("Current_smoker", "Non_current_smoker")
{
  # x11()
  barplot(data, legend=T, beside=T, main='Smoking habits')
}
