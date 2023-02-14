
load("./materiale/workspaces/times.RData")
load("./materiale/workspaces/smoking.RData")

times = c(t_control,t_schz) 

times_ex
times_cur
times_not

tempi = c(times_ex, times_cur, times_not)
label_smoke = as.factor( c( rep("ex", length(times_ex)),rep("cur", length(times_cur)), rep("not", length(times_not))) )

fit = aov(tempi ~ label_smoke)
summary(fit)

library(ggplot2)

x=label_smoke  #etichette delle classe
y=tempi    #variabile da spiegare
{x11()
  p=ggplot(data.frame(Smoking.Habit=x,Reaction.Time=y),aes(x=Smoking.Habit,y=Reaction.Time))+
  geom_boxplot( aes(fill=x))+theme_minimal()+theme(legend.position = "")
  p+scale_fill_grey(start=0.3,end=0.8)
}

n       <- length(x)      # total number of obs.
ng      <- table(x)       # number of obs. in each group
treat   <- levels(x)      # levels of the treatment
g       <- length(treat)     # number of levels (i.e., of groups)

{x11()
  barplot(tapply(y, x, mean), names.arg=levels(x), ylim=c(0,max(y)),
          las=2, col=rainbow(g),main='Model under H1')
}

### verify the assumptions:
# 1) normality (univariate) in each group (6 tests)
Ps=c()
for ( ii in 1:g){
  Ps <- c(Ps, shapiro.test(y[ x==treat[ii] ])$p)
}
Ps

# 2) same covariance structure (= same sigma^2)
bartlett.test(y, x)

fit <- aov(y ~ x)
summary(fit)

#######################################
# Bonferroni CI for all the differences, confidence intervals?

ng=c(length(times_cur), length(times_ex), length(times_not))   # total number of obs.in the single group
      # total number of obs.
k <- g*(g-1)/2
alpha= 0.002

Mediag  <- tapply(y, x, mean)
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g)

treat = levels(label_smoke)
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }}

{
  x11(width = 14, height = 7)
  par(mfrow=c(1,2))
  plot(x, y, xlab='treat', ylab='y', col = rainbow(6), las=2)
  
  h <- 1
  plot(c(1,g*(g-1)/2),range(ICrange), pch='',xlab='pairs treat', ylab='Conf. Int. tau y')
  for(i in 1:(g-1)) {
    for(j in (i+1):g) {
      ind <- (i-1)*g-i*(i-1)/2+(j-i)
      lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
      points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
      points(h, ICrange[ind,1], col=rainbow(6)[j], pch=16); 
      points(h, ICrange[ind,2], col=rainbow(6)[i], pch=16); 
      h <- h+1
    }}
  abline(h=0)
}

{
  x11(width = 14, height = 7)
  par(mfrow=c(1,2))
  plot(x, y, xlab='treat', ylab='y', col = rainbow(6), las=2)
  
  df = data.frame( differences = c("cur-ex", "cur-not", "ex-not"),
                   Confidence.Interval = c(Mediag[1]-Mediag[2], Mediag[1]-Mediag[3], Mediag[2]-Mediag[3]),
                   upper = ICrange[,2],
                   lower = ICrange[,1]
  )
  
  p = ggplot( df, aes(differences,Confidence.Interval))
  p + geom_pointrange(aes(ymin = lower, ymax = upper))+ geom_errorbar(aes(ymin = lower, ymax = upper), width=0.3)+labs(x="")
  
  h <- 1
  plot(c(1,g*(g-1)/2),range(ICrange), pch='',xlab='pairs treat', ylab='Conf. Int. tau y')
  for(i in 1:(g-1)) {
    for(j in (i+1):g) {
      ind <- (i-1)*g-i*(i-1)/2+(j-i)
      lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
      points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
      points(h, ICrange[ind,1], col=rainbow(6)[j], pch=16); 
      points(h, ICrange[ind,2], col=rainbow(6)[i], pch=16); 
      h <- h+1
    }}
  abline(h=0)
}

