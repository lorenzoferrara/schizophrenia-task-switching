
setwd("C:/Users/lofer/OneDrive/Documenti/GitHub/Brain-Connectivity")
setwd("./materiale")
load("./workspaces/rec_2.RData")

elenco_copia <- read.delim("./events recording/elenco_copia.txt", header=T)
elenco <- read.delim("./events recording/elenco.txt", header=T)
p <- read.delim("./data/participants.csv")

lista_colori = c( rep("blue", 125), c(rep("red", 50)))

library(ggplot2)

{x11()
pp=ggplot(data.frame(Diagnosis=p$diagnosis, Age=p$age),aes(x=Diagnosis,y=Age))+
  geom_boxplot( aes(fill=Diagnosis),width=0.5)+
  scale_fill_manual(values = c("royalblue","orange2" ))+
  theme_minimal()+theme(legend.position = "")
pp
}

#1)____________________________________________
colors=c(rep("blue", 125), rep("red", 50))
times= c(t_sani, t_schz)
p$diagnosis=as.factor(p$diagnosis)

fit.age=lm(times ~ p$age * p$diagnosis)
summary(fit.age)

fit.age=lm(times ~ p$age + p$diagnosis)
summary(fit.age)
coef.age=fit.age$coefficients

{
  x11()
  pp=NULL
  # pp  = ggplot( )+
  #   geom_smooth(aes(Age,Reaction.Time),data=data.frame(Age=p$age[1:125],Reaction.Time=times[1:125]),method ="lm",se=F,color="royalblue")+
  #   geom_smooth(aes(Age1,Reaction.Time1),data=data.frame(Age1=p$age[126:175],Reaction.Time1=times[126:175]),method ="lm",se=F,color="orange2")+
  #   geom_point(aes(Age,Reaction.Time,color="Control"),data=data.frame(Age=p$age[1:125],Reaction.Time=times[1:125])) + 
  #   geom_point(aes(Age1,Reaction.Time1,color="Schz"),data=data.frame(Age1=p$age[126:175],Reaction.Time1=times[126:175]),color="orange2")+
  #   scale_colour_manual(values=c( "Control" = "royalblue", "Schz" = "orange2"), name = "Diagnosis")
  pp  = ggplot(data=data.frame(Age=p$age, Reaction.Time=times, diag=p$diagnosis),aes(Age,Reaction.Time))+
    geom_point(aes(colour=p$diagnosis))+            
    scale_colour_manual(values=c( "CONTROL" = "royalblue", "SCHZ" = "orange2"), name = "Diagnosis")+
    geom_abline(aes(intercept=coef.age[1],slope=coef.age[2],color="CONTROL"))+
    geom_abline(aes(intercept=coef.age[1]+coef.age[3],slope=coef.age[2],color="SCHZ"))+
    theme( axis.title = element_text(size=14, face = "bold"), legend.position = "", 
           legend.text = element_text(size=11))
  pp
}

dev.off()

#2)____________________________________________

phenotype.health <- read.delim("./covariates/phenotype health.tsv", header=T, sep=" ")

bmi=as.numeric(phenotype.health$bmi)
bmi



fit.bmi=lm(times ~ bmi * p$diagnosis)
summary(fit.bmi)

fit.bmi=lm(times ~ bmi + p$diagnosis)
summary(fit.bmi)
coef.bmi=fit.bmi$coefficients

{
  x11()
  pp=NULL
  # pp  = ggplot()+
  #   geom_smooth(aes(bmi,Reaction.Time),data=data.frame(bmi=bmi[1:125],Reaction.Time=times[1:125]),method ="lm",se=F,color="royalblue")+
  #   geom_smooth(aes(bmi1,Reaction.Time1),data=data.frame(bmi1=bmi[126:175],Reaction.Time1=times[126:175]),method ="lm",se=F,color="orange2")+
  #   geom_point(aes(bmi,Reaction.Time,color="Control"),data=data.frame(bmi=bmi[1:125],Reaction.Time=times[1:125])) + 
  #   geom_point(aes(bmi1,Reaction.Time1,color="Schz"),data=data.frame(bmi1=bmi[126:175],Reaction.Time1=times[126:175]),color="orange2")+
  #   scale_colour_manual(values=c( "Control" = "royalblue", "Schz" = "orange2"), name = "Diagnosis")
  pp  = ggplot(data=data.frame(bmi=bmi, Reaction.Time=times, diag=p$diagnosis),aes(bmi,Reaction.Time))+
    geom_point(aes(colour=p$diagnosis))+            
    scale_colour_manual(values=c( "CONTROL" = "royalblue", "SCHZ" = "orange2"), name = "Diagnosis")+
    geom_abline(aes(intercept=coef.bmi[1],slope=coef.bmi[2],color="CONTROL"))+
    geom_abline(aes(intercept=coef.bmi[1]+coef.bmi[3],slope=coef.bmi[2],color="SCHZ"))+
    theme( axis.title = element_text(size=14, face = "bold"), legend.position = "", 
           legend.text = element_text(size=11))
  
  pp
}


#1)____________________________________________

fit.doppia=lm(times ~ bmi * p$diagnosis * p$age)
summary(fit.doppia)

library(MASS)
step.model= stepAIC(fit.doppia, direction="both", trace=F)
summary(step.model)
coef.doppia = step.model$coefficients



library(plotly)
x_grid <- seq(from = min(bmi,na.rm=T), to = max(bmi,na.rm=T), length = 100)
y_grid <- seq(from = min(p$age,na.rm=T), to = max(p$age,na.rm=T), length = 100)
z_control <- t(outer(x_grid, y_grid, function(x,y) coef.doppia[1]+coef.doppia[2]*x+coef.doppia[4]*y))
z_schz <- t(outer(x_grid, y_grid, function(x,y) coef.doppia[1]+coef.doppia[3]+coef.doppia[2]*x+coef.doppia[4]*y))

{x11()
plot_ly(x=bmi, y=p$age , z=times, type="scatter3d", mode="markers", color=I(c(rep("royalblue",125),rep("orange2",50))))%>%
add_surface(
  x = x_grid,
  y = y_grid,
  z = z_control,
  colorscale = list(c(0,1),c("royalblue","royalblue")),
  showscale=F,
  showlegend=F,
  opacity=0.8
) %>%
  add_surface(
    x = x_grid,
    y = y_grid,
    z = z_schz,
    colorscale = list(c(0,1),c("#de9000","#de9000")),
    showscale=F,
    showlegend=F,
    opacity=0.8
  ) %>%
  # Axes labels and title:
  layout(
     scene = list(
      zaxis = list(title = "Reaction.Time"),
      yaxis = list(title = "Age"),
      xaxis = list(title = "BMI"))) %>%
        layout(
          xaxis = list( titlefont = list(siz=22),  tickfont = list(siz=22)),
          yaxis = list( titlefont = list(siz=22),  tickfont = list(siz=22)),
          zaxis = list( titlefont = list(siz=22),  tickfont = list(siz=22))
        )
}

graphics.off()

