
## IN QUESTO SCRIPT HO FATTO GRAFICI RIGUARDANTI UN SINGOLO INDIVIDUO
##
##

setwd("./materiale/events recording")

elenco <- read.delim("./elenco.txt", header=T)

s <- read.delim("./sub-10171_task-taskswitch_events.tsv")
s <- read.delim("./sub-50085_task-taskswitch_events.tsv")
s

s$duration =as.numeric(s$duration)
s$ReactionTime=as.numeric(s$ReactionTime)

{x11(width=400, height = 300)
plot( 1:length(s$ReactionTime), s$ReactionTime, type="l", lty=1, lwd=1.5)
}

      #PLOTTO I TEMPI DI REAZIONE IN BASE A SWITCH/NO SWITCH
{ x11()
  par(mfrow=c(1,2))
  minimo=min(s$ReactionTime, na.rm=T)
  massimo=max(s$ReactionTime, na.rm=T)
  boxplot(s$ReactionTime[s$Switching == "SWITCH"], xlab="switch", ylim=c(minimo,massimo))
  boxplot(s$ReactionTime[s$Switching != "SWITCH"], xlab="noswitch", ylim=c(minimo,massimo) )
}
dev.off()

{
  #divido per switch\no switch
  col_sw <- s$Switching
  col_sw[col_sw == "SWITCH"] <- "red"
  col_sw[col_sw == "NOSWITCH"] <- "blue"
  #colors
  
  #divido per correct response
  col_cr <- s$CorrectResp
  col_cr[col_cr == 1] <- "red"
  col_cr[col_cr == 0] <- "blue"
  
  #divido per congruency
  col_con <- s$Congruency
  col_con[col_con == "CONGRUENT"] <- "red"
  col_con[col_con == "INCONGRUENT"] <- "blue"
}

##
{ x11()
  par(mfrow=c(1,3))
  plot(s$duration, s$ReactionTime, col=col_sw)
  legend(0.5, 1.8, legend=c("SWTICH", "NOSWITCH"), title="Type",
         col=c("red", "blue"), lty=1, cex=0.6)
  
  plot(s$duration, s$ReactionTime, col=col_cr)
  legend(0.5, 1.8, legend=c("CORRECT", "NON CORRECT"), title="Response",
         col=c("red", "blue"), lty=1, cex=0.6)
  
  plot(s$duration, s$ReactionTime, col=col_con)
  legend(0.5, 1.8, legend=c("CONGRUENT", "INCONGRUENT"), title="Congruency",
         col=c("red", "blue"), lty=1, cex=0.6)
}

{ x11()
  par(mfrow=c(1,2))
  M=max(s$ReactionTime, na.rm = T)
  m=min(s$ReactionTime, na.rm = T)
  boxplot(s$ReactionTime[s$CorrectResp == 0], xlab="non correct", ylim=c(m,M), ylab="ReactionTime")
  boxplot(s$ReactionTime[s$CorrectResp != 0], xlab="correct", ylim=c(m,M), ylab="ReactionTime")
}

dev.off()



