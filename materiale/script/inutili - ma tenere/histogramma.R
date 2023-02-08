crea.hist <- function(vettore) {
  num=13
  M=max(vettore)
  a <- seq(0, M, M/(num))
  table = rep(0,num)
  
  for (j in 1:length(vettore)){
    for (i in 1:num){
      if(vettore[j] > a[i] & vettore[j] <= a[i+1]){
        table[i] = table[i] + 1
      }
    }
  }
  table=table/length(vettore)
  
  nomi=round(a[1:num], digits=2)
  
  X11()
  barplot(table, names.arg = nomi, ylim = c(0, max(table)+0.05), ylab="Density")
  
  return(table)
}

save(crea.hist, file="G:/Il mio Drive/Brain Connectivity/materiale/crea.hist.RData")
