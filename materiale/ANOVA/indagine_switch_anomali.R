## indagine
v<-NULL
for (ii in 1:175)
{ 
  if (numero_switch[ii]==60) # da esecuzione di anova_generale
    v<-append(v,BigList[ii])}
v


ii=0
switches<-rep(0,length(v))
da_leggere$TIPO="Colore"

for (individui in v){
  ii=ii+1
  da_leggere<-read.table(individui)
  
  for (jj in 1:95)
  {
    if (da_leggere$ColorShapeProbe[jj]=="C" || da_leggere$ColorShapeProbe[jj]=="COLOR")
      {da_leggere$TIPO[jj]<-"Colore"}
    else 
     { da_leggere$TIPO[jj] <- "Forma"}
  }
  
  for (jj in 1:95)
  {
    if  (da_leggere$TIPO[jj]!=da_leggere$TIPO[jj+1])
    switches[ii]=switches[ii]+1
  }
}
  
