plot_temp <- function(sp,df,poss,prov,provincia_estratta, provincia_sconfitta,province_rimanenti,turno)
{
  plot(sp,col=df$poss)
  titolo <- paste(df$poss[df$prov==provincia_estratta]," >> ", provincia_sconfitta, " turno:", turno," rimanenti: ",province_rimanenti)
  title(titolo)
  Sys.sleep(0.1)
}

distanza <- function(x1,x2,y1,y2){
  d <- sqrt((x2-x1)^2+(y2-y1)^2)
  return(d)
}