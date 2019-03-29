plot_temp <- function(sp,
                      df,
                      poss,
                      prov,
                      provincia_estratta, 
                      provincia_sconfitta,
                      province_rimanenti,
                      turno)
{
  plot(sp,col=df$poss)
  titolo <- paste(df$poss[df$prov==provincia_estratta]," >> ", provincia_sconfitta, " turno:", turno," rimanenti: ",province_rimanenti)
  title(titolo)
  Sys.sleep(0.1)
}

distanza <- function(x1,
                     x2,
                     y1,
                     y2){
  d <- sqrt((x2-x1)^2+(y2-y1)^2)
  return(d)
}

plot_run <- function(sp,df,df_originale){
  numero_vittorie <- table(unlist(df$prov_vincente))
  max_numero_vittorie <- max(numero_vittorie)
  df_finale <- data.frame(numero_vittorie)
  names(df_finale) <- c("prov","freq")
  merge <- merge(df_originale,df_finale,by="prov", sort=F)
  rbPal <- colorRampPalette(c('blue','red'))
  colori <- rbPal(max_numero_vittorie)[as.numeric(cut(merge$freq,seq(0,max_numero_vittorie,by=1)))]
  plot(sp, col=colori)
}