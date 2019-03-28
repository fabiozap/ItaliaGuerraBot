rm(list=ls())         # rimuovi tutto

#------------------- carica librerie ---------------------
library(Hmisc)
library(rgeos)
library(sp)
library(rgdal)

#------------------ carica dati --------------------------

# Inserire la cartella dove son presenti i dati    <<--

Cartella <- "C:/Users/fabio.lazzarato/Downloads/ItaliaGuerraBot/"   # <<--
setwd(Cartella)         # setta la mia cartella dove ho messo i poligoni
italia <- readOGR(dsn=path.expand("ProvCM01012019_g"))        # legge i poligoni
centr <- gCentroid(italia, byid = T, id = italia$SIGLA)       # trova il centroide di ogni poligono

#------------------creazione tabella master --------------
tab_centr_input <- data.frame(coordinates(centr), italia$SIGLA, italia$SIGLA)
names(tab_centr_input)[3] <- "prov"
names(tab_centr_input)[4] <- "poss"

#--------------------funzione distanza ------------------------------
distanza <- function(x1,x2,y1,y2){
  d <- sqrt((x2-x1)^2+(y2-y1)^2)
  return(d)
}

#-------------------tabella finale-----------------------
tabella_finale <- data.frame(prov_vincente=character(),numero_turni=integer())

#-------------------inizio delle run -----------------------------
numero_di_run <- 100
for(i in 1: numero_di_run){
  
  tab_centr<-tab_centr_input
  prov_unic <- 107
  turno <- 0
  
  #-----------------ciclo while fino a che ne rimane una--------------
  while(prov_unic != 1) {
    
    
    prov_att <- sample(tab_centr$poss,1)
    x1 <- tab_centr$x[tab_centr$prov==prov_att]
    y1 <- tab_centr$y[tab_centr$prov==prov_att]
    
    x2 <- tab_centr$x
    y2 <- tab_centr$y
    
    d <- distanza(x1,x2,y1,y2)
    tab_dist <- data.frame(tab_centr$prov,d,tab_centr$poss)
    d_min <- min(tab_dist$d[tab_centr$poss != prov_att])
    prov_vic <- tab_dist[tab_dist$d == d_min,"tab_centr.prov"]
    tab_centr$poss[tab_centr$prov==prov_vic] <- prov_att
    prov_unic <- length(unique(tab_centr$poss))
    turno <- turno +1
    
    # SCOMMENTA PER VEDERE L'AVANZAMENTO DELLA GUERRA

    # if (turno %% 20 == 0 ){
    #   plot(italia,col=tab_centr$poss)
    #   titolo <- paste(prov_att," >> ",prov_vic," turno:",turno," rimanenti:",prov_unic)
    #   title(titolo)
    #   Sys.sleep(0.1)
    # }
    
    
    # alla fine resetta per riportare tutto alla situazione iniziale
    if (prov_unic==1){
      tab_centr <- tab_centr_input
      turno_finale <- turno
      turno <- 0
    }
    

  }
  temp <- data.frame(prov_att,turno_finale)
  names(temp) <- c("prov_vincente","numero_turni")
  tabella_finale <- rbind(tabella_finale,temp)
}

