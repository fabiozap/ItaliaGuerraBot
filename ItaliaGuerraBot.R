rm(list=ls())         # rimuovi tutto

#------------------- carica librerie ---------------------
library(Hmisc)
library(rgeos)
library(sp)
library(rgdal)

#------------------ carica dati --------------------------



# Inserire la cartella dove son presenti i dati    <<--

Cartella <- "C:/Users/fabio.lazzarato/Documents/ItaliaGuerraBot/"   # <<--
setwd(Cartella);         # setta la mia cartella dove ho messo i poligoni

source("funzioni.R")

italia <- readOGR(dsn=path.expand("ProvCM01012019_g"))        # legge i poligoni
centr <- gCentroid(italia, byid = T, id = italia$SIGLA)       # trova il centroide di ogni poligono

#------------------creazione tabella master --------------
tab_centr_input <- data.frame(coordinates(centr), italia$SIGLA, italia$SIGLA);
names(tab_centr_input)[3] <- "prov"
names(tab_centr_input)[4] <- "poss"

#-------------------tabella finale-----------------------
tabella_finale <- data.frame(prov_vincente=character(),numero_turni=integer())

#-------------------inizio delle run -----------------------------
numero_di_run <- 10000

 for(i in 1: numero_di_run){
  
  tab_centr<-tab_centr_input
  prov_unic <- 107
  turno <- 0
  
  #-----------------ciclo while fino a che ne rimane una--------------
  while(prov_unic != 1) {
    
    
    # prov_att: dall'elenco dei possessori viene estratta la provincia attaccante
    
    #prov_att <- sample(tab_centr$poss,1)
    prov_att <- sample(tab_centr$prov,1)
    x1 <- tab_centr$x[tab_centr$prov==prov_att]
    y1 <- tab_centr$y[tab_centr$prov==prov_att]
    
    x2 <- tab_centr$x
    y2 <- tab_centr$y
    
    d <- distanza(x1,x2,y1,y2);
    tab_dist <- data.frame(tab_centr$prov,d,tab_centr$poss)
    d_min <- min(tab_dist$d[tab_centr$poss != tab_centr$poss[tab_centr$prov==prov_att]])
    prov_vic <- tab_dist[tab_dist$d == d_min,"tab_centr.prov"]
    tab_centr$poss[tab_centr$prov==prov_vic] <- tab_centr$poss[tab_centr$prov==prov_att]
    prov_unic <- length(unique(tab_centr$poss))
    turno <- turno +1
    
    

    
    # if (turno %% 20 == 0 ){
    #   plot_temp(sp                     = italia,
    #             df                     = tab_centr,
    #             poss                   = poss,
    #             prov                   = prov,
    #             provincia_estratta     = prov_att,
    #             provincia_sconfitta    = prov_vic,
    #             province_rimanenti     = prov_unic,
    #             turno                  = turno)
    # }
    
    
    # alla fine resetta per riportare tutto alla situazione iniziale
    if (prov_unic==1){
      situazione_finale <- tab_centr;
      tab_centr <- tab_centr_input;
      turno_finale <- turno;
      turno <- 0;
    };
    
  }
  to_be_print <- paste("Run: ",i," Vincitore: ",situazione_finale$poss[situazione_finale$prov==prov_att],", # turni: ",turno_finale)
  print(to_be_print)
  temp <- data.frame(situazione_finale$poss[situazione_finale$prov==prov_att],turno_finale)
  names(temp) <- c("prov_vincente","numero_turni")
  tabella_finale <- rbind(tabella_finale,temp)
  
  nome_png <- paste("imm-",i,".png")
  
  
  # plot_run(sp                    = italia,
  #          df                    = tabella_finale,
  #          df_originale          = tab_centr)
  
 }

