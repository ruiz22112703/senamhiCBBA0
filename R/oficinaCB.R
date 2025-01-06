# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

{
  gc();gc();gc()
  load(url('https://github.com/ruiz22112703/senamhiCBBA0/raw/refs/heads/main/oficina/estacionesR.rda'))
  lis1<-lis[-which(lis=="Sillar-cbba (Automatica).r")]
  lis<-URLencode(lis,reserved = FALSE)
  lis1<-URLencode(lis1,reserved = FALSE)
  if(aaa==0){
    for (i in 1:length(lis)) {
      tryCatch({
        source(glue("https://github.com/ruiz22112703/senamhiCBBA0/raw/refs/heads/main/oficina/INFO/{lis[i]}"))
      }, error=function(e){
        print(paste("Error en esta estación.....!!!"))
      })
    }
    cat('Se ha actualizados datos de las estaciones de Cochabamba \n')
    cat("Gracias por el Servicio...!!! \n")
    cat('\n')

  }else if(aaa==1){
    for (j in 1:length(lis1)) {
      tryCatch({
        source(glue("https://github.com/ruiz22112703/senamhiCBBA0/raw/refs/heads/main/oficina/INFO/{lis1[j]}"))
      }, error=function(e){
        print(paste("Error en esta estación.....!!!"))
      })
    }
    cat('Se ha actualizados datos de las estaciones de Cochabamba \n')
    cat("Gracias por el Servicio...!!!")
  }else{
    cat('Por favor digite un numero entre "0" y "1" \n')
    cat("\n")
  }
  #msg
  {
    dir=dir('C:/Users/Cocha/Desktop/output-cbba/out')
    dir0<-list.files('C:/Users/Cocha/Desktop/output-cbba/out/',pattern = '.csv',full.names = F)
    dir1<-list.files('C:/Users/Cocha/Desktop/output-cbba/out/',pattern = '.csv',full.names = T)
    getwd()
    est_send<-c('Aguadas Misicuni.csv','Aiquile Automática.csv','Alalay (Automatica).csv','Calientes Automática.csv',
                'Chimboco (Aut).csv','Cuatro Esquinas Misicuni.csv','INAC Aeropuerto Cbba.csv',
                'Lahuachama Presa (Aut.).csv','Mizque (Automática).csv','Parque Tunari SDC (Autom).csv',
                'Pojo (Automática).csv','Presa Misicuni Aut..csv','Puerto Villarroel (Automatica).csv','Pocona (Automatica).csv','Linkupata (Automatica).csv',
                'Pasorapa (Automatica).csv','Sipe Sipe (Automática).csv','Sacabamba.csv','Tiraque (Automatica).csv','Vacas (Automática).csv',
                'Viloma (Automatica).csv','Vila Vila (Automatica).csv','Templo Misicuni.csv','Sillar-cbba (Automatica).csv','Omereque (Automática).csv')
    est_send1<-dir0[dir0%in%est_send]
    cat('\n',file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    cat(paste("*SERIVICIO NACIONAL DE METEOROLOGIA E HIDROLOGIA*",'\n'),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    cat(paste("___________*SENAMHI-CBBA*",'\n','\n'),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    cat(paste("Monitoreo Meteorologico de Cochabamba categorizadas por regiones. ",'\n','\n','\n'),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    setwd('C:/Users/Cocha/Desktop/output-cbba/out')


    #i=est_send1[15]
    lis<-list()
    id_est<-c()
    for(i in est_send1){
      a<-read.csv(i)
      dayaf<-bbb
      #print(i)
      if(a[nrow(a),1]==(Sys.Date())|(a[nrow(a),1]==(Sys.Date()-1))){

        b<-a[(nrow(a)-dayaf+1):nrow(a),1:4]
        lis<-append(lis,list(b))
        id_est<-append(id_est,i)
        #b[,2]<-sprintf('%0.1f',round(b[,2],1))
        #b[,3]<-sprintf('%0.1f',round(b[,3],1))
        #b[,4]<-sprintf('%0.1f',round(b[,4],1))
        #cat(glue("----- *{gsub('.csv','',i)}* ----- \n \n"),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
        #cat('dia----->    \t lluvia \t tmax \t tmin \n',file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt',append = T)
        #cat(glue("{b[1:dayaf,1]}\t {b[1:dayaf,2]}\t         {b[1:dayaf,3]}\t  {b[1:dayaf,4]} \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
        #cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
        #cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
      }
      #if(a[nrow(a),1]==(Sys.Date())){
      #  cat(glue("{b[2,1]}\t {b[2,2]}\t         {b[2,3]}\t  {b[2,4]} \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
      #  cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
      #else{
      #  cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
      #cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
      #}

    }

    metropolitana<-c("Chimboco (Aut)", "INAC Aeropuerto Cbba" ,"Parque Tunari SDC (Autom)","Sipe Sipe (Automática)","Viloma (Automatica)", 'Cochabamba Aeropuerto')
    valles<-c("Sacabamba",'Anzaldo_M(GPRS)','Sichez_H(GPRS)','Villa Rivero_M (GPRS)','Lahuachama Presa (Aut.)','Linkupata (Automatica)')
    tropico<-c('Bulo Bulo_M (GPRS)','Villa Tunari_H (GPRS)','Sillar-cbba (Automatica)','Chimore Aeropuerto')
    ConoSur<-c('Aiquile Automática','Tiraque (Automatica)','Vacas (Automática)','Vila Vila (Automatica)','Pasorapa (Automatica)','Pojo (Automática)','Alalay (Automatica)','Mizque (Automática)','Pocona (Automatica)','Omereque (Automática)')
    altiplano<-c('Aguadas Misicuni','Calientes Automática','Cuatro Esquinas Misicuni','Presa Misicuni Aut.','Templo Misicuni')

    region_1<-list(metropolitana,valles,tropico,ConoSur,altiplano)
    region_2<-list('REGION METROPOLITANA','REGION DE LOS VALLES','REGION DEL TROPICO','REGION DEL CONO SUR','REGION ANDINA')

    for(k in 1:length(region_2)){
      cat(glue("----- *{region_2[[k]]}* ----- \n \n"),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
      cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
      for(j in which(gsub('.csv','',id_est)%in%region_1[[k]])) {
        if(length(which(gsub('.csv','',id_est)%in%region_1[[k]]))!=0){
          lis[[j]][,2]<-sprintf('%0.1f',round(lis[[j]][,2],1))
          lis[[j]][,3]<-sprintf('%0.1f',round(lis[[j]][,3],1))
          lis[[j]][,4]<-sprintf('%0.1f',round(lis[[j]][,4],1))

          cat(glue("----- *{gsub('.csv','',id_est[j])}* ----- \n \n"),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
          cat('dia----->    \t lluvia \t tmax \t tmin \n',file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt',append = T)
          cat(glue("{lis[[j]][1:dayaf,1]}\t {lis[[j]][1:dayaf,2]}\t         {lis[[j]][1:dayaf,3]}\t  {lis[[j]][1:dayaf,4]} \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
          cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
        }else{
          cat(paste('No tenemos Datos por el momento en este Piso Agro-Ecologico'),'\n \n',append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
        }

      }
    }

    cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    cat(paste('Datos Actualizados  para la fecha y hora:',as.character(now())),'\n \n',append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    cat(paste('Monitor de Lluvias y Temperaturas de SENAMHI-CBBA, Bolivia'),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    cat(glue(" \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
    cat(glue("--------------------------------------------------------------------------------- \n \n "),append = T,file = 'C:/Users/Cocha/Desktop/output-cbba/texto1.txt')
  }
  #
  rm(list = ls());
  gc();gc();gc()
}
