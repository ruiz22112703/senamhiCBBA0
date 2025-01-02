library(tidyverse)
time<-seq(1,23,1)
now()
repeat{
  tryCatch({
    if((hour(now())%in%time)&minute(now())<5){
	cat('minuto_0:',minute(now()),'\n')
      {
    source("C:/Users/Cocha/Desktop/INFO/aeropuerto P. Cajon (Aut).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Aguadas Misicuni.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Aiquile Automática.r", encoding = 'UTF-8')
    #source("C:/Users/Cocha/Desktop/INFO/Alalay (Automatica).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Calientes Automática.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Chiaraje Automática.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Chimboco (Aut).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Chiñata (Autom).r")
    source("C:/Users/Cocha/Desktop/INFO/Cuatro Esquinas Misicuni.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/El Abra (Autom).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/INAC Aeropuerto Cbba.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Lahuachama Presa (Aut.).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Linkupata (Automatica).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Misicuni (Automatica).r", encoding = 'UTF-8')#fdfdfd
    source("C:/Users/Cocha/Desktop/INFO/Mizque (Automática).r")
    source("C:/Users/Cocha/Desktop/INFO/Omereque (Automática).r")
    source("C:/Users/Cocha/Desktop/INFO/Parque Tunari SDC (Autom).r", encoding = 'UTF-8')
    #source("C:/Users/Cocha/Desktop/INFO/Pasorapa (Automatica).r", encoding = 'UTF-8')  
    source("C:/Users/Cocha/Desktop/INFO/Pico de Loro (Autom).r", encoding = 'UTF-8')
    #source("C:/Users/Cocha/Desktop/INFO/PUENTE POJO 2022/15min.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Pocona (Automatica).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Pojo (Automática).r")
    source("C:/Users/Cocha/Desktop/INFO/Presa Misicuni Aut.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Presa Totora Qhocha Aut.r", encoding = 'UTF-8')  
    source("C:/Users/Cocha/Desktop/INFO/Puente Décima Aut. Hidr.r")
    #source("C:/Users/Cocha/Desktop/INFO/Puente Pojo (Autom.)/15min.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Puente Siles (Hidro).r")
    source("C:/Users/Cocha/Desktop/INFO/Puerto Villarroel (Automatica).r", encoding = 'UTF-8')
    #source("C:/Users/Cocha/Desktop/INFO/Sacabamba.r", encoding = 'UTF-8')
    #source("C:/Users/Cocha/Desktop/INFO/San Benito (Autom)/15min.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Sauce Pilapata (Autom.).r", encoding = 'UTF-8')
    #source("C:/Users/Cocha/Desktop/INFO/Senda VI (Automatica)/15min.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Sipe Sipe (Automática).r")
    source("C:/Users/Cocha/Desktop/INFO/Sunjani Misicuni.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Tapacari (Automática).r")
    source("C:/Users/Cocha/Desktop/INFO/Templo Misicuni.r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Tiraque (Automatica).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Vacas (Automática).r")
    source("C:/Users/Cocha/Desktop/INFO/Vila Vila (Automatica).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Viloma (Automatica).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/INFO/Sillar-cbba (Automatica).r", encoding = 'UTF-8')
    source("C:/Users/Cocha/Desktop/Nihel/msg.r", encoding = 'UTF-8')
    }
    cat('Se actualizo los datos de las estaciones Automaticas de CBBA ........!!!','\n')
    cat('Hora:',hour(now()),'\n')
    cat('Minuto:',minute(now()),'\n')
  }
  }, error=function(e){
    print(paste("Error en esta estación.....!!!"))
  })

  
  # second(Sys.time())%in%time
  # time%in%minute(Sys.time())
}
