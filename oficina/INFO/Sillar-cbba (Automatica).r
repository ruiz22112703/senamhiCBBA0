  library(tidyverse)
  library(jsonlite)
  library(glue)
  # url
  # file
  #--- inicio de transferencia de datos del Sillar ----07
  date0<-as.Date('2024-01-10')
  date=seq(date0,today(),1)

  sillar <- fromJSON("http://200.87.128.217:8000/record_database.php?estacion=sillar&fecha=2024-01-12")

  for (i in 1:length(date)) {
    if(i==1){
      sillar <- fromJSON(glue("http://200.87.128.217:8000/record_database.php?estacion=sillar&fecha={date[i]}"))
    }else{
      sillar<-sillar %>% rbind(fromJSON(glue("http://200.87.128.217:8000/record_database.php?estacion=sillar&fecha={date[i]}")))
    }

  }
  sillar1<-sillar[,c(12,10,4,6)];sillar1 %>% tibble()
  sillar1[,2]<-as.numeric(sillar1[,2])
  sillar1[,3]<-as.numeric(sillar1[,3])
  sillar1[,4]<-as.numeric(sillar1[,4])
  sillar2<-sillar1 %>% separate(fecha,into=c('date','hour'),sep=' ') %>% 
    group_by(date) %>% 
    summarise(prcp=sum(precipitacion,na.rm = T),tx=max(temp_digital,na.rm = T),
              tn=min(temp_digital,na.rm = T),HRmean=mean(hum_dig,na.rm=T),
              HRmax=max(hum_dig),HRmin=min(hum_dig,na.rm=T))
  sillar2[,2]<-round(sillar2[,2],1)
  sillar2[,3]<-round(sillar2[,3],1)
  sillar2[,4]<-round(sillar2[,4],1)
  sillar2[,5]<-round(sillar2[,5],1)
  sillar2[,6]<-round(sillar2[,6],1)
  sillar2[,7]<-round(sillar2[,7],1)
  sillar2

  #cat('date\tprcp\ttx\ttn\tHRmean\tHRmax\tHRmin \n' ,file='Sillar-cbba (Automatica).csv')
  #cat(glue('{sillar2$date}\t{sillar2$prcp}\t{sillar2$tx}\t{sillar2$tn}\t{sillar2$HRmean}\t{sillar2$HRmax}\t{sillar2$HRmin}'),'\n',
    #    file='Sillar-cbba (Automatica).csv',append = T)
  write.csv(sillar2,'C:/Users/Cocha/Desktop/output-cbba/out/Sillar-cbba (Automatica).csv',row.names=F)
  print('Sillar-cbba (Automatica).csv')
