  library(tidyverse)
  library(readxl)
  est<-'Sacabamba.csv'
  setwd(old)
  setwd('C:/Users/Cocha/Documents/ESTACIONES CBBA/SIVINGANI MISICUNI 15 min')
  DIR=function(k){
    dimr<-length(k)
    k<-e$dir
    df<-data.frame(grados=c(0,15,35,55,75,105,125,145,165,195,215,235,255,285,305,
                            325,345),grados1=c(14,34,54,74,104,124,144,164,194,214,
                                               234,254,284,304,324,344,360))
    df$ptsCard<-c('N','NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W',
                  'WNW','NW','NNW','N')
    k<-as.integer(round(k,0))
    #class(k)
    ap<-c()
    j<-k[1]
    for (j in k) {
      #  print(j)
      for (i in 1:nrow(df)) {
        if(j%in%df[i,1]:df[i,2]==T){
          ap<-append(ap,df[i,3])
        }
      }
    }
    return(ap)  
  }
  a<-read.csv(est,sep = ';',check.names = F,fileEncoding = "Latin1")
  a<-a[-c(3,6,7,9)]
  head(a)
  tail(a)
  b<-a
  head(b)
  b[,3]<-gsub(',','.',a[,3])  
  b[,4]<-gsub(',','.',b[,4])  
  b[,5]<-gsub(',','.',b[,5])  
  b[,6]<-gsub(',','.',b[,6])  
  b[,7]<-gsub(',','.',b[,7])  
  b[,8]<-gsub(',','.',b[,8])  
  head(b)
  d<-b %>% separate(Date,into = c('d','m','y'),sep='/')
  d$y<-paste0('20',d$y)
  as_tibble(d)
  d1<-d %>% unite(date,y:d,sep = '-')
  as_tibble(d1)
  d1$date<-as.Date(d1$date)
  d2<-d1 %>% mutate(date123=if_else(Time=='0:00:00',date-1,date))
  #view(d2)
  d3<-d2 %>% separate(date123,into = c('y','m','d'),sep='-')
  tibble(d3)
  names(d3)<-c('date','time','rad','prcp','HR','temp','dir','vel','y','m','d')
  d3<-as.data.frame(d3)
  d3[,3]<-as.numeric(d3[,3])
  d3[,4]<-as.numeric(d3[,4])
  d3[,5]<-as.numeric(d3[,5])
  d3[,6]<-as.numeric(d3[,6])
  d3[,7]<-as.numeric(d3[,7])
  d3[,8]<-as.numeric(d3[,8])
  head(d3)
  #view(d3)
  #id<-which(d3$vel>100)
  #d3[1688,]
  #d3[id,7]<-NA
  #d3[id,8]<-NA
  e<-d3 %>% group_by(y,m,d) %>% summarise(prcp=sum(prcp,na.rm = T),
                                          tx=max(temp,na.rm = T),
                                          tn=min(temp,na.rm = T),HRmean=mean(HR,na.rm = T),
                                          HRmax=max(HR,na.rm = T),HRmin=min(HR,na.rm = T),
                                          dir=dir[which.max(vel)],
                                          vel=max(vel,na.rm = T),radMean=mean(rad,na.rm = T),
                                          radMax=max(rad,na.rm = T),.groups = 'drop')
  as_tibble(e)
  f<-e %>% unite(date,y:d,sep='-')
  f$DIR<-DIR(f$dir)
  g<-f[,c(1:8,12,9:11)]
  tail(g)
  setwd(old)
  setwd('C:/Users/Cocha/Desktop/output-cbba/out')
  write.csv(g,est,row.names = F)
  
