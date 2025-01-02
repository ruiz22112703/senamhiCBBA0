library(tidyverse)
library(readxl)
#setwd('C:/Users/Cocha/Desktop/Nihel/')
est<-'Misicuni (Automatica)'
old<-getwd()
setwd(old)
setwd('C:/Users/Cocha/Documents/ESTACIONES CBBA/SIVINGANI MISICUNI 15 min')
dir()
#old<-getwd()
arch<-dir()[which(substr(dir(),1,9)=='EMisicuni')]
j<-1
#i<-arch[1]
date<-c()
DIR=function(k){
  dimr<-length(k)
  df<-data.frame(grados=c(0,15,35,55,75,105,125,145,165,195,215,235,255,285,305,
                          325,345),grados1=c(14,34,54,74,104,124,144,164,194,214,
                                             234,254,284,304,324,344,360))
  df$ptsCard<-c('N','NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W',
                'WNW','NW','NNW','N')
  k<-as.integer(round(f$dir,0))
  class(k)
  ap<-c()
  #j<-k[1]
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
date
#i<-arch[9]
print(est)
i<-arch[1]
j<-1
for(i in arch){
  date<-append(date,as.Date(substr(i,11,20))-1)
  a<-read.table(i,sep = ',',header = T,skip = 3)
  sum(a$Tot)
  b<-a %>% summarise(prcp=sum(Tot,na.rm = T),tx=max(Smp.1,na.rm = T),
                     tn=min(Smp.1,na.rm = T),HRmean=mean(Smp.2,na.rm = T),
                     HRmax=max(Smp.2,na.rm = T),HRmin=min(Smp.2,na.rm = T),
                     presionMean=mean(Smp,na.rm = T),presionMax=max(Smp,na.rm = T),
                     presionMin=min(Smp,na.rm = T),dir=Smp.5[which.max(Max)],Na=NA,
                     vel=max(Max,na.rm = T)*3.6,radMean=mean(Smp.3,na.rm=T),
                     radMax=max(Smp.3,na.rm=T))
  if(j==1){
    c<-b
  }else{
    c[nrow(c)+1,]<-b  
  }
  j<-j+1
}
d<-c %>% mutate(date=date)
f<-d
f$dir123<-DIR(round(f$dir))
e<-f[,c(15,1:10,16,12:14)]
out<-setwd('C:/Users/Cocha/Desktop/output-cbba/out')
write.csv(e,gsub('$','.csv',est),row.names = F)

