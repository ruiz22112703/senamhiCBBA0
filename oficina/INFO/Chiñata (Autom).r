library(tidyverse)
library(readxl)
############
{
  #setwd('/home/ruiz/senamhi/file15min/')
  est<-'Chiñata (Autom)'
  old<-getwd()
  setwd(old)
  setwd('C:/Campbellsci/LoggerNet')
  arc<-'Chiñata_EN_Min15.dat'#vientos
  #vientos
  ##########
  #k<-f$dir
  #i<-arc
  #i<-arc
  ##########
  for (i in arc) {
    print(i)
    a<-read.delim(i,sep = ',',header = T,skip = 3)
    if(any(duplicated(a$X)==T)){
      a<-a[-which(duplicated(a$X)==T),]
    }
    ind<-which(a[,1]=='TOA5'|a[,1]=='TIMESTAMP'|a[,1]=='TS'|a[,1]=='')
    if(length(ind)==0){
      b<-a %>% as_tibble()
    } else{
      b<-a[-ind,] %>% as_tibble()  
    }
    c<-b %>% separate(col=X,into = c('date','hour'),sep = ' ')
    c$date<-as.Date(c$date)
    #view(c)
    d<-c %>% mutate(date123=if_else(hour=='00:00:00',date-1,date))
    e<-d %>% separate(date123,into = c('y','m','d'),sep='-')
    e$Tot<-as.numeric(e$Tot);
    e$Smp<-as.numeric(e$Smp)
    #view(e)
    #write.csv(e,'test.csv')
    #id<-which(e$Max>27.7)
    #e[id,13]<-NA
    #e[id,14]<-NA
    f<-e %>% group_by(y,m,d) %>% summarise(prcp=sum(Tot,na.rm = T),
                                      max=max(Smp,na.rm = T),
                min=min(Smp,na.rm = T),mean=mean(Smp,na.rm = T),
                .groups = 'drop')
    #e1<-e[6677:6772,]
    #head(e1);tail(e1)
    #sum(e1$Tot)
    #view(e1)
    #view(f)
    #view(e)
    #f$dir123<-DIR(round(f$dir))
    #g<-f[,c(1:13,17,14:16)]
    h<-f %>% unite(date,y:d,sep = '-')
    setwd(old)
    out<-setwd('C:/Users/Cocha/Desktop/output-cbba/out')
    write.csv(h,gsub('$','.csv',est),row.names = F)
    setwd(old)
  }
setwd(old)
getwd()
}
########
#write.csv(h,'tex122.csv')
