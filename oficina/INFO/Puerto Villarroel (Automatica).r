library(tidyverse)
library(readxl)
############
{
  #setwd('/home/ruiz/senamhi/file15min/')
  est<-'Puerto Villarroel (Automatica)'
  old<-getwd()
  setwd(old)
  setwd('C:/Users/Cocha/Documents/ESTACIONES CBBA/SIVINGANI MISICUNI 15 min')
  arc<-'PuertoV'#vientos
  #vientos
  ##########
  #k<-f$dir
  #i<-arc
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
  #i<-arc
  ##########
  #dir()
  for (i in arc) {
    print(i)
    a <- read.csv("PuertoV", header=F)
    tibble(a)
    if(any(duplicated(a$X)==T)){
      a<-a[-which(duplicated(a$X)==T),]
    }
    ind<-which(a[,1]=='TOA5'|a[,1]=='TIMESTAMP'|a[,1]=='TS'|a[,1]=='')
    if(length(ind)==0){
      b<-a %>% as_tibble()
    } else{
      b<-a[-ind,] %>% as_tibble()  
    }
    c<-b %>% separate(col=V1,into = c('date','hour'),sep = ' ')
    c$date<-as.Date(c$date)
    d<-c %>% mutate(date123=if_else(hour=='00:00:00',date-1,date))
    e<-d %>% separate(date123,into = c('y','m','d'),sep='-')
    e$V2<-as.numeric(e$V2);e$V3<-as.numeric(e$V3);
    e$V4<-as.numeric(e$V4);e$V5<-as.numeric(e$V5);
    e$V8<-as.numeric(e$V8);e$V11<-as.numeric(e$V11);
    e$V12<-as.numeric(e$V12);
    #view(e)
    #write.csv(e,'test.csv')
    #id<-which(e$V13>27.7)
    #e[id,13]<-NA
    #e[id,14]<-NA
    f<-e %>% group_by(y,m,d) %>% summarise(prcp=sum(V3,na.rm = T),
                                           tx=max(V4,na.rm = T),
                tn=min(V4,na.rm = T),HRmean=mean(V5,na.rm = T),
                HRmax=max(V5,na.rm = T),HRmin=min(V5,na.rm = T),
                presMean=mean(V2,na.rm = T),presMax=max(V2,na.rm = T),
                presMin=min(V2.rm = T),dir=V11[which.max(V12)],
                vel=max(V12,na.rm = T)*3.6,radMean=mean(V8,na.rm = T),
                radMax=max(V8,na.rm = T),.groups = 'drop')
    #e1<-e[6677:6772,]
    #head(e1);tail(e1)
    #sum(e1$Tot)
    #view(e1)
    #view(f)
    #view(e)
    f$dir123<-DIR(round(f$dir))
    g<-f[,c(1:13,17,14:16)]
    h<-g %>% unite(date,y:d,sep = '-')
    setwd(old)
    out<-setwd('C:/Users/Cocha/Desktop/output-cbba/out')
    write.csv(h,gsub('$','.csv',est),row.names = F)
    setwd(old)
  }
setwd(old)
}########
#write.csv(h,'tex122.csv')

