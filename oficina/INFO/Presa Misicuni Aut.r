library(tidyverse)
library(readxl)
############
{
  #setwd('/home/ruiz/senamhi/file15min/')
  est<-'Presa Misicuni Aut.'
  old<-getwd()
  setwd(old)
  setwd('C:/Campbellsci/LoggerNet')
  arc<-'PresaMisicuniEI_EM_Min5.dat'#vientos
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
  for (i in arc) {
    print(i)
    a<-read.delim(i,sep = ',',header = T,skip = 3)
    if(any(duplicated(a$X)==T)){
      a<-a[-which(duplicated(a$X)==T),]
    }
    tibble(a)
    ind<-which(a[,1]=='TOA5'|a[,1]=='TIMESTAMP'|a[,1]=='TS'|a[,1]=='')
    if(length(ind)==0){
      b<-a %>% as_tibble()
    } else{
      b<-a[-ind,] %>% as_tibble()  
    }
    c<-b %>% separate(col=X,into = c('date','hour'),sep = ' ')
    c$date<-as.Date(c$date)
    d<-c %>% mutate(date123=if_else(hour=='00:00:00',date-1,date))
    e<-d %>% separate(date123,into = c('y','m','d'),sep='-')
    e$Smp.2<-as.numeric(e$Smp.2);e$Tot<-as.numeric(e$Tot);
    e$Smp.4<-as.numeric(e$Smp.4);e$Smp.4<-as.numeric(e$Smp.4);
    e$Smp.5<-as.numeric(e$Smp.5);e$Smp.7<-as.numeric(e$Smp.7);
    e$Smp.8<-as.numeric(e$Smp.8);e$Smp.3<-as.numeric(e$Smp.3)
    #view(e)
    #write.csv(e,'test.csv')
    id<-which(e$Smp.7>27.7)
    e[id,12]<-NA
    e[id,13]<-NA
    #view(e)
    e<-e%>%mutate(Stmin=ifelse(Smp.2<10&Smp.2>-50,13.1267+0.6215*Smp.2-11.37*(Smp.7*3.6)**(0.16)+0.3965*Smp.2*(Smp.7*3.6)**(0.16),Smp.2))
    e<-e%>%mutate(Stmax=ifelse(Smp.2>=26&Smp.3>=40,-8.78469476+1.61139411*Smp.2+2.338548839*Smp.7-0.14611605*Smp.2*Smp.7-0.012308094*Smp.2**2-0.016424828*Smp.7**2+0.002211732*Smp.2**2*Smp.7+0.00072546*Smp.2*Smp.7**2-0.000003582*Smp.2**2*Smp.7**2,Smp.2))
    f<-e %>% group_by(y,m,d) %>% summarise(prcp=sum(Tot,na.rm = T),
                                           tx=max(Smp.2,na.rm = T),
                tn=min(Smp.2,na.rm = T),HRmean=mean(Smp.3,na.rm = T),
                HRmax=max(Smp.3,na.rm = T),HRmin=min(Smp.3,na.rm = T),
                presMean=mean(Smp.5,na.rm = T),presMax=max(Smp.5,na.rm = T),
                presMin=min(Smp.5,na.rm = T),dir=Smp.8[which.max(Smp.7)],
                vel=max(Smp.7,na.rm = T)*3.6,radMean=mean(Smp.4,na.rm = T),
                radMax=max(Smp.4,na.rm = T),STm=min(Stmin,na.rm=T),STM=max(Stmax,na.rm=T),.groups = 'drop')

    #e1<-e[6677:6772,]
    #head(e1);tail(e1)
    #sum(e1$Tot)
    #view(e1)
    #view(f)
    #view(e)
    f$dir123<-DIR(round(f$dir))
    g<-f[,c(1:13,19,14:18)]
    h<-g %>% unite(date,y:d,sep = '-')
    h[,2:11]<-apply(h[,2:11],2,round,1)
    h[,13:17]<-apply(h[,13:17],2,round,1)
    setwd(old)
    out<-setwd('C:/Users/Cocha/Desktop/output-cbba/out')
    write.csv(h,gsub('$','.csv',est),row.names = F)
    setwd(old)
  }
  setwd(old)
}
########
#write.csv(h,'tex122.csv')
