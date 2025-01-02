library(tidyverse)
library(readxl)
options(warn=-1)
############
{
  #setwd('/home/ruiz/senamhi/file15min/')
  est<-'Templo Misicuni'
  old<-getwd()
  setwd(old)
  setwd('C:/Campbellsci/LoggerNet')
  arc<-'Templo_EPT_Min15.dat'#vientos
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
    e$Max<-as.numeric(e$Max);e$Min<-as.numeric(e$Min);
    e$Avg.1<-as.numeric(e$Avg.1);e$Max.1<-as.numeric(e$Max.1);
    e$Min.1<-as.numeric(e$Min.1);
    #view(e)
    #write.csv(e,'test.csv')
    e[is.na(e)]=NA
    #e[id,13]<-NA
    #e[id,14]<-NA
    f<-e %>% group_by(y,m,d) %>% summarise(prcp=sum(Tot,na.rm = T),
                                           tx=max(Max,na.rm = T),
                tn=min(Min,na.rm = T),HRmean=mean(Avg.1,na.rm = T),
                HRmax=max(Max.1,na.rm = T),HRmin=min(Min.1,na.rm = T),.groups = 'drop')

   e0<-e[,c(1,2,4,8,9,10,12,13)]
    names(e0)<-c("date","time","prcp",'tx','tn','HRmean','HRmax','HRmin')
    setwd('C:/Users/Cocha/Desktop/output-cbba/out_0')
    write.csv(e0,gsub('$','.csv',est),row.names = FALSE,sep = ',')
    
    iden<-grep(date(now()),e0$date,fixed = T)
    if(length(iden)!=0){
      iden1<-which((e0[iden,3]!=0)==TRUE)
      e1<-e0[iden,]
      e2<-e1[iden1,]
      setwd('C:/Users/Cocha/Desktop/output-cbba/out_rain')
      write.csv(e2,gsub('$','.csv',est),row.names = FALSE,sep = ',')  
    }else{
      next
    }

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
}
########
#write.csv(h,'tex122.csv')