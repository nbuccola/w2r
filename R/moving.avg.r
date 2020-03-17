moving.avg<-function(x,nwindow){
    ###nwindow is the moving average time window
    # x is a dataframe with a JDAY column and another column with the data
    # x must not have NA values
    library(zoo)
    x<-as.data.frame(x)
    colnames(x)<-gsub("jday","JDAY",colnames(x))
    hd<-which(x$JDAY<=(1+nwindow))
    tl<-which(x$JDAY>=(max(x$JDAY)-nwindow))
    ends<-c(hd,tl)
    x.new<- #apply a (centered!) moving average based on nwindow
        as.numeric(sapply(x$JDAY[-ends],
                          function(w.i){
                              round(mean(x[x$JDAY>=(w.i-nwindow) & x$JDAY<(w.i+nwindow),2],na.rm=TRUE),2)
                          }))
    x$smooth<-NA;
    x$smooth[-ends]<-x.new
    x$smooth[1]<-mean(x[hd,2])
    x$smooth[nrow(x)]<-mean(x[tl,2],na.rm=T)
    x$smooth[1:nrow(x)]<-na.approx(x$smooth[1:nrow(x)],rule=2)
    return(x[,c("JDAY","smooth")])
    }

moving.avg.oncols<-function(x,nwindow){
    jdcol<-match("JDAY",colnames(x))
    x.s<-x;x.s[,-jdcol]<-NA
    for(i in (1:ncol(x))[-jdcol]){
        x.s[,i]<-moving.avg(x[,c(jdcol,i)],nwindow=nwindow)$smooth
    }
    return(x.s)
}
