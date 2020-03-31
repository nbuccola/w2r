#' Calculate the estimated egg emergence timing based on cumulative degree-days of water temperature time-series data
#'
#' @param tout dataframe; tout is a dataframe with the first column as a numeric Julian day
#' @param atu.day numeric; the day in which to start the emergence calculation, default = 263
#' @param davg logical; calculate based on daily average, default = TRUE
#' @param interpMissing logical, interpolate missing values? Default = TRUE
#' @param hatchValue numeric, the value in which eggs are assumed to hatch. Defaults to 1750
#' @return
#' \item{atu.d is a dataframe with numeric JDAY dates}
#' \item{atu is a dataframe with month and day units for the date}
#' @author Norman Buccola
#' @keywords calculate emergence timing CEQUALW2 water temperature
#' @examples
#' calcEmergenceTiming()
#' @export
calcEmergenceTiming<-function(tout,

                              atu.day=263, #atu.day is the day in which to start the emergence calculation
                              davg=T,
                              interpMissing=T,
                              hatchValue=1750
                              ){
  if(interpMissing){
    if(any(!is.na(tout$JDAY<atu.day)) & floor(tout$JDAY[1])<=2){
       jd<-1:366
    }else{
      jd<-atu.day:max(tout$JDAY,na.rm=T)
    }
    #jd<-jd[jd>=min(tout$JDAY) & jd<=max(tout$JDAY)]
    jdi<-!jd%in%unique(floor(tout$JDAY))
    if(any(jdi)){
      # fill missing days
      tout<-data.frame(JDAY=jd,Temp=approx(x=tout[,1],y=tout[,2],xout=jd,rule=2)$y)
    }
  }
  if(floor(tout$JDAY[1])<=2){
    atu.temps<-rbind(tout[tout$JDAY>atu.day,],
                     tout[tout$JDAY<atu.day,])
  }else{
    atu.temps<-tout[tout$JDAY>atu.day,]
  }

    if(mean(atu.temps[,2],na.rm=T)<32){
        atu.temps[,-1]<-atu.temps[,-1]*(9/5)+32 # convert to F
    }
    atu.temps$JDAY[atu.temps$JDAY<atu.day]<-
         atu.temps$JDAY[atu.temps$JDAY<atu.day]+max(atu.temps$JDAY)
    atu.temps<-atu.temps[!apply(apply(atu.temps,2,is.na),1,any),]
    if(davg){
       atu.temps<-apply.davg.oncols(atu.temps)
    }
    #print(summary(atu.temps))
    cum.dif<-atu.temps
    cum.dif[,-1]<-NA
    if(ncol(atu.temps)>2){
        cum.dif[,-1]<-apply(atu.temps[,-1]-32,2,cumsum)
        atu.d<-apply(cum.dif[,-1],2,function(x){cum.dif[x>=hatchValue,1][1]})
    }else{
        cum.dif[,-1]<-cumsum(atu.temps[,-1]-32)
        atu.d<-cum.dif[cum.dif[,-1]>=hatchValue,1][1]
    }
    atu<-as.data.frame(t(format(as.Date(as.numeric(atu.d),origin=as.Date('2010-12-31')),'%m/%d')),stringsAsFactors=F)
    atu.d<-as.data.frame(t(round(as.numeric(atu.d))),stringsAsFactors=F)
    #print(atu)
    return(list(atu=atu,atu.d=atu.d))
}
