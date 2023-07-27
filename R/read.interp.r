#' Read in each flow INPUT files, then take a daily average of the flow from a CE-QUAL-W2 model
#'
#' @param npt character; name of file to read
#' @param pre logical; precip file?
#' @param d.avg logical; daily averaging? Defaults to TRUE
#' @param first.day logical; first day to start getting data
#' @param area.acres numeric; area of reservoir in acres to convert rainfall to pre.npt file
#' @return a data.frame with the model inflows
#' @author Norman Buccola
#' @keywords CEQUALW2 model output post-processing read files
#' @examples
#' read.interp()
#' @export
read.interp<-function(npt,pre=F,d.avg=T,first.day=NA,area.acres=3500){
    print(npt)
    csvfrmt <- readChar(npt,1) == '$'
    if(csvfrmt){
        qin<-read.csv(npt,header =F,#sep="",
                      stringsAsFactors = F,skip=3)
        colnames(qin)<-c("JDAY",paste0(npt,(1:(ncol(qin)-1))))
    }else{
        qin<-read.fwf(npt, widths=c(8,8),# header =T,#sep="",
                      stringsAsFactors = F,skip=3,col.names=c('JDAY',npt))
    }
  if(is.na(first.day)|!is.numeric(first.day)){
    first.day<-qin$JDAY[!is.na(qin[,"JDAY"])][1]
  }
  last.day<-qin$JDAY[length(which(!is.na(qin$JDAY)))]
  qin.fall<-qin[qin$JDAY>first.day&qin$JDAY<last.day,]
  tm<-seq(first.day,last.day,by=0.04166667)#note that this will interpolate values !!!
  qin.fall.i<-data.frame(JDAY=tm,
    Q=approx(x=qin.fall$JDAY,y=qin.fall[,2],xout=tm,rule=2)$y)
  if(pre){ # convert m/s to cms:  multiply by Detroit area of 3700 acres * 4046.85643 [m^2/acre]
    qin.fall.i[,2]<-qin.fall.i[,2]*area.acres*4046.85642}
  if(d.avg){all.days<-seq(floor(first.day),floor(last.day),1) #daily basis
    day<-unique(floor(qin.fall.i$JDAY))
    }else{all.days<-seq(first.day,last.day,by=0.04166667)#hourly basis
    day<-unique(qin.fall.i$JDAY)}
  if(d.avg){qout<-data.frame(JDAY=all.days,Q=NA)
    for(i in 1:length(day)) {
    qout[all.days==day[i],2]<-
      mean(qin.fall.i[floor(qin.fall.i$JDAY)==day[i],2])}
  }else{qout<-qin.fall.i}
  if(is.na(qout$Q[nrow(qout)]))  qout$Q[length(qout$Q)]<-0
  if(is.na(qout$Q[1])) qout$Q[1]<-0
    #qout$Q<-na.approx(qout$Q)
    qout$Q<-approx(y=qout$Q,x=qout$JDAY,xout=qout$JDAY,rule=2)$y

  #print(str(qout))
    return(qout)}
