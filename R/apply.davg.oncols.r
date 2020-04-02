#' Apply daily average on columns of dataframe
#'
#' @param mod.wo character; A data frame with JDAY (numeric) as the first column in sub-daily timesteps
#' @param daystep numeric; timestep (in days) to calculate average over
#' @param ignoreQzeros logical; ignores zero flows in daily averaging !!!
#' @param calc.max logical; if true, calculate the daily max, not the daily average!
#' @param calc.min logical; if true, calculate the daily min, not the daily average!
#' @return
#' \item{davg.out}{a dataframe with daily data instead of sub-daily}
#' @author Norman Buccola
#' @keywords daily average CEQUALW2 water balance
#' @examples
#' apply.davg.oncols()
#' @export
apply.davg.oncols<-function(mod.wo,
                            daystep=1,
                            ignoreQzeros=F,
                            calc.max=F,
                            calc.min=F){
    mod.wo<-mod.wo[is.numeric(mod.wo$JDAY),]
    firstday=mod.wo$JDAY[1]
    lastday=mod.wo$JDAY[nrow(mod.wo)]
    ndays<-unique(floor(mod.wo$JDAY[mod.wo$JDAY>=firstday & mod.wo$JDAY<=lastday]))
    davg.out<-as.data.frame(matrix(NA,length(ndays),ncol(mod.wo)))
    davg.out[,1]<-ndays
    for(j in 2:ncol(davg.out)){
        for(i in 1:length(ndays)){
            calc.i<-floor(mod.wo[,1])%in%ndays[i]
            if(ignoreQzeros){
                calc.i[mod.wo[calc.i,j]==0]<-FALSE
            }
            if(any(calc.i)&any(!is.na(calc.i))){
                if(calc.max){
                    davg.out[i,j]<-max(mod.wo[calc.i,j],na.rm=TRUE)
                }else{
                    if(calc.min){
                      davg.out[i,j]<-min(mod.wo[calc.i,j],na.rm=TRUE)
                    }else{
                      davg.out[i,j]<-mean(mod.wo[calc.i,j],na.rm=TRUE)
                    }
                }
            }
        }
    }
    davg.out<-as.data.frame(davg.out)
    colnames(davg.out)<-colnames(mod.wo)
    return(davg.out)
}
