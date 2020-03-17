#' Apply daily average on columns of dataframe
#'
#' Function to apply a daily average on the columns of a dataframe v0.1 2020-03-02
#'
#' @param mod.wo character; A data frame with JDAY (numeric) as the first column in sub-daily timesteps
#' @param daystep numeric; timestep (in days) to calculate average over
#' @param ignoreQzeros logical; ignores zero flows in daily averaging !!!
#' @param calc.max logical; if true, calculate the daily max, not the daily average!
#' @param calc.min logical; if true, calculate the daily min, not the daily average!
#' @return
#' \item{a dataframe with daily data instead of sub-daily}
#' @author Norman Buccola
#' @keywords daily average CEQUALW2 water balance
#' @examples
#' apply.davg.oncols()
#' @export
apply.davg.oncols<-function(mod.wo, # A data frame with JDAY as the first column
                            daystep=1, # timestep (in days) to calculate average over
                            ignoreQzeros=F,  # ignores zero flows in daily averaging !!!
                            calc.max=F, # if true, calculate the daily max, not the daily average!
                            calc.min=F # if true, calculate the daily min, not the daily average!
){
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
