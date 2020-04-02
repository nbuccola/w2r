#' Calculate fit statistics
#'
#' Function to compute the fit statistics of model and measured data v0.1 2020-03-02
#'
#' @param out dataframe with measured data in first column, simulated in second column. No missing values allowed.
#' @return
#' \item{x}{a dataframe with the number of observations (N), root mean squared error (RMSE), nash-sutcliffe error (NS), mean error (ME), mean absolute error (MAE), Kendall tau correlation coefficient (COR), SMAPE=SMAPE}
#' @author Norman Buccola
#' @keywords error fit CEQUALW2 water balance
#' @examples
#' errs()
#' @export
errs<-function(out){
  narows<-apply(apply(out,2,is.na),1,any)
  NSE <- function(sim,obs){
    1 - ( sum( (obs - sim)^2 ) / sum( (obs - mean(obs, na.rm = TRUE))^2 ))
  }
  rmse <- function(sim,obs){
    sqrt( mean( (sim - obs)^2, na.rm = TRUE) )
  }
  #measured should be first column, modeled in second!
  meas<-out[!narows,1];mod<-out[!narows,2];
  nonzero<-which(meas>=1e-2|meas<=-1e-2)# Do not include "zero" values from measured data in MAPE calculation
  RMSE<-round(rmse(as.numeric(meas),as.numeric(mod)),digits=3)
  NS<-round(NSE(as.vector(mod),as.vector(meas)),digits=3)
  #NashSutliffe Error:  1-mean((mod-meas)^2)/var(meas)
  ME<-round(mean(as.numeric(mod),na.rm=TRUE)-mean(as.numeric(meas),na.rm=TRUE),digits=3)
  MAE<-round(mean(abs(as.numeric(meas[nonzero])-
                        as.numeric(mod[nonzero])),na.rm=TRUE),digits=3)
  COR<-round(cor(as.vector(mod),as.vector(meas),method="kendall"),digits=3)
  #SMAPE<-round(mean(abs((as.numeric(meas[nonzero])-as.numeric(mod[nonzero]))/
  #                        ((as.numeric(meas[nonzero])+as.numeric(mod[nonzero]))/2)),na.rm=TRUE),digits=3)
  #ABDEV<-round(mean(abs(as.numeric(mod)-mean(as.numeric(meas),na.rm=TRUE))),digits=3)
  return(data.frame(N=length(meas),RMSE=RMSE,NS=NS,ME=ME,MAE=MAE,COR=COR))#,SMAPE=SMAPE))
}
