#' Read in the w2_selective.npt file for the CE-QUAL-W2 model
#'
#' @param path character; path to model
#' @return
#' \item{out}{a list with the number of outlets and priorities of each group}
#' @author Norman Buccola
#' @keywords CEQUALW2 read w2_selective.npt
#' @examples
#' readW2ConInOut()
#' @export
readW2Selective<-function(path=path){
  # these are the rows of the W2selective.npt file to read
  #library(foreach)
  fl<-file.path(path,'w2_selective.npt')
  w2slns<-readLines(fl)
  npt.lines<-unlist(sapply(c("SPLIT1","SPLIT2","SPLITOUT","DEPTH","PRIORITY"),grep,w2slns))
  splitNum<-as.numeric(read.fwf(fl,widths=rep(8,10),
                      stringsAsFactors = F,
                      skip=npt.lines['SPLIT1'],n=1)[3])

  split2<-foreach::foreach(i=1:splitNum,.combine=rbind) %do% {
                  as.numeric(read.fwf(fl,widths=rep(8,10),
                            stringsAsFactors = F,
                            skip=npt.lines['SPLIT2']+i-1,n=1)[c(1,5,6,10)])
  }
  which.outs<-foreach::foreach(i=1:splitNum,.combine=rbind) %do% {
                  as.numeric(scan(fl,"character",
                          skip=npt.lines['SPLITOUT']+i-1,nlines=1)[-1])
  }
  depths<-foreach::foreach(i=1:splitNum,.combine=rbind) %do% {
                  as.numeric(scan(fl,"character",
                          skip=npt.lines['DEPTH']+i-1,nlines=1)[-1])
  }
  priorities<-foreach::foreach(i=1:splitNum,.combine=rbind) %do% {
                  as.numeric(scan(fl,"character",
                              skip=npt.lines['PRIORITY']+i-1,nlines=1)[-1])
  }
  if(is.null(nrow(split2))){
    nouts<-split2[4]
    if(any(depths!=0)){
       floaters<-which.outs[depths!=0]
    }else{
       floaters<-NA
    }
    priorities[priorities<0]<-NA
    # !assume that the highest priority outlet is the power outlet!
    #browser()
    if(all(is.na(priorities))){
      power<-NA
    }else{
      power<-which.outs[which.min(priorities)]
    }
  }else{
    split2<-split2[,c(2,3)]
    power<-floaters<-as.list(rep(NA,splitNum))
    for(st in 1:splitNum){
        if(any(depths[st,]!=0)){
            floaters[[st]]<-which.outs[st,depths[st,]!=0]
        }else{
            floaters[[st]]<-NA
        }
      priorities[[st]][priorities[[st]]<0]<-NA
      # !assume that the highest priority outlet is the power outlet!
      power[[st]]<-which.outs[st,which.min(priorities[st,priorities[st,]>0])]
    }
  }
  #print(paste(nouts,"outlets"))
  #print(paste("floating outlets:",floaters))
  return(list(power=power,floaters=floaters,
              depths=depths,priorities=priorities,
              which.outs=which.outs,
              split2=split2))
}
