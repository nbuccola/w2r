#' This function will extract the inflows to a CE-QUAL-W2 model
#'
#' @param path character; model directory to read from
#' @param q.rows list; rows of the control file to read defaults to q.rows=list(QIN=1:4,QTR=1:2,QDT=1)
#' @param d.avg logical; daily averaging? Defaults to TRUE
#' @return a data.frame with the model inflows
#' @author Norman Buccola
#' @keywords CEQUALW2 model output post-processing read inflows sum
#' @examples
#' get.w2.inflows()
#' @export
get.w2.inflows<-function(path=NA,
                         q.rows=list(QIN=1:4,QTR=1:2,QDT=1),#  rows of the control file to read
                         d.avg=T){ #,first.day=1
  directory=getwd()
  setwd(path)
  # get inflows and model output
  w2lns<-readLines('w2_con.npt')
  QinNms<-c("QIN FILE","QTR FILE","QDT FILE")
  QinNms<-QinNms[unlist(sapply(names(q.rows),grep,QinNms))]
  qinLines<-unlist(sapply(QinNms,grep,w2lns))
  q.rows<-unlist(mapply(function(x,y){return((x+y)-1)},x=qinLines,y=q.rows))
  #q.rows<-q.rows-1
  q.nms<-mapply(scan,file='w2_con.npt',what='character',
             #skip=qinLines,
             skip=q.rows,
             nlines=1,sep='',quiet=T)
  print(q.nms)
  npt.names<-apply(q.nms,2,function(x)x[length(x)])
  npt.names<-gsub('[\\]','/',npt.names)
  print(npt.names)

  #################################################################################
  # read in each flow INPUT files, then take a daily average of the flow
  #source(paste(directory,'/r_functions/read.interp.r',sep=''))
  all.qin<-sapply(npt.names,read.interp,d.avg=d.avg) #,first.day=first.day)
  all.qin<-as.data.frame(all.qin)
  if(ncol(all.qin)>2){
      Qs<-all.qin[,1]
      for(i in 2:ncol(all.qin)){
          Qs<-merge(Qs,all.qin[,i],by='JDAY')
      }
      colnames(Qs)[-1]<-npt.names
      # add all flow inputs
      Qs$Total.Qin<-apply(Qs[,-1],1,sum,na.rm=T)
  }else{Qs<-as.data.frame(all.qin[[1]])
        print(str(Qs))#;print(str(as.data.frame(Qs[[1]])))
        print(colnames(Qs))
        colnames(Qs)[2]<-'Total.Qin'
    }
  #Qs[,-1]<-Qs[,-1]*35.314666 # convert to cfs for plotting!!
  #print(summary(Qs))#;head(Qs);#plot(Qs)
  setwd(directory)
  return(Qs[,c('JDAY','Total.Qin')])
}
