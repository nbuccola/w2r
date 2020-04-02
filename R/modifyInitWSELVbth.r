#' Modify initial water surface elevation in bathymetry file
#'
#' Function to re-write the initial water surface within the bathymetry file of a CE-QUAL-W2 model v0.3 2020-03-02
#'
#' @param path Define the model path in a character string
#' @param newInitWSELV numeric; New initial WSELV to be in meters - note: If not NA, the initial water level in the bathymetry file will be re-written to this value!
#' @param wb numeric CE-QUAL-W2 waterbody to look at
#' @author Norman Buccola
#' @keywords CEQUALW2 water balance
#' @examples
#' modifyInitWSELVbth()
#' @export
modifyInitWSELVbth<-function(path,
                             newInitWSELV=NA, # New initial WSELV to be in meters
                             wb #Water body to look at
                             ){
  w2Pth<-paste0(path,'/w2_con.npt')
  w2slns<-readLines(w2Pth)
  bth<-"BTH FILE"
  vars<-c(bth)
  varnums<-as.list(rep(NA,length(vars)))
  names(varnums)<-varnames<-vars
  npt.lines<-unlist(sapply(vars,grep,w2slns))
  end.lines<-unlist(sapply(npt.lines,
                           function(x){xe<-grep('^[[:space:]]*$',w2slns[x:length(w2slns)])[1]-1
                           return(xe+x)}))
  # only applied to lop-dex and hcr so far

  #for(i in 1:length(vars)){
    i=1
    if(grepl('FILE',vars[i])){
      widths<-c(8,90)
    }else{
      widths<-rep(8,10)
    }
    varnum<-read.fwf(w2Pth,widths=widths,stringsAsFactors = F,
                     skip=npt.lines[i]-1,n=end.lines[i]-npt.lines[i])
    bthPth<-file.path(path,varnum[wb+1,2])
    bthlns<-readLines(bthPth)
    blankLines<-grep("^$",bthlns)
    widths<-rep(8,10)
    initELWS<-read.fwf(file.path(path,varnum[wb+1,2]),widths=widths,stringsAsFactors = F,
                       skip=blankLines[2]+1,n=blankLines[3]-blankLines[2]-2)
    initELWSval<-unique(apply(initELWS,1,unique))[[1]]
    print(paste('Changing Initial WSELV in ',bth,'from',initELWSval,'to',newInitWSELV[,2]))
    initELWS[!is.na(initELWS)]<-round(newInitWSELV[,2],3)
    bthTop<-bthlns[1:(blankLines[2]+1)]
    bthBot<-bthlns[blankLines[3]:length(bthlns)]
    # Rewrite Bathymetry file
    write(bthTop,file =bthPth,append = F,ncolumns = 1)
    write.fwf(initELWS,file = bthPth,
              na = "",justify='right',sep="",
              width=widths,append = T,colnames = F)
    write(bthBot,file =bthPth,append = T,ncolumns = 1)

}
