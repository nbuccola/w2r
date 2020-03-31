#' This function will modify the control file for a CE-QUAL-W2 model (w2_con.npt)
#'
#' @param path character; model directory to read from
#' @param seg numeric; which segment to extract from the model?
#' @param wb numeric; which water body to lookup in the model?
#' @return re-writes a new w2_con.npt file
#' @author Norman Buccola
#' @keywords CEQUALW2 model read w2_con.npt
#' @examples
#' modifyW2con()
#' @export
modifyW2con<-function(path,qdt="OFF",
                      seg, #Segment to look up
                      wb #Water body to look at
                      ){
  w2Pth<-paste0(path,'/w2_con.npt')
  w2slns<-readLines(w2Pth)
  qdtSwitch<-"DST TRIB"
  vars<-c(qdtSwitch)
  varnums<-as.list(rep(NA,length(vars)))
  names(varnums)<-varnames<-vars
  npt.lines<-unlist(sapply(vars,grep,w2slns))
  end.lines<-unlist(sapply(npt.lines,
                           function(x){xe<-grep('^[[:space:]]*$',w2slns[x:length(w2slns)])[1]-1
                           return(xe+x)}))
  # only applied to lop-dex and hcr so far
  branch<-wb

  #for(i in 1:length(vars)){
    i=1
    if(grepl('FILE',vars[i])){
      widths<-c(8,90)
    }else{
      widths<-rep(8,10)
    }
    w2conTop<-w2slns[1:(npt.lines[i]-1)]
    w2conBot<-w2slns[end.lines[i]:length(w2slns)]

    varnum<-read.fwf(w2Pth,widths=widths,stringsAsFactors = F,
                     skip=npt.lines[i]-1,n=end.lines[i]-npt.lines[i])

    varnum[wb+1,2]<-qdt

    print(paste('Changing', qdtSwitch, 'to', qdt, 'in',w2Pth))
    # Rewrite w2con.npt
    #fileConn<-file(w2Pth)
    #writeLines(w2conTop,con =fileConn)
    write(w2conTop,file =w2Pth,append = F,ncolumns = 1)
    gdata::write.fwf(varnum,file = w2Pth,
              na = "",justify='right',sep="",
              width=widths,append = T,colnames = F)
    #close(fileConn)
    write(w2conBot,file =w2Pth,append = T,ncolumns = 1)

}
