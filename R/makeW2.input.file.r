#' ## Write npt file for CE-QUAL-W2
#' @param x.df dataframe with first column JDAY
#' @param new.npt.filename character; filename
#' @param fwf logical; write a fixed-width file or csv?
#' @param path character; define the path to write to
#' @param note character; write a note to describe the data in the first 3 lines of the file
#' @param writefiles logical; write file or return a dataframe?
#' @return a data.frame if writefiles == F
#' @author Norman Buccola
#' @keywords CEQUALW2 model write npt files
#' @examples
#' makeW2.input.file()
#' @export
makeW2.input.file<-function(x.df,new.npt.filename,fwf=TRUE,
  path='',note='',writefiles=TRUE){
  x.df<-as.data.frame(x.df)
  x.df[,]<-apply(x.df,2,round,3)
  unique.rows<-function(x){x[match(unique(x$JDAY),x$JDAY),]}
  x.df<-unique.rows(x.df)
  x.df<-x.df[!apply(apply(x.df,2,is.na),1,any),]
if(fwf){
  writeLines(c(paste('#', new.npt.filename),
               paste('#', note)), con = new.npt.filename, sep = "\n", useBytes = FALSE)
  gdata::write.fwf(as.data.frame(t(matrix(substr(colnames(x.df),0,8),byrow=FALSE)),
                          substr(colnames(x.df),0,8)),
          file=new.npt.filename,
          sep="", na="",rownames=FALSE,
          colnames=FALSE, justify="left",formatInfo=FALSE,
          width=8,append=TRUE)
#  write(c(paste('#',deparse(colnames(x.df)))),new.npt.filename,append=T)
  write.fwf(x.df, file=new.npt.filename, sep="", na="",
    colnames=FALSE,
            justify="right",formatInfo=TRUE,  width=8,append=TRUE)
}else{
  writeLines(c(paste('$', new.npt.filename),
               paste('#', note)), con = new.npt.filename, sep = "\n", useBytes = FALSE)
  write.table(as.data.frame(t(matrix(substr(colnames(x.df),0,8),byrow=FALSE)),
                          substr(colnames(x.df),0,8)),
            file=new.npt.filename,
            sep=",", na="",row.names=FALSE,
            col.names = FALSE,append=TRUE)
  #  write(c(paste('#',deparse(colnames(x.df)))),new.npt.filename,append=T)
  write.table(x.df,
              file=new.npt.filename,
              sep=",", na="",row.names=FALSE,
              col.names = FALSE,append=TRUE)
}
  print(  paste(path,new.npt.filename,sep=""))
  #setwd(directory)
}

