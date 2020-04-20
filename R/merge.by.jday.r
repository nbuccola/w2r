#' Merge 2 dataframes by JDAY
#'
#' @param df1 dataframe; with JDAY column and another column with the data
#' @param df2 dataframe; with JDAY column and another column with the data
#' @return a dataframe with JDAY as the first column
#' @author Norman Buccola
#' @keywords merge 2 dataframes by JDAY
#' @examples
#' merge.by.jday()
#' @export
merge.by.jday<-function(df1,df2){
  #make sure that JDAY is in the first column of each data.frame
  df1$JDAY<-round(df1$JDAY,2);df2$JDAY<-round(df2$JDAY,2)
  df1.sub<-df1[df1$JDAY>=min(df2$JDAY) & df1$JDAY<=max(df2$JDAY),]
  df2.sub<-df2[df2$JDAY>=min(df1$JDAY) & df2$JDAY<=max(df1$JDAY),]
  JDAY<-sort(unique(c(df1.sub$JDAY,df2.sub$JDAY)))
  big.df<-as.data.frame(cbind(JDAY,matrix(NA,length(JDAY),ncol(df1.sub)+ncol(df2.sub)-2)))
  big.df[JDAY%in%df1.sub$JDAY,2:ncol(df1.sub)]<-df1.sub[df1.sub$JDAY%in%JDAY,-1]
  big.df[JDAY%in%df2.sub$JDAY,(1+ncol(df1.sub)):ncol(big.df)]<-df2.sub[df2.sub$JDAY%in%JDAY,-1]
  colnames(big.df)[-1]<-c(colnames(df1)[-1],colnames(df2)[-1])
  return(big.df)}

#' Merge 2 dataframes by JDAY
#'
#' @param df1 dataframe; with JDAY column and another column with the data
#' @param df2 dataframe; with JDAY column and another column with the data
#' @param return.nas logical; Include missing values?
#' @return a dataframe with JDAY as the first column
#' @author Norman Buccola
#' @keywords merge 2 dataframes by JDAY
#' @examples
#' merge.by.jday()
#' @export
merge.interp<-function(x1,x2,return.nas=T){
     #this assumes the first column is JDAY
   x1<-as.data.frame(x1);x2<-as.data.frame(x2)
   x1$JDAY<-round(x1$JDAY,2);x2$JDAY<-round(x2$JDAY,2)
   #only use hourly data
   new.i<-round(seq(ceiling(min(min(x1$JDAY,na.rm=T),
                                min(x2$JDAY,na.rm=T))),
                    floor(max(max(x1$JDAY,na.rm=T),
                              max(x2$JDAY,na.rm=T))),by=1/24),2)
   narows1<-apply(is.na(x1),1,all)
   narows2<-apply(is.na(x2),1,all)
   x1i<-unique(round(new.i))%in%unique(round(x1$JDAY[!narows1]))
   print(paste('x1 is missing JDAY:',unique(round(new.i))[!x1i]))
   x2i<-unique(round(new.i))%in%unique(round(x2$JDAY[!narows2]))
   print(paste('x2 is missing JDAY:',unique(round(new.i))[!x2i]))
   x<-merge(x1,x2,by='JDAY',all.x=T,all.y=T)
   colnames(x)<-c('JDAY',colnames(x1)[-1],colnames(x2)[-1])
   #str(x);summary(x)
   if(!return.nas){x<-as.data.frame(na.approx(x))}
   return(x)
}

#' Merge 2 dataframes by JDAY with hourly data
#'
#' @param x1 dataframe; with JDAY column and another column with the data
#' @param x2 dataframe; with JDAY column and another column with the data
#' @param return.nas logical; Include missing values?
#' @return a dataframe with JDAY as the first column
#' @author Norman Buccola
#' @keywords merge dataframes by JDAY
#' @examples
#' merge.interp.24hr()
#' @export
merge.interp.24hr<-function(x1,x2,return.nas=T){
     #this assumes the first column is JDAY
   x1<-as.data.frame(x1);x2<-as.data.frame(x2)
   x1$JDAY<-round(x1$JDAY,2);x2$JDAY<-round(x2$JDAY,2)
   #only use hourly data
   new.i<-round(seq(ceiling(min(min(x1$JDAY,na.rm=T),
                                min(x2$JDAY,na.rm=T))),
                    floor(max(max(x1$JDAY,na.rm=T),
                              max(x2$JDAY,na.rm=T))),by=1/24),2)
   narows1<-apply(is.na(x1),1,all)
   narows2<-apply(is.na(x2),1,all)
   #remove missing columns
   nacols1<-apply(apply(x1,2,is.na),2,all)
   nacols2<-apply(apply(x2,2,is.na),2,all)
   x1<-x1[,!nacols1]; x2<-x2[,!nacols2]
   x1i<-unique(round(new.i))%in%unique(round(x1$JDAY[!narows1]))
   print(paste('x1 is missing JDAY:',unique(round(new.i))[!x1i]))
   x2i<-unique(round(new.i))%in%unique(round(x2$JDAY[!narows2]))
   print(paste('x2 is missing JDAY:',unique(round(new.i))[!x2i]))
   #x<-merge(x1[x1i,],x2[x2i,],by='JDAY',all.x=T,all.y=T)
   x<-as.data.frame(matrix(NA,length(new.i),(1+ncol(x1)-1+ncol(x2)-1)))
   colnames(x)<-c('JDAY',colnames(x1)[-1],colnames(x2)[-1])
   x$JDAY<-new.i
   #print(head(x))
   for(j in 2:(ncol(x1))){
       x[,j]<-approx(x=x1[,1],y=x1[,j],xout=new.i,rule=1)$y
   }
   for(j in 2:(ncol(x2))){
       #print(j)
       if(length(which(!is.na(x2[,j])))>1){
           x[,(j+ncol(x1)-1)]<- approx(x=x2[,1],y=x2[,j],xout=new.i,rule=1)$y
       }
   }
   #if(return.nas)
   #str(x);summary(x)
#    if(!return.nas)x<-x[!apply(apply(x,2,is.na),1,any),]

   if(!return.nas){x<-na.approx(x)}
   return(x)
}

