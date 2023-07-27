#' Wrapper function for waterBalance function
#'
#' This function will grab inputs from CE-QUAL-W2 model (w2_con.npt file), lookup filenames, and then use them as input to the waterBalance function within the w2r package.
#'
#' @param path, # path to model
#' @param wd,  # Generic path to find functions
#' @param seg, #Segment to look up in w2_con.npt
#' @param wb, #Water body to look up in w2_con.npt
#' @param RESSIMCode, #RES-SIM name for boundary conditions
#' @param elvVolCrvFl, #Elevation - Volume Curve
#' @param write.files #T/F on whether or not to write file
#' @return
#' \item{watbal}{output from the waterBalance function}
#' @author Norman Buccola
#' @keywords CEQUALW2 water balance QDT
#' @examples
#' readW2ConInOut()
#' @export
# Read in the number of branches (QIN) and gates (QGT)
readW2ConInOut<-function(path, # path to model
                         wd,  # Generic path to find functions
                         seg, #Segment to look up
                         wb, #Water body to look at
                         RESSIMCode, #RES-SIM name for boundary conditions
                         elvVolCrvFl, # Elevation - Volume Curve
                         write.files #T/F on whether or not to write file
                         ){
  #setwd(wd)
  # source(file.path(wd,'r_functions/errs.r'))
  # source(file.path(wd,'r_functions/waterBalanceGeneric.r'))
  # source(file.path(wd,'r_functions/apply.davg.oncols.r'))
  # source(file.path(wd,'r_functions/moving.avg.r'))
  # source(file.path(wd,'r_functions/modifyInitWSELVbth.r'))
  write.files <- as.logical(write.files)
  w2Pth<-paste0(path,'/w2_con.npt')
  w2slns<-readLines(w2Pth)
  vars.in<-c("QDT FILE") #"QIN FILE","QTR FILE",
  vars.out<-"QOT FILE"
  qdtSwitch<-"DST TRIB"
  vars<-c("NBR ","NTR ","LOCATION ",qdtSwitch,vars.in,vars.out)
  varnums<-as.list(rep(NA,length(vars)))
  names(varnums)<-varnames<-vars
  npt.lines<-unlist(sapply(vars,grep,w2slns))
  end.lines<-unlist(sapply(npt.lines,
                           function(x){xe<-grep('^[[:space:]]*$',w2slns[x:length(w2slns)])[1]-1
                           return(xe+x)}))

  # Get input file names and w2_con parameters
  print('Reading w2_con.npt')
  for(i in 1:length(vars)){
    #print(vars[i])
    if(grepl('FILE',vars[i])){
      widths<-c(8,90)
    }else{
      widths<-rep(8,10)
    }
    varnum<-read.fwf(w2Pth,widths=widths,stringsAsFactors = F,
                     skip=npt.lines[i]-1,n=end.lines[i]-npt.lines[i])
    if(any(grepl(vars[i],vars.in))|
       any(grepl(vars[i],qdtSwitch))|
       any(grepl(vars[i],vars.out))
    ){
      npti<-w2slns[(npt.lines[i]+1):(end.lines[i]-1)]
      # Trim leading and trailing white space
      if(length(npti)>1){
        # Choose the correct branch/waterbody
        npti<-npti[varnums[['BRANCH']]]
      }
      npti<-gsub("^\\s+|\\s+$","",substr(npti,9,nchar(npti)))
      # Check to see if QDT is on or off and remove from list if off
      qdtSwitchi<-names(varnums)[i]==qdtSwitch #"DST TRIB" #DTRC"
      # if(vars[i]=="QIN FILE"){
      #   Qinpts<-npti
      # }else{
        if(!vars[i]%in%vars.out & # Do not include QGT file
           any(!qdtSwitchi) &
           ((vars[i]=="QDT FILE" & any(qdtSwitch=="ON")) | #include QDT file(s)
            (vars[i]=="QTR FILE" & varnums[["NTR "]]!=0)) #include Trib file(s)
        ){
          #Qinpts<-c(Qinpts,npti)
        }else{
          # Read QGT filename
          if(vars[i]%in%vars.out){
            Qoutpt<-npti
          }else{
            if(any(qdtSwitchi)){qdtSwitch<-npti}
          }
        }
      #}

      varnums[[i]]<-npti
    }else{
      if(grepl("LOC",vars[i])){
        # Choose the correct waterbody and define the branch
        varnums[[i]]<-as.numeric(varnum[grep(wb,varnum[,1]),5])
        names(varnums)[i]<-'BRANCH'
      }else{
        varnums[[i]]<-as.numeric(varnum[2,grep(gsub(' ','',vars[i]),varnum[1,])])
      }
    }
  } # End of var loop

  # Read inflows and sum
  # if(length(Qinpts)>1){
  #   for(qin in 1:length(Qinpts)){
  #     if(grepl('not used',Qinpts[qin])){
  #       next
  #     }
  #     if(grepl('.out|.csv',Qinpts[qin])){
  #       Qini<-read.csv(paste0(path,'/',Qinpts[qin]),
  #                      skip=2,col.names = c('JDAY',Qinpts[qin]))
  #     }else{
  #       Qini<-read.fwf(paste0(path,'/',Qinpts[qin]), widths=c(8,8),
  #                      skip=3,col.names = c('JDAY',Qinpts[qin]))
  #     }
  #     Qini$JDAY<-round(Qini$JDAY)
  #     if(qin==1){
  #       Qin1<-Qini
  #     }else{
  #       # Interpolate missing values
  #       jds<-sort(unique(c(Qini$JDAY,Qin1$JDAY)))
  #       if(any(!jds%in%Qini$JDAY)){
  #         Qini<-data.frame(JDAY=jds,Qi=approx(x=Qini$JDAY,y=Qini[,2],xout=jds)$y)
  #         colnames(Qini)[2]<-Qinpts[qin]
  #       }
  #       Qin1<-merge(Qin1,Qini,by='JDAY',all=T)
  #     }
  #   } # End of loop reading in Qinpts
  #   print(str(Qin1))
  #
  #   if(ncol(Qin1)==2){
  #     Qin<-data.frame(JDAY=Qin1$JDAY,Qin=Qin1[,-match('JDAY',colnames(Qin1))])
  #   }else{
  #     Qin<-data.frame(JDAY=Qin1$JDAY,
  #                     Qin=apply(Qin1[,-match('JDAY',colnames(Qin1))],1,sum))
  #   }
  # }else{
  #   Qin<-read.csv(paste0(path,'/',Qinpts),skip=2,col.names = c('JDAY',Qinpts))
  #   Qin$JDAY<-round(Qin$JDAY)
  # }
  qfls<-list.files(file.path(path),pattern=paste0('qwo_',seg,'\\**'))
  # Read in outflows
  print(paste('Reading',qfls))
  Qot<-read.csv(file.path(path,qfls),skip=3,header=F,stringsAsFactors = F)[,c(1:2)]
  colnames(Qot)<-c('JDAY',paste0('qwo_',seg))
  #Read Elevation file boundary conditions
  meas.elvs<-list.files(path)
  meas.elvs<-meas.elvs[grepl("ELEV|elev|WSELV",meas.elvs) &
                       grepl(RESSIMCode,meas.elvs) &
                       !grepl('png',meas.elvs)]#,pattern="ELEV.csv")
  if(length(meas.elvs)<1){
    meas.elvsFldr<-strsplit(npti,'\\\\')[[1]]
    yr <- strsplit(meas.elvsFldr[length(meas.elvsFldr)],'_')[[1]]
    yr <- gsub('.csv','',yr[length(yr)]) #as.numeric(gsub("\\D","",
    meas.elvsFldr <- meas.elvsFldr[-length(meas.elvsFldr)]
    meas.elvs<-file.path(paste(meas.elvsFldr,sep="", collapse="//"),
                         paste0(yr,'_',RESSIMCode,'-POOLELEV.csv'))
  }

  print(meas.elvs)
  if(length(meas.elvs)>1){
    meas.elvs<-meas.elvs[grep(RESSIMCode,meas.elvs)]
  }
  ##########################
  #Execute the water balance
  ##########################

  if(qdtSwitch=="ON"){ #append old water balance file if QDT was ON
    append.filename<-varnums[["QDT FILE"]][1]
    new.npt.filename<-NA
    print('QDT set to ON in w2_con.npt')
    #meas.elvs<-meas.elvs[grep(gsub("_.*","",append.filename),meas.elvs)]
  }else{ #write a new water balance file if QDT was OFF
    append.filename<-NULL
    new.npt.filename<-paste0("qwb_",seg,".csv")
    print('QDT set to OFF in w2_con.npt')
  }
  if(write.files){
    save.plot<-T
  }else{
    save.plot<-T
  }
  tsrs<-list.files(path,pattern="tsr_")
  opt.txt<-tsrs[grep(paste0('seg',seg),tsrs)]
  opt.txt<-opt.txt[1]
  #source("C:/Users/g2echnb9/Documents/R/w2r_dev/R/waterBalanceGeneric.r")
  watbal<-waterBalance(opt.txt='wl.opt', # opt.txt, #
                       seg=seg,
                       wb=wb,
                       meas.elvs=meas.elvs,
                       elvVolCrvFl = elvVolCrvFl,
                       new.npt.filename = new.npt.filename,
                       append.filename=append.filename,
                       path=path,
                       write.files=write.files,
                       save.plot=save.plot)
  print(watbal$fit)


  #summary(Qin)
  #summary(Qot)
  return(list(#Qin=Qin,
              #Qot=Qot,
              watbal=watbal))
}
