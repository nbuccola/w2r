#' Water Balance Utility for CE-QUAL-W2 model
#'
#' This function will compare measured and modeled lake elevation output, then write a new inflow file to be run on the next W2 model run to help achieve simulated flows that match the measured flows. This version setup for River models
#'
#' @param opt.txt Defaults to reading in the wl.opt file, but can specify a "*.tsr" output from W2
#' @param seg numeric; which segment to get wselv from W2 output
#' @param wb numeric; which water body to get from W2 model output
#' @param new.npt.filename new balance flow file to write
#' @param daystep number of days of averaging window; default to use the smallest timestep in model output
#' @param meas.elvs filename for the measured elevations (csv format)
#' @param rule.curve.filename filename for the optional Lake rule curve (csv format); if none then NA
#' @param write.files Write a new distributed tributary inflow file? TRUE or FALSE
#' @param save.plot Save a plot of the output? TRUE or FALSE
#' @param append.filename will append the named file with the new water balance file; if NULL, new.npt.filename will be used to write a new QDT file
#' @param version numeric CE-QUAL-W2 version number; if 4 or greater, output is csv format!; otherwise, it's fixed width (8 space columns)
#' @return
#' \item{fit}{a data.frame of the model to observed fit statistics}
#' \item{compElvs}{a data.frame of the daily elevations}
#' \item{compVol}{a data.frame of the daily volumes and flows}
#' @author Norman Buccola
#' @keywords CEQUALW2 water balance
#' @examples
#' waterBalance()
#' @export
waterBalance<-function(opt.txt=NA,
                           seg=NA, # Numeric
                           wb=NA, #Numeric, which water body?
                           new.npt.filename=NA,
                           rule.curve.filename=NA,#'***.csv',
                           daystep=1, # Number of days in which to average over daystep=NA will interpolate to model output
                           path='/*',
                           meas.elvs='***.npt',  #measured lake elevations
                           write.files=T,
                           save.plot=F,
                           version=4,
                           append.filename=NULL){
  #browser()

# providing append.filename will append the named file with the latest water balance
  if(version>=4 ){ #|opt.txt=="wl.opt"
      mod.opt<- read.csv(file.path(path,opt.txt), header=TRUE,stringsAsFactors = F)
      if(opt.txt=="wl.opt"){
        mod.opt<-mod.opt[,c(1,grep(seg,colnames(mod.opt)))]
      }else{
        mod.opt<-mod.opt[,c(1,3)]
      }
      colnames(mod.opt)<-c('JDAY','ELWS')
  }else{
      mod.opt<- read.table(file.path(path,opt.txt), header=TRUE,
                       skip=11,stringsAsFactors = F)
  }
  #str(mod.opt); head(mod.opt)
  #############Get the measured elevations and compare#########
  elvHdr <-readLines(file.path(path,meas.elvs),1)
  if(grepl('\\$',elvHdr)){
    elvs<-read.csv(file.path(path,meas.elvs), header=TRUE,stringsAsFactors = F,skip=2)#[,c(1,3)]
    colnames(elvs)<-c('JDAY','ELWS')
  }else{
    elvs<-gdata::read.fwf(file.path(path,meas.elvs),widths=rep(8,2),skip=3,
    col.names= c('JDAY', 'ForebayEL_m')) #
  }

  #inspect the missing values
  missing.rows<-which(apply(apply(elvs,2,is.na),1,any))
  if(length(missing.rows)>1){
      print('The following rows are missing data and will be removed...')
      print(elvs[missing.rows,])
      #remove missing values
      elvs<-elvs[-missing.rows,]
  }
  elvs<-data.frame(JDAY=mod.opt$JDAY,
                   ForebayEL_m=approx(x=elvs[,1],y=elvs[,2],xout=mod.opt$JDAY)$y)
  #str(elvs); head(elvs)
  if(!is.na(rule.curve.filename)){
   #Read in the reservoir rule curve
   rcurve<- read.csv(file.path(path,rule.curve.filename), header=TRUE,
                     col.names=c('Date','rule'),
                     skip=2,stringsAsFactors = F)
   rcurve$Date<-as.Date(rcurve$Date)
   rcurve$JDAY<-julian(rcurve$Date,origin=as.Date(paste0(as.numeric(format(rcurve$Date[1],'%Y'))-1,'-12-31')))
   rcurve$Date<-NULL
   #convert feet to meters...
   rcurve$rule<-rcurve$rule*0.3048
  }

  ###################compare measured and modeled#################################
  if(!is.na(daystep)){
    # use the daily average of the water elevations to eliminate noise
    print(paste0(daystep,'-Day averaging'))
    compElvs<-data.frame(JDAY=mod.opt$JDAY) #,meas=NA,mod=NA)
    compElvs$mod <- approx(x=mod.opt$JDAY,y=mod.opt$ELWS,xout=compElvs$JDAY)$y
    compElvs$meas <- approx(x=elvs$JDAY,y=elvs$ForebayEL_m,xout=compElvs$JDAY)$y
    compElvs$mod[(nrow(compElvs)-5):nrow(compElvs)] <-
      moving.avg(compElvs[(nrow(compElvs)-5):nrow(compElvs),c('JDAY','mod')],daystep)$smooth
    # alldays<-unique(as.integer(mod.opt$JDAY))
    # ndays<-alldays[seq(1,length(alldays),by=daystep)]
    # compElvs<-data.frame(JDAY=ndays,meas=NA,mod=NA)
    # for(i in 1:length(ndays)){
    #   if(i==1){
    #     compElvs$mod[i]<-mod.opt$ELWS[1]
    #     compElvs$meas[i]<-elvs$ForebayEL_m[1]
    #   }else{
    #     compElvs$mod[i]<-mean(mod.opt$ELWS[as.integer(mod.opt$JDAY)%in%ndays[i]])
    #     compElvs$meas[i]<-mean(elvs$ForebayEL_m[as.integer(elvs$JDAY)%in%ndays[i]])
    #   }
    # }
  }else{
    # Interpolate to the model output timestep
    print('Interpolating to model output timesteps')
    compElvs<-data.frame(JDAY=mod.opt$JDAY,
                         meas=approx(x=elvs$JDAY,y=elvs[,2],xout=mod.opt$JDAY,rule=2)$y,
                         mod=mod.opt[,2])
  }

  # Calcualte fitstats
  fitstats<-w2r::errs(compElvs[,c('meas','mod')])[,c(1,4,5)]
  if(fitstats$MAE>0.75){
    save.plot<-T
  }else{
    save.plot<-T
  }
  #Get data ready for plotting
  compElvsL<-reshape2::melt(compElvs,id.vars = 'JDAY')
  compElvsL$Data<-'Lake_Level_m'
  compElvs$WSELV_fit<-compElvs$meas-compElvs$mod
  compElvsDL<-reshape2::melt(compElvs[,c('JDAY','WSELV_fit')],id.vars = 'JDAY')
  compElvsDL$Data<-'Water_Balance_Difference_m'
  compElvsL<-rbind(compElvsL,compElvsDL)
  compElvs$meas.diff<-c(0,diff(compElvs$meas))
  compElvs$mod.diff<-c(0,diff(compElvs$mod))

  # bq<-quantile(compElvs$meas-compElvs$mod,c(0.01,0.99),na.rm=T)
  # extDays<-unique(round(compElvs$JDAY[(compElvs$meas-compElvs$mod)<bq[1] |
  #                                    (compElvs$meas-compElvs$mod)>bq[2]]))
  #print(summary(extDays))
  #str(extDays)

  ###############read in USACE elevation/volume curve from Pre-Processor ######
  prelns<-readLines(paste0(path,'/pre.opt'))
  vars<-paste0("Waterbody ",wb," Volume-Area-Elevation Table")
  npt.lines<-grep(vars,prelns)+5
  end.lines<-grep(" Layer",prelns[(npt.lines):length(prelns)])[1]
  acoe.crv<-read.table(file.path(path,'pre.opt'),
             skip=npt.lines[1],comment.char = "K",fill=T,
             nrows=end.lines-8,stringsAsFactors = F
             )[,c(2,4)]
  acoe.crv<-as.data.frame(apply(acoe.crv,2,as.numeric))
  acoe.crv<-acoe.crv[!apply(apply(acoe.crv,2,is.na),1,any),]
  acoe.crv[,2]<-acoe.crv[,2]*1E6

  # set up regression for volume as a function of elevation
  #crvmod<-lm('vol.m.3. ~ elev.m.',acoe.crv,);#str(crvmod);summary(crvmod)
  crvmod<-smooth.spline(x=acoe.crv[,1],y=acoe.crv[,2])
  compVol<-data.frame(JDAY=compElvs$JDAY,meas=NA,mod=NA)
  compVol$meas[compElvs$meas!='NaN']<-
    predict(crvmod,x=compElvs$meas[compElvs$meas!='NaN'])$y
  compVol$mod<-predict(crvmod,x=compElvs$mod)$y
  compVol$meas.diff<-c(0,diff(compVol$meas))
  compVol$mod.diff<-c(0,diff(compVol$mod))
  compVol$balance<-compVol$meas-compVol$mod
  compVol$balance.cms<-c(0,diff(compVol$balance))/(c(diff(compVol$JDAY),0)*24*(60^2)) #((c(0,diff(compVol$JDAY))**(60^2),0)
  # Remove extreme values
  watbalance<-compVol[,match(c('JDAY','balance.cms'),colnames(compVol))]
  watbalance<-apply.davg.oncols(compVol[,match(c('JDAY','balance.cms'),
                               colnames(compVol))])
  # apply a two-day moving average
  #watbalance$balance.cms<-moving.avg(watbalance,1)$smooth
  #bq<-quantile(watbalance$balance.cms,c(0.05,0.95),na.rm=T)
  #watbalance$balance.cms[watbalance$balance.cms<bq[1]]<-bq[1]
  #watbalance$balance.cms[watbalance$balance.cms>bq[2]]<-bq[2]
  #bq<-quantile(watbalance$balance.cms,c(0.03,0.97),na.rm=T)

  # write water balance distributed tributary flow file
  if(write.files){
    # Make initial WSELV (bathymetry file) match the first day of the observed WSELV
    modifyInitWSELVbth(path=path,wb=wb,
                       newInitWSELV=elvs[1,2])

    #remaining.days<-(last(watbalance$JDAY)+1):366
    #zeros<-data.frame(JDAY=remaining.days,balance.cms=0)
    #watbalance<-rbind(watbalance,zeros)

    if(!is.null(append.filename)){
      # Set a max threshold elevation difference to ignore modifying the oldwaterbalance
      maxElvDif_m <- 0.5
      newJDAY <- compElvs$JDAY[abs(compElvs$WSELV_fit)>maxElvDif_m][1]
      print(paste0('Adding new water balance (QDT) to ',append.filename, ' beginning on JDAY ',newJDAY))
      new.npt.filename<-append.filename
      watbalHdr <- readLines(file.path(path,append.filename),1)
      # if you want to append an older distributed trib inflow file, specify it here
      if(grepl('\\$',watbalHdr)){
         oldwatbalance<-read.csv(file.path(path,append.filename),skip=3,
           col.names=c('JDAY','Old_Qdt'),stringsAsFactors = F)
      }else{
        oldwatbalance<-read.fwf(file.path(path,append.filename),skip=3,
                              widths=c(8,8),col.names=c('JDAY','Old_Qdt'))
      }
      watbalance$balance.cms[watbalance$JDAY<newJDAY] <- 0
      watbalanceNew<-data.frame(
        JDAY=watbalance$JDAY,
        # Add old watbalance to new
        New_Qdt_Qcms=approx(x=oldwatbalance$JDAY,y=oldwatbalance[,2],
                           xout=watbalance$JDAY)$y +
                           watbalance$balance.cms)
      # Add zeros to missing timesteps
      watbalanceNew[is.na(watbalanceNew$New_Qdt_Qcms),2]<-0
      watbalance<-watbalanceNew; rm(watbalanceNew)
    }else{
      print(paste0('Writing new water balance (QDT) to ',new.npt.filename))
      new.npt.filename<-paste0('WB_',gsub(".csv",new.npt.filename,meas.elvs))
    }
    watbalanceL<-reshape2::melt(watbalance,id.vars = 'JDAY')
    watbalanceL$Data<-'QDT_Flow_cms'
    compElvsL<-rbind(compElvsL,watbalanceL)

    if(exists("oldwatbalance")){
      oldwatbalanceL<-reshape2::melt(oldwatbalance,id.vars = 'JDAY')
      oldwatbalanceL$Data<-'QDT_Flow_cms'
      compElvsL<-rbind(compElvsL,oldwatbalanceL)
    }
    watbalance<-as.data.frame(apply(watbalance,2,round,5))
    if(max(watbalance$JDAY)<366){
      watbalance<-as.data.frame(rbind(watbalance,
                                      watbalance[nrow(watbalance),]))
      watbalance[nrow(watbalance),]<-c(366,0)
    }
    obsceneVals <- abs(watbalance[,2])>1e9
    if(any(obsceneVals)){
      watbalance[obsceneVals,2] <- NA
      watbalance[,2] <- na.approx(watbalance[,2])
    }
    write(paste('$#', new.npt.filename,'Water Balance; distributed tributary (cms) ',
                format(Sys.time(), "%Y-%m-%d_%H:%M")),file.path(path,new.npt.filename))
    write('#',file.path(path,new.npt.filename),append=T)
    write(c(paste('#',toString(colnames(watbalance)))),file.path(path,new.npt.filename),append=T)
    write.table(watbalance, file=file.path(path,new.npt.filename),
                sep=",", na="0",append=T,col.names = F,row.names = F)
  }else{
    print('Water balance (QDT) not written')
  }


  if(save.plot){
    # add fitstats
    compElvsL$Data<-as.factor(compElvsL$Data)
    ann_text <- data.frame(JDAY=1,value=max(compElvs$WSELV_fit)*0.95,
                           lab = toString(paste0(colnames(fitstats),': ',round(fitstats[1,],2))),
                           variable=factor('WSELV_fit',levels=levels(compElvsL$variable)),
                           Data = factor("Water_Balance_Difference_m",levels = levels(compElvsL$Data)))
    p<-ggplot2::ggplot(compElvsL,aes(x=JDAY,y=value,colour=variable)) +
      geom_line(alpha=0.6) +
      geom_text(data = ann_text,label = ann_text$lab,
                hjust   = -0.1,vjust   = 1,
                show.legend = FALSE) +
      facet_grid(Data~.,scales = 'free') +
      theme(legend.position = 'top')
    ggplot2::ggsave(p,filename = file.path(path,paste0('WB_',gsub('.csv','',meas.elvs),
                                              format(Sys.time(), "%Y-%m-%d_%H%M"),'.png')),
           device = 'png',
           width = 9,height=6)
  }
  return(list(compElvs=compElvs,compElvsL=compElvsL,compVol=compVol,fit=fitstats))
}
