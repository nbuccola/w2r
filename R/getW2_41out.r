#' This function will extract simulated CE-QUAL-W2 lake elevation, outflows, inflows, and outflow temperature
#'
#' @param mod.path character; Defaults to reading in the wl.opt file, but can specify a "*.tsr" output from W2
#' @param atu.day numeric; the days in which the accumulated thermal units will begin. Typically c(244, 280, 310)
#' @param tcrit Optional dataframe with critical fish temperatures and days
#' @param modOutDetails Dataframe with columns: Dir, SegmentOutput, and wb columns
#' @param sumOutTable Logical; include the summary table output defaults to TRUE
#' @param davg character vector, typically c('avg','max','min')
#' @return a data.frame of the model to observed fit statistics
#' @author Norman Buccola
#' @keywords CEQUALW2 model output post-processing
#' @examples
#' getW2_41out()
#' @export
getW2_41out<-function(mod.path=w2Dirs[w2ioi],
                      atu.day=atu.day,
                      tcrit=NULL, #Optional dataframe with critical fish temperatures and days
                      modOutDetails=NA, # Dataframe with columns: Dir, SegmentOutput, and wb columns
                      sumOutTable=T,
                      davg=c('avg','max','min')
                      ){
  #source(file.path(wd,'r_functions/apply.davg.oncols.r'))
  ##
  # source('../r_functions/gsub.rec.r')
  # source('../r_functions/extract.qwo-two.r')
  # source('../r_functions/moving.avg.r')
  # source('../r_functions/merge.by.jday.r')
  # source('../r_functions/readW2Selective.r')
  # source('../r_functions/gs.plot.jday.lt.r')
  # source('../r_functions/get.w2.inflows.r')
  # source('../r_functions/read.interp.r')
  # source('../r_functions/calcEmergenceTiming.r')
  # cy<-gsub('cy|CY','',rev(strsplit(rev(strsplit(mod.path,"/")[[1]])[1],'_')[[1]])[1])
  cy<-gsub('cy|CY','',strsplit(rev(strsplit(modOutDetails$Dir,"/")[[1]])[1],'_')[[1]][1])
  #cy<-gsub('cy','',unique(unlist(lapply(strsplit(modOutDetails$Dir,"/"),function(x) x[[1]]))))
  fls<-list.files(mod.path)
  if(any(grepl('dynsplit_selective',fls))){
    # Read in temperature target
    temp.targs<-read.fwf(file.path(mod.path,'dynsplit_selective1.npt'),
                         skip=3,widths=rep(8,2),comment.char='#')
    colnames(temp.targs)<- c("JDAY", "TTarg")
    #Convert temp in ferinheit to celsius
    c2f<-function(x){(x*(9/5))+32}
    f2c<-function(x){(x-32)/(9/5)}
    temp.targs[,-1]<-c2f(temp.targs[,-1])
    str(temp.targs)
    # read in the selective.npt file
    outlets<-readW2Selective(path=mod.path)
    #print(outlets)
  }

  optfls<-fls[grepl('.opt',fls)]
  ################################
  ## Setup reading temperature and flow output
  qwo.textfile<-optfls[grep(paste0('qwo_',modOutDetails$SegmentOutput),optfls)]
  two.textfile<-optfls[grep(paste0('two_',modOutDetails$SegmentOutput),optfls)]
  print(paste0('reading output from',mod.path))
  wo<-get.dam.outflow.mod.output(
        path=mod.path,
        qwo.textfile=qwo.textfile,
        two.textfile=two.textfile,
        seg=modOutDetails$SegmentOutput #tsr.txt=tsr.textfile
        )
  tnm<-'mod.t'
  qnm<-'mod.q|Q'

  if(exists('outlets')){
    floaters.exist<-any(!is.na(outlets$floaters)) & length(outlets$floaters)>1
      if((floaters.exist|any(!unlist(sapply(outlets$power,is.finite)))) &
          !is.null(nrow(outlets$split2))){
        # Summarize by groups
        for(grp in 1:nrow(outlets$split2)){
            wo$Tgrp<-wo$Egrp<-NA
            wo$Qgrp<-0
            grp.i<-wo$JDAY>=outlets$split2[grp,1] & wo$JDAY<=outlets$split2[grp,2]
            if(floaters.exist & !is.na(outlets$floaters[[grp]])){
                qgrp.cols<-match(paste0('Q',outlets$floaters[[grp]]),colnames(wo))
                tgrp.cols<-match(paste0('T',outlets$floaters[[grp]]),colnames(wo))
                egrp.cols<-match(paste0('E',outlets$floaters[[grp]]),colnames(wo))
            }else{
                qgrp.cols<-match(paste0('Q',outlets$power[[grp]]),colnames(wo))
                tgrp.cols<-match(paste0('T',outlets$power[[grp]]),colnames(wo))
                egrp.cols<-match(paste0('E',outlets$power[[grp]]),colnames(wo))
            }
            #browser()

            if(length(qgrp.cols)>1){
                # Flow-weighted average temperature
                wo$Tgrp[grp.i]<-apply(wo[grp.i,qgrp.cols]*wo[grp.i,tgrp.cols],1,sum)/
                  apply(wo[grp.i,qgrp.cols],1,sum)
                wo$Qgrp[grp.i]<-apply(wo[grp.i,qgrp.cols],1,sum)
                wo$Egrp[grp.i]<-apply(wo[grp.i,egrp.cols],1,mean,na.rm=T)
            }else{
                wo$Tgrp[grp.i]<-wo[grp.i,tgrp.cols]
                wo$Qgrp[grp.i]<-wo[grp.i,qgrp.cols]
                if(!is.na(egrp.cols)){
                    wo$Egrp[grp.i]<-wo[grp.i,egrp.cols]
                }
            }
        wo$QgrpPrc[grp.i]<-round((wo$Qgrp[grp.i]/wo$mod.q[grp.i])*100)
        colnames(wo)[match(c('Tgrp','Qgrp','QgrpPrc','Egrp'),colnames(wo))]<-paste0(c('Tgrp','Qgrp','QgrpPrc','Egrp'),grp)
        }
      }
  }
  #print(str(wo))
  wo<-wo[,!apply(apply(wo,2,is.na),2,all)]

  if(any(grepl(qnm,colnames(wo)))){
    qcols<-grepl(paste0('JDAY|',qnm),colnames(wo)) & !grepl('grp',colnames(wo))
    #source(file.path(wd,'r_functions/apply.davg.oncols.r'))
    qout<-apply.davg.oncols(wo[,qcols])
    colnames(qout)[-1]<-paste0('Davg_',colnames(qout)[-1])
  }

  if(any(grepl(tnm,colnames(wo)))){
  ############################################################
  # merge daily avg. temps from each model scenario
  ########################################################
      tcols<-match(c('JDAY','mod.t'),colnames(wo))
      #source(file.path(wd,'r_functions/apply.davg.oncols.r'))
      tout<-apply.davg.oncols(wo[,tcols])
      colnames(tout)[-1]<-paste0('Davg_',colnames(tout)[-1])
      if('min' %in% davg){
        tmin<-apply.davg.oncols(wo[,tcols],calc.min=T)
        colnames(tmin)[-1]<-paste0('Dmin_',colnames(tmin)[-1])
        tout<-merge(tout,tmin,by='JDAY',all=T)
      }
      if('max' %in% davg){
        tmax<-apply.davg.oncols(wo[,tcols],calc.max = T)
        colnames(tmax)[-1]<-paste0('Dmax_',colnames(tmax)[-1])
        tout<-merge(tout,tmax,by='JDAY',all=T)
      }
      ecols<-match(c('JDAY','mod.elv'),colnames(wo))
      eout<-apply.davg.oncols(wo[,ecols])
      colnames(eout)[-1]<-paste0('Davg_',colnames(eout)[-1])

      #str(tout);
      ###############################################
      #Calculate the Accumulated Thermal Units since atu.day of each scenario
      ################################################
      if(!is.null(tcrit)){
           tcrit<-cbind(tcrit,matrix(NA,nrow(tcrit),(ncol(tout)-1)))
           colnames(tcrit)[(ncol(tcrit)-(ncol(tout)-2)):ncol(tcrit)]<-colnames(tout)[-1]
           #calculate the number of days in violation of each criteria for each scenario
           for(cr in which(!is.na(tcrit$crit.op))){
               t.cr<-tout$JDAY>=tcrit$JDAY.min[cr] &
                   tout$JDAY<=tcrit$JDAY.max[cr]
               tcrit.cr<-tcrit$criteria[cr]
               if(tcrit$crit.op[cr]=="<"){
                 # Percent time
                 #tcprcnt<-function(x){length(which(x<tcrit.cr))/length(x)}
                 # Number of Days
                  tcprcnt<-function(x){length(which(x<tcrit.cr))}
               }else{
                  if(tcrit$crit.op[cr]==">"){
                    # Percent time
                    #tcprcnt<-function(x){length(which(x>tcrit.cr))/length(x)}
                    # Number of Days
                    tcprcnt<-function(x){length(which(x>tcrit.cr))}
                  }
               }
               if(ncol(tout)>2){
                   tcrit[cr,match(colnames(tout)[-1],colnames(tcrit))]<-
                       apply(tout[t.cr,-1],2,tcprcnt)
               }else{
                   tcrit[cr,match(colnames(tout)[-1],colnames(tcrit))]<-
                       tcprcnt(tout[t.cr,-1])
               }
           }

          if(any(is.na(atu.day))){
             atu.day<-tcrit$JDAY.min[match("Emergence",tcrit$Life.Stage)]
          }else{
             if(any(grepl("SteelheadEmergence",tcrit$Life.Stage))){
               atu.day.st<-152 #c(91,121)
               tcemSt<-foreach(atu=atu.day.st,.combine="rbind") %do% {
                   tcem<-tcrit[tcrit$Life.Stage=="SteelheadEmergence",]
                   tcem$JDAY.min[match("SteelheadEmergence",tcem$Life.Stage)]<-atu
                   return(tcem)
               }
               tcrit<-tcrit[-grep("SteelheadEmergence",tcrit$Life.Stage),]
               tcrit<-rbind(tcrit,tcemSt)
             }
             tcem<-foreach(atu=atu.day,.combine="rbind") %do% {
                 tcem<-tcrit[tcrit$Life.Stage=="Emergence",]
                 tcem$JDAY.min[match("Emergence",tcem$Life.Stage)]<-atu
                 return(tcem)
             }
             tcrit<-tcrit[-match("Emergence",tcrit$Life.Stage),]
             tcrit<-rbind(tcrit,tcem)
             rm(tcem)
          }
       }# End of percent time above/below critical temperature calculations
       scnCols<-(1:ncol(tout))[-grep('JDAY',colnames(tout))]
       atus<-foreach(atu=atu.day,.combine="rbind") %do% {
               em<-foreach(scnCol=scnCols,.combine='cbind') %do% {
                  calcEmergenceTiming(tout=tout[,c(match('JDAY',colnames(tout)),scnCol)],
                                          atu.day = atu)[[1]]
               }
             em<-data.frame(aday=atu,em,stringsAsFactors=F)
             colnames(em)<-c('aday',colnames(tout)[-1])
             return(em)
       }
       colnames(atus)[-1]<-gsub(tnm,'mmdd',colnames(tout)[-1])

       atu.d<-foreach(atu=atu.day,.combine="rbind") %do% {
         em<-foreach(scnCol=scnCols,.combine='cbind') %do% {
           calcEmergenceTiming(tout=tout[,c(match('JDAY',colnames(tout)),scnCol)],
                               atu.day = atu)[[2]]
         }
         em<-data.frame(aday=atu,em,stringsAsFactors=F)
         colnames(em)<-c('aday',colnames(tout)[-1])
         return(em)
       }
       colnames(atu.d)[-1]<-gsub(tnm,'aday',colnames(tout)[-1])
       atu<-as.data.frame(cbind(atus,atu.d[,-1]),stringsAsFactors=F)
       atu$Year<-cy
       atu$Site<-modOutDetails$RESSIMCode
       atu$Scenario<-modOutDetails$Scenario
       if(exists('tcrit')){
               tcrit[tcrit$Life.Stage=="Emergence",
                     match(c("JDAY.min",gsub('aday',tnm,colnames(atu.d)[-1])),colnames(tcrit))]<-
                     atu.d[atu.d$aday%in%tcrit$JDAY.min[tcrit$Life.Stage%in%"Emergence"],]
               tcrit$Year<-cy
               tcrit$Site<-modOutDetails$RESSIMCode
               tcrit$Scenario<-modOutDetails$Scenario
       }
    } # End of mod.t (temperature calcs)

    if(sumOutTable){
        # Create a long format dataframe for plotting
        ao<-merge(tout,merge(eout,qout,by='JDAY',all=T),by='JDAY',all=T)
        #print(colnames(ao))
        ao$Date<-as.Date(ao$JDAY,origin="2008-12-31")
        colnames(ao)<-gsub(tnm,'Tout',colnames(ao))
        colnames(tcrit)<-gsub(tnm,'Tout',colnames(tcrit))
        #print(str(ao))
        # Add columns with temperature difference
        if(exists('temp.targs')){
             ao<-merge(ao,temp.targs,by='JDAY',all=T)
             aot<-ao[,grep('Tout',colnames(ao))]-ao[,'TTarg']
             colnames(aot)<-gsub('Tout','Tdif',colnames(aot))
             ao<-cbind(ao,aot)
        }
        #whichCols<-!grepl('JDAY',colnames(ao))

        aoL<-reshape2::melt(ao,id.vars=c('Date','JDAY'))
        aoL$Year<-cy
        aoL$Site<-modOutDetails$RESSIMCode
        aoL$Scenario<-modOutDetails$Scenario
    } # end of sumOutTable
    if(exists('aoL')){
        x<-list(ao=aoL)
    }
    if(exists('atu')){
        x<-list(atu=atu)
        #names(x)<-scenario
    }
    if(exists('tcrit')){
        x<-list(tcrit=tcrit)
        #names(x)<-scenario
    }
    if(exists('aoL') & exists('atu')){
        x<-list(ao=aoL,atu=atu)
    }
    if(exists('aoL') & exists('atu') & exists('tcrit')){
        x<-list(ao=aoL,tcrit=tcrit,atu=atu)
    }
    if(exists('x')){
        #print(str(x))
        return(x)
    }
}
