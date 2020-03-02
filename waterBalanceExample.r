#devtools::install('C:/Users/g2echnb9/Documents/R/w2r')
library(w2r)
projFldr<-''
scenarios<-data.frame(NAA='HCR_LOP---0',stringsAsFactors = F)
# Read in RES-SIM to W2 codes
RessimCodes<-read.csv(system.file("extdata", "WillW2OutputNmsSeg.csv", package = "w2r"),
                      stringsAsFactors = F)

# Water Balance steps:
# 1) Run the W2 models a first time with QDT set to OFF. 
# 2) Run the following code chunk to calculate the missing flow needed (QDT) to meet the operational lake levels and close the water balance for each reservoir. Use the WillW2_HCR-LOP_IRRM_2020_watbalSwitch.csv file to control which reservoir, year, and scenario needs a new QDT (re-)written. 
# 3) Re-Run W2 model with QDT set to ON and repeat step 2. 
# 4) Repeat steps 2 and 3 as needed. In the MF Willamette, only one iteration was needed.

# Control whether or not to perform the water balance in this csv file:
watbalSwitches<-read.csv(system.file("extdata",'WillW2_HCR-LOP_IRRM_2020_watbalSwitchv2.csv',
                                     package = "w2r"),
                         stringsAsFactors = F,header=T)

w2io<-as.list(rep(NA,ncol(watbalSwitches)-2)) 
names(w2io)<-colnames(watbalSwitches)[-c(1,ncol(watbalSwitches))]
firstRun<-F # True will change QDT to "OFF" in the control file
library(foreach)
for(sc in 1:length(scenarios)){
  sc=1
  w2Path<-file.path(system.file("extdata",package = "w2r"),names(scenarios)[sc])
  w2Dirs<-list.dirs(w2Path,recursive=F)
  w2DirsShrt<-list.dirs(w2Path,recursive=F,full.names = F)
  scModWhich<-which(!is.na(watbalSwitches[,sc+1]))
  if(!any(scModWhich)){next}
  scMods<-sort(unlist(sapply(RessimCodes$W2name[scModWhich],grep,w2DirsShrt)))
  scModsDf<-foreach(m= 1:length(scMods),.combine='rbind') %do% {
    mm<-data.frame(RESSIMCode=NA,Dir=w2DirsShrt[scMods][m],stringsAsFactors = F)
    if(grepl('lop',mm$Dir) & m>1 && grepl('lop',w2DirsShrt[scMods][m-1])){
      mm$RESSIMCode<-'DEX'
    }else{
      if(grepl('lop',mm$Dir)){
        mm$RESSIMCode<-'LOP'
      }else{
        mm$RESSIMCode<-RessimCodes$RESSIMCode[grep(strsplit(mm$Dir,'_')[[1]][2],RessimCodes$W2name)]
      }
    }
    mm$SegmentOutput<-RessimCodes$SegmentOutput[RessimCodes$RESSIMCode==mm$RESSIMCode]
    mm$wb<-RessimCodes$wb[RessimCodes$RESSIMCode==mm$RESSIMCode]
    return(mm)  
  }
  w2io[[sc]]<-as.list(w2DirsShrt[scMods]) #rep(NA,length(scMods)))
  names(w2io[[sc]])<-w2DirsShrt[scMods]
  
  opt2keep<-c('pre.opt',paste0("_seg",unique(scModsDf$SegmentOutput),'.opt'), #'wl.opt',
            paste0("qwo_",unique(scModsDf$SegmentOutput),'.opt'))

  for(i in 1:length(scMods)){ 
    print(scModsDf[i,])
    mod<-scModsDf$RESSIMCode[i]
    wtbli<-grepl(mod,watbalSwitches$RESSIMCode) & 
      grepl(substr(gsub('cy','',scModsDf$Dir[i]),0,4),watbalSwitches$Year)
    if(firstRun & watbalSwitches[wtbli,sc+1]){
      # Change QDT to "OFF"
      qdt<-'OFF'
    }else{
      qdt<-'ON'
    }
    #source("C:/Users/g2echnb9/Documents/R/w2r/R/modifyW2con.r")
    modifyW2con(path=file.path(w2Path,scModsDf$Dir[i]),
                seg=scModsDf$SegmentOutput[i],
                qdt=qdt,
                wb=scModsDf$wb[i])
    #source("C:/Users/g2echnb9/Documents/R/w2r/R/readW2ConInOut.r")
    #source("C:/Users/g2echnb9/Documents/R/w2r/R/waterBalanceGeneric.r")
    w2io[[sc]][[i]]<-readW2ConInOut(path=file.path(w2Path,scModsDf$Dir[i]), 
                          write.files=watbalSwitches[wtbli,sc+1],
                          seg=scModsDf$SegmentOutput[i],
                          wb=scModsDf$wb[i],
                          RESSIMCode=RessimCodes$ReachName[grep(scModsDf$RESSIMCode[i],RessimCodes$RESSIMCode)],
                          wd=wd)
    if(watbalSwitches[wtbli,sc+1]){
      # Remove old model output
      optfls<-list.files(file.path(w2Path,scModsDf$Dir[i]),pattern='.opt',full.names = T)
      opt2keepi<-opt2keep[!grepl(scModsDf$SegmentOutput[i],opt2keep)]
      optfls<-optfls[!grepl(paste(opt2keepi,collapse = '|'),optfls)]
      for(fl in 1:length(optfls)){
          print(paste('Deleting', optfls[fl]))
          file.remove(optfls[fl])
      }
    }

    wbFitSc<-data.frame(Scenario=names(scenarios)[sc],
                        Year=watbalSwitches$Year[wtbli],
                        RESSIMCode=scModsDf$RESSIMCode[i],
                        w2io[[sc]][[i]]$watbal$fit)
    
    if(exists('wbFits')){
      wbFits<-rbind(wbFits,wbFitSc)
    }else{
      wbFits<-wbFitSc
    }
  } # end of i loop for each model
} # end of sc loop for each scenario folder

w2io[[sc]][[i]][['watbal']][['fit']]
p<-ggplot2::ggplot(w2io[[sc]][[i]][['watbal']][['compElvsL']],
                   aes(x=JDAY,y=value,colour=variable)) +
  geom_line(alpha=0.6) +
  facet_grid(Data~.,scales = 'free') +
  theme(legend.position = 'top')
p
