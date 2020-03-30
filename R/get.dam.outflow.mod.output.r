###############################################################################
# Get model output for the outflow at the dam
get.dam.outflow.mod.output<-
  function(qwo.textfile=NA,#'qwo_25',
           seg=NA,#only need tsr.txt if you need WSELV #tsr.txt=NA,
           path='/W2_mods/Detroit_Lake_2010_00-01',metric=F,
           two.textfile=NA,#'two_25',
           str.txt=NA,#'str_br1',
           withdrawals=0,
           const='ELWS'
  )
    # withdrawals=1:10 or the NWD value in w2_con.npt; withdrawals=0 is a special case
  {
    #setwd(path)
    unique.rows<-function(x){x[match(unique(x$JDAY),x$JDAY),]}
    if(!is.na(qwo.textfile)){
      firstLine<-readLines(paste0(path,'/',qwo.textfile),4)[4]
      #print(firstLine)
      if(any(grepl(",",firstLine))){
        qwo<-read.csv(paste0(path,'/',qwo.textfile),
                      header =F,
                      stringsAsFactors = F,skip=3)
        qwo<-qwo[,!apply(apply(qwo,2,is.na),2,all)]
      }else{
        qwo<-read.table(paste0(path,'/',qwo.textfile),skip=3)
      }
      colnames(qwo)<-c('JDAY','mod.q',paste0("Q",1:(ncol(qwo)-2)))[1:ncol(qwo)];
      if(!metric){qwo[,-1]<-qwo[,-1]* 35.31466688# convert to cfs for plotting!!
      }
      qwo<-unique.rows(qwo)
    }
    if(!is.na(two.textfile)){

      if(any(grepl(",",firstLine))){
        two<-read.csv(paste0(path,'/',two.textfile),
                      header =F,na.strings=c("-99.00"),
                      stringsAsFactors = F,skip=3)
        two<-as.data.frame(apply(two,2,function(x){
                                  x[x==-99 | x==0]<-NA
                                  return(x)
                                  }))
      }else{
        two<-read.table(paste0(path,'/',two.textfile),skip=3,
                        na.strings=c("0.00","-99.00"))
      }
      colnames(two)<-c('JDAY','mod.t',paste0("T",1:(ncol(two)-2)))[1:ncol(two)];
      two<-two[,!apply(apply(two,2,is.na),2,all)]
      two<-unique.rows(two)
      if(!metric){  two[,-1]<-two[,-1]*(9/5)+32 # convert to F for plotting!!
      }
    }
    #firstLine<-readLines(paste0(path,'/',tsr.txt),1)[1]
    #print(firstLine)
    #if(any(grepl(",",substr(firstLine,1,5)))){
    wl<-read.csv(file.path(path,'wl.opt'),header =T,
                  stringsAsFactors = F)
    wl<-wl[,c(1,unlist(sapply(seg,grep,colnames(wl))))] #[,c(1,3)]
    colnames(wl)[2]<-'mod.elv'
    wl<-unique.rows(wl)#[,match(c('JDAY','ELWS'),colnames(wl))]);rm(wl)
    if(!metric){
      if(grepl('ELWS',const)){
        wl[,'mod.elv']<-wl[,'mod.elv']/0.3048 # convert meters to feet for plotting!!
      }
      if(grepl('T2',const)){
        wl[,'mod.t']<-wl[,'mod.t']*(9/5)+32 # convert to F for plotting!!
      }
      if(grepl('Q',const)){
        wl[,'mod.q']<-wl[,'mod.q']* 35.31466688 # convert to cfs for plotting!!
      }
    }
    if(!is.null(nrow(withdrawals))){
      withdrawals<-withdrawals[1,]
    }
    if(!is.na(str.txt)){
      strs<-read.table(paste0(path,'/',str.txt),
                       header =F,skip=2,
                       stringsAsFactors = F,
                       col.names=c('JDAY',paste0('T',withdrawals),
                                   paste0('Q',withdrawals),paste0('E',withdrawals))
      )
      strs<-unique.rows(strs[,unlist(sapply(c('JDAY','E'),grep,colnames(strs)))])
      if(!metric){strs[,-1]<-strs[,-1]/0.3048 # convert meters to feet for plotting!!
      }
    }

    if(!is.na(qwo.textfile)){

      #MERGE THE TWO AND QWO FILES!!!
      # find the missing days
      opt<-merge.interp.24hr(x1=qwo,x2=two)
      # remove all columns with no flow
      zeroCols<-which(apply(apply(opt,2,function(x)x==0|is.na(x)),2,all))
      if(any(zeroCols!=0)){
        zeroCols<-unique(substr(colnames(opt)[zeroCols],2,3))
        zeroCols.j<-unique(unlist(sapply(zeroCols,grep,colnames(opt))))
        print(paste("Removing Zero Column Flows:",
                    toString(colnames(opt)[zeroCols.j])))
        opt<-opt[,-zeroCols.j]
      }

    }
    if(!is.na(str.txt)){
      if(any(zeroCols!=0)){
        opt<-merge.interp.24hr(opt,strs[,-unlist(sapply(zeroCols,grep,colnames(strs)))])
      }else{
        opt<-merge.interp.24hr(opt,strs)
      }
      # Create missing data where/when an outlet has no flow
      qcols<-unlist(sapply(c('q','Q'),grep,colnames(opt)))
      which.na<-function(x){x==0|is.na(x)}
      for(j in 1:length(qcols)){
        if(colnames(opt)[qcols[j]]=='mod.q'){
          jc<-match('mod.t',colnames(opt))
        }else{
          jc<-grep(substr(colnames(opt)[qcols][j],2,3),colnames(opt))
          jc<-jc[!grepl('Q',colnames(opt)[jc])]
        }
        NA.j<-which.na(opt[,qcols[j]])
        opt[NA.j,jc]<-NA
      }
    }

    # Add the lake elevation data if present
    if(!is.na(qwo.textfile) & exists('wl')){
      opt$mod.elv<-approx(wl$JDAY,wl[,match('mod.elv',colnames(wl))],xout=opt$JDAY)$y
      # for(cn in 1:length(const)){
      #   #opt$cons<-approx(elv$JDAY,elv[,match(const[cn],colnames(elv))],xout=opt$JDAY)$y
      #   opt$cons<-approx(tsr$JDAY,tsr[,match(const[cn],colnames(tsr))],xout=opt$JDAY)$y
      #   colnames(opt)<-gsub('cons',const[cn],colnames(opt))
      # }
    }else{
      if(exists('wl')){
        opt<-wl
      }
    }
    #if(length(jday.NA)>0){
    #    opt2<-opt[opt$JDAY!=jday.NA,]
    #}
    #print(str(opt))
    #str(opt);head(opt)
    #setwd(wd)
    return(opt)
  }
