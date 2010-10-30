library(tcltk)
library(tcltk2)
library(MASS)
## library(RODBC)
## source("mixsep.R")
## source("plot.R")
## source("J2.R")
## source("J3.R")
## source("functions.R")

mixsep <- function(){
  killR <- tclVar("")
  font14bf <- tkfont.create(family = "helvetica", size = 14, weight = "bold")
  font9bf <- tkfont.create(family = "helvetica", size = 9, weight = "bold")
  font9bful <- tkfont.create(family = "helvetica", size = 9, weight = "bold",underline="true")
  font9 <- tkfont.create(family = "helvetica", size = 9, weight = "normal")
  font7 <- tkfont.create(family = "helvetica", size = 7, weight = "normal")
  font8 <- tkfont.create(family = "helvetica", size = 8, weight = "normal") 
  font9ul <- tkfont.create(family = "helvetica", size = 9, weight = "normal",underline="true")
  font9itul <- tkfont.create(family = "helvetica", size = 9, slant = "italic", weight = "normal",underline="true")
  font9it <- tkfont.create(family = "helvetica", size = 9, slant = "italic")
  font9bfit <- tkfont.create(family = "helvetica", size = 9, weight="bold", slant = "italic")
  ## Help functions
  getMultipath <- function(x){
    if(length(grep("\\{",x))==0) return(dirname(unlist(strsplit(x," "))[1]))
    x <- unlist(strsplit(x,"\\{"))
    x <- x[x!=""][1]
    x <- unlist(strsplit(x,"\\}"))
    dirname(x[x!=""][1])
  }
  getFile <- function(mult=FALSE)  {
    nameInput <- tclvalue(tkgetOpenFile(parent=msmain,initialdir=tclvalue(path),multiple="true",
                                        filetypes="{{CSV Files} {.csv .txt}} {{Tab-delimited Files} {.tab}}"))
    if(nameInput=="") return(NULL)
    if(exists("mixsep.data",envir=.GlobalEnv)) data <- get("mixsep.data",envir=.GlobalEnv)
    else data <- list()
    mpath <- getMultipath(nameInput)
    tclvalue(path) <<- mpath
    wsNames <- unlist(strsplit(nameInput," "))
    if(length(wsNames)==1) filenames <- nameInput
    else{
      nameBlocks <- c(grep("\\{",wsNames),grep("\\}",wsNames))
      if(length(nameBlocks)==0) filenames <- wsNames
      else{
        nameBlocks <- sort(nameBlocks)
        Blocks <- list()
        for(i in seq(from=1,by=2,len=length(nameBlocks)/2)) Blocks <- c(Blocks,list(nameBlocks[i]:nameBlocks[i+1]))
        filenames <- wsNames[-unlist(Blocks)]
        for(i in 1:length(Blocks)) filenames <- c(filenames,gsub("\\{","",gsub("\\}","",paste(wsNames[Blocks[[i]]],collapse=" "))))
      }
    }
    name <- character(length(filenames))
    for(i in 1:length(filenames)){
      filetmp <- filenames[i]
      filetype <- rev(unlist(strsplit(filetmp,"\\.")))[1]
      if(filetype=="csv" | filetype=="txt"){
        dat <- read.csv(filenames[i],header=TRUE)
        if(ncol(dat)==1) dat <- read.csv2(filenames[i],header=TRUE)
      }
      else if(filetype=="tab") dat <- read.delim(filenames[i],header=TRUE)
      name[i] <- basename(filenames[i])
      if(mult){ ## multi-sample file(s)
        multSample <- tktoplevel()
        tcl("tk_setPalette","gray93") 
        posmsmain <- c(tclvalue(tkwinfo("x",msmain)),tclvalue(tkwinfo("y",msmain)))
        tkwm.geometry(multSample,paste("+",paste(posmsmain,collapse="+"),sep=""))
        tkwm.geometry(multSample,"")
        tkwm.title(multSample, "Select column containing 'sample name' information")
        multWait <- tclVar("")
        tkbind(multSample,"<Destroy>",function(){tclvalue(multWait) <- "cancel"})
        topFrame <- tkframe(multSample)
        dataFrame <- tkframe(multSample)
        butFrame <- tkframe(multSample)
        tkgrid(tklabel(topFrame,text="Select column containing 'sample name' information"))
        tkgrid(topFrame)
        tkgrid(tklabel(dataFrame,text="Sample name column:"),column=0,row=1,sticky="e")
        for(k in 1:ncol(dat)){
          tkgrid(tkcheckbutton(dataFrame,variable=paste(name[i],names(dat)[k],sep=":")),column=k,row=1,sticky="w")  
          tkgrid(tklabel(dataFrame,text=names(dat)[k]),column=k,row=2)
          for(j in 1:5) tkgrid(tklabel(dataFrame,text=paste(dat[j,k])),column=k,row=2+j)
        }
        tkgrid(dataFrame)
        tkgrid(tklabel(multSample,text=""),row=j+3)
        sample.button <- tkbutton(butFrame,text="Select sample column",command=function(){ tkdestroy(multSample); tclvalue(multWait) <- "ok"})
        cancel.button <- tkbutton(butFrame,text="Cancel",command=function(){ tkdestroy(multSample); tclvalue(multWait) <- "cancel"})
        tkgrid(sample.button,tklabel(butFrame,text="   "),cancel.button)
        tkgrid(butFrame)
        tkwait.variable(multWait)
        if(paste(tclvalue(multWait))=="cancel") return(NULL)
        multiVar <- numeric(0)
        for(k in 1:ncol(dat)){
          if(tclvalue(paste(name[i],names(dat)[k],sep=":"))=="1") multiVar <- c(multiVar,k)
        }
        Samples <- split(dat,do.call("paste",c(x=dat[,multiVar,drop=FALSE],sep=":")))
        for(k in 1:length(Samples)){
          if(!is.element(names(Samples)[k],names(data))){
            data <- c(list(tmpname=list(data=Samples[[k]],result=NA)),data)
            names(data)[1] <- paste(name[i],names(Samples)[k],sep=":")
            tkinsert(caselist,0,paste(name[i],names(Samples)[k],sep=":"))
          }
        }
      }
      else{ ## single sample file(s)
        if(!is.element(name[i],names(data))){
          data <- c(list(tmpname=list(data=dat,result=NA)),data)
          names(data)[1] <- name[i]
          tkinsert(caselist,0,name[i])
        }
      }
    }
    assign("mixsep.data",data,envir=.GlobalEnv)
    tkselection.clear(caselist,1,length(data)-1)
    tkselection.set(caselist,0)
  }
  removeFile <- function(){
    cases <- names(get("mixsep.data",envir=.GlobalEnv))
    caseSelection <- as.numeric(tkcurselection(caselist))+1
    caseChoice <- cases[caseSelection]
    if(length(caseChoice)==0){
      tkmessageBox(title="No file selected",message="No file selected",icon="error",type="ok")
      return(NULL)
    }
    else if(length(caseChoice)==1){
      singleton <- match(tclvalue(dataid),caseChoice,NA)
      if(!is.na(singleton)){
        deleteActive <- tkmessageBox(title="Deleting the active file",message="You will delete the active file currently under analysis",
                                     icon="error",type="okcancel")
        if(tclvalue(deleteActive)=="ok"){
          tkdelete(caselist,0)
          assign("mixsep.data",list(),envir=.GlobalEnv)
          ## Reset control variables
          tclvalue(dataid) <<- ""
          tclvalue(colselected) <<- ""
          tclvalue(locusCol) <<- ""
          tclvalue(alleleCol) <<- ""
          tclvalue(heightCol) <<- ""
          tclvalue(areaCol) <<- ""
          tclvalue(pars) <<- ""
          tclvalue(res) <<- ""
          tclvalue(estTau) <<- ""
          tclvalue(estAlpha) <<- ""
          tclvalue(estAlpha2) <<- ""
          tclvalue(dropLocus) <<- ""
          tclvalue(known1Set) <<- 0
          tclvalue(known2Set) <<- 0
          tclvalue(known3Set) <<- 0
          tclvalue(plotselect) <<- ""
          TAB2()
          TAB3()
          TAB4()
        }
      }
      else{ tkdelete(caselist,0)
            assign("mixsep.data",list(),envir=.GlobalEnv)
          }
    }
    else{
      rmList <- caseChoice
      activeCase <- match(tclvalue(dataid),caseChoice,NA)
      if(!is.na(activeCase)){
        rmActive <- rmList[activeCase]
        rmList <- rmList[-activeCase]
        caseActive <- caseSelection[activeCase]
        caseSelection <- caseSelection[-activeCase]
      }
      data <- get("mixsep.data",envir=.GlobalEnv)
      for(i in length(rmList):1){
        data <- data[names(data)!=rmList[i]]
        tkdelete(caselist,caseSelection[i]-1)
      }
      assign("mixsep.data",data,envir=.GlobalEnv)
      if(!is.na(activeCase)){
        deleteActive <- tclVar("")
        deleteActive <- tkmessageBox(title="Deleting the active file",message="You will delete the active file currently under analysis",
                                     icon="error",type="okcancel")
        if(tclvalue(deleteActive)=="ok"){
          data <- data[names(data)!=rmActive]
          tkdelete(caselist,caseActive-1)
          assign("mixsep.data",data,envir=.GlobalEnv)
          ## Reset control variables
          tclvalue(dataid) <<- ""
          tclvalue(colselected) <<- ""
          tclvalue(locusCol) <<- ""
          tclvalue(alleleCol) <<- ""
          tclvalue(heightCol) <<- ""
          tclvalue(areaCol) <<- ""
          tclvalue(pars) <<- ""
          tclvalue(res) <<- ""
          tclvalue(estTau) <<- ""
          tclvalue(estAlpha) <<- ""
          tclvalue(estAlpha2) <<- ""
          tclvalue(dropLocus) <<- ""
          tclvalue(known1Set) <<- 0
          tclvalue(known2Set) <<- 0
          tclvalue(known3Set) <<- 0
          tclvalue(plotselect) <<- ""
          TAB2()
          TAB3()
          TAB4()
        }
        else return(NULL)
      }
    }
  }
  openAnalysis <- function(){  
    if(!exists("mixsep.data",env=.GlobalEnv)){
      tkmessageBox(title="No data files loaded",message="No data files loaded!",icon="error",type="ok")
      return(NULL)
    }
    msdata <- get("mixsep.data",envir=.GlobalEnv)
    cases <- names(msdata)
    if(length(as.numeric(tkcurselection(caselist)))>1){
      tkmessageBox(title="Multiple files selected",message="Only one file can be analysed at the time",icon="error",type="ok")
      return(NULL)
    }
    tclvalue(dataid) <<- cases[as.numeric(tkcurselection(caselist))+1]
    ## Reset control variables
    tclvalue(colselected) <<- ""
    tclvalue(locusCol) <<- ""
    tclvalue(alleleCol) <<- ""
    tclvalue(heightCol) <<- ""
    tclvalue(areaCol) <<- ""
    tclvalue(pars) <<- ""
    tclvalue(res) <<- ""
    tclvalue(estTau) <<- ""
    tclvalue(estAlpha) <<- ""
    tclvalue(estAlpha2) <<- ""
    tclvalue(dropLocus) <<- ""
    tclvalue(known1Set) <<- 0
    tclvalue(known2Set) <<- 0
    tclvalue(known3Set) <<- 0
    tclvalue(plotselect) <<- ""
    TAB2()
    TAB3()
    TAB4()
    tk2notetab.select(tabwin,"Data")
  }
  killMs <- function(){
    kill <- tkmessageBox(title="Terminate mixsep and R",message="This terminates both the mixture separator and R",icon="error",type="okcancel",default="ok")
    if(tclvalue(kill)=="ok"){
      tclvalue(killR) <<- "ok"
      tkdestroy(msmain)
      q("no")
    }
  }
  colSelected <- function(){
    locus <- as.numeric(paste(tclvalue(locusCol)))
    allele <- as.numeric(paste(tclvalue(alleleCol)))
    height <- as.numeric(paste(tclvalue(heightCol)))
    area <- as.numeric(paste(tclvalue(areaCol)))
    if(all(is.na(c(height,area))) | all(c(height,area)<1)){
      tkmessageBox(title="Area and height missing",message="At least one column containing peak height or area need to be speficified",icon="error",type="ok")
      return(NULL)
    }
    else if(is.na(height) | (height<1)){
      msdata <- get("mixsep.data",env=.GlobalEnv)
      msdata[[tclvalue(dataid)]]$data$height <- msdata[[tclvalue(dataid)]]$data[,area]/10
      height <- ncol(msdata[[tclvalue(dataid)]]$data)
      assign("mixsep.data",msdata,env=.GlobalEnv)
    }
    else if(is.na(area) | (area<1)){
      msdata <- get("mixsep.data",env=.GlobalEnv)
      msdata[[tclvalue(dataid)]]$data$area <- msdata[[tclvalue(dataid)]]$data[,height]*10
      area <- ncol(msdata[[tclvalue(dataid)]]$data)
      assign("mixsep.data",msdata,env=.GlobalEnv)
    }
    if(any(is.na(c(locus,allele,height,area))) | any(c(locus,allele,height,area)<1)){
      tkmessageBox(title="Column missing",message="One or more of the mandatory columns are missing",icon="error",type="ok")
      return(NULL)
    }
    tclvalue(colselected) <- paste(c(locus,allele,height,area),sep=",",collapse=",")
    TAB2()
    TAB3()
  }
  resetData <- function(){
    tclvalue(colselected) <- ""
    TAB2()
  }
  callMixsep <- function(){
    tkconfigure(msmain,cursor="watch")
    tcl("update","idletasks")
    cols <- as.numeric(unlist(strsplit(tclvalue(colselected),",")))
    msdata <- get("mixsep.data",envir=.GlobalEnv)
    data <- msdata[[paste(tclvalue(dataid))]]$data
    data <- data[,cols]
    names(data) <- c("locus","allele","height","area")
    data <- convertTab(data)
    m <- as.numeric(tclvalue(noContrib))
    locusorder <- unique(paste(data$locus))
    ## REGARDING SUSPECTS:
    if(any(fProfs <- (1:3)[c(tclvalue(known1Set)=="1",tclvalue(known2Set)=="1",tclvalue(known3Set)=="1")])){
      nFixed <- length(fProfs)
      if(nFixed>m){
        tkconfigure(msmain,cursor="arrow")
        tcl("update","idletasks")
        tkmessageBox(message="Too many fixed profiles specified compared to the number of contributors",icon="error",type="ok")
        return(NULL)
      }
      datas <- split(data,data$locus)
      known1Profile <- replicate(length(datas),list(),simplify=FALSE)
      names(known1Profile) <- locusorder
      for(s in names(known1Profile)){
        for(i in 1:nrow(datas[[s]])){
          if(tclvalue(paste(tclvalue(dataid),"known1",s,datas[[s]]$allele[i],sep=":"))=="1") known1Profile[[s]] <- c(known1Profile[[s]],datas[[s]]$allele[i])
        }
      }
      known2Profile <- replicate(length(datas),list(),simplify=FALSE)
      names(known2Profile) <- locusorder
      for(s in names(known2Profile)){
        for(i in 1:nrow(datas[[s]])){
          if(tclvalue(paste(tclvalue(dataid),"known2",s,datas[[s]]$allele[i],sep=":"))=="1") known2Profile[[s]] <- c(known2Profile[[s]],datas[[s]]$allele[i])
        }
      }
      known3Profile <- replicate(length(datas),list(),simplify=FALSE)
      names(known3Profile) <- locusorder
      for(s in names(known3Profile)){
        for(i in 1:nrow(datas[[s]])){
          if(tclvalue(paste(tclvalue(dataid),"known3",s,datas[[s]]$allele[i],sep=":"))=="1") known3Profile[[s]] <- c(known3Profile[[s]],datas[[s]]$allele[i])
        }
      }
      knownProfile <- list()
      for(i in fProfs){
        knownProfile <- c(knownProfile,list(get(paste("known",i,"Profile",sep=""))))
        if(is.null(unlist(knownProfile[[length(knownProfile)]]))){
          tkconfigure(msmain,cursor="arrow")
          tcl("update","idletasks")
          tkmessageBox(title="Empty profile",message=paste("Fixed Profile",i,"is empty"),type="ok",icon="error")
          return(NULL)
        }
      }
      names(knownProfile) <- paste("Profile",(1:m)[fProfs])
      res <- mixturesep(data,m=m,trace=FALSE,dropLocus=(paste(tclvalue(dropLoci))=="1"),alternatives=(paste(tclvalue(searchalt))=="1"),
                        p=as.numeric(paste(tclvalue(altp))),fixedProfiles=knownProfile,recur=TRUE,gui=TRUE)
    }
    ##    
    else res <- mixturesep(data,m=m,trace=FALSE,dropLocus=(paste(tclvalue(dropLoci))=="1"),
                           alternatives=(paste(tclvalue(searchalt))=="1"),
                           p=as.numeric(paste(tclvalue(altp))),gui=TRUE)
    if(is.null(res)){
      tkconfigure(msmain,cursor="arrow")
      tcl("update","idletasks")
      return(NULL)
    }
    tclvalue(estAlpha) <<- res$stats[1]
    if(length(res$stats)==3){
      tclvalue(estAlpha2) <<- res$stats[2]
      tclvalue(estTau) <<- res$stats[3]
    }
    else tclvalue(estTau) <<- res$stats[2]
    if(paste(tclvalue(dropLoci))=="1"){
      tclvalue(dropLocus) <<- paste(res$dropLoci,collapse=",",sep=",")
    }
    else dropLocus <<- tclVar("")
    msdata[[paste(tclvalue(dataid))]]$result <- res
    assign("mixsep.data",msdata,envir=.GlobalEnv)
    tclvalue(res) <<- "ok"
    tclvalue(pars) <<- "ok"
    TAB4()
    tkconfigure(msmain,cursor="arrow")
    tcl("update","idletasks")
    tk2notetab.select(tabwin,"Results")
  }
  plotEpg <- function(){
    cols <- as.numeric(unlist(strsplit(tclvalue(colselected),",")))
    msdata <- get("mixsep.data",envir=.GlobalEnv)
    data <- msdata[[paste(tclvalue(dataid))]]$data
    data <- data[,cols]
    names(data) <- c("locus","allele","height","area")
    loci <- unique(paste(data$locus))
    plotCombo <- rep(0,length(loci))
    plottedProfiles <- character(length(loci))
    for(i in 1:length(loci)){
      if(paste(tclvalue(paste(tclvalue(dataid),loci[i],sep=":")))=="A"){
        if(tclvalue(tkcurselection(LOCI[[i]]))==""){
          tkmessageBox(title="Missing specification of alternative",
                       message=paste("Missing specification of alternative in locus ",loci[i],sep=""),icon="error",type="ok")
          return(NULL)
        }
        else{
          plotCombo[i] <- as.numeric(tkcurselection(LOCI[[i]]))+1
          plottedProfiles[i] <- msdata[[paste(tclvalue(dataid))]]$result$alternatives[plotCombo[i],i]
        }
      }
      else{
        plotCombo[i] <- as.numeric(tclvalue(paste(tclvalue(dataid),loci[i],sep=":")))
        plottedProfiles[i] <- paste(msdata[[paste(tclvalue(dataid))]]$result$profiles[,i],collapse="/")
      }
    }
    if(all(plotCombo==0)){
      expArea <- msdata[[paste(tclvalue(dataid))]]$result$expectedAreas
      tclvalue(estAlpha) <<- msdata[[paste(tclvalue(dataid))]]$result$stats[1]
      if(length(msdata[[paste(tclvalue(dataid))]]$result$stats)==3){
        tclvalue(estAlpha2) <<- msdata[[paste(tclvalue(dataid))]]$result$stats[2]
        tclvalue(estTau) <<- msdata[[paste(tclvalue(dataid))]]$result$stats[3]
      }
      else tclvalue(estTau) <<- msdata[[paste(tclvalue(dataid))]]$result$stats[2]
    }
    else{
      expArea <- computeExpArea(msdata[[paste(tclvalue(dataid))]],cols,plotCombo)
      tclvalue(estAlpha) <<- round(expArea$alpha[1],4)
      if(length(expArea$alpha)==2) tclvalue(estAlpha2) <<- round(expArea$alpha[2],4)
      tclvalue(estTau) <<- round(expArea$tau,4)
      expArea <- expArea$data
    }
    tclvalue(plotselect) <<- paste(plotCombo,sep=",",collapse=",")
    if(tclvalue(newPlot)==1){ if(capabilities("X11")) x11() else windows() }
    data <- merge(data,expArea,by=c("locus","allele"))
    if(length(grep("null",names(dev.cur())))>0){
      if(capabilities("X11")) x11() else windows() 
    }
    names(plottedProfiles) <- loci
    plotEPG(data,addProfile=(tclvalue(addProfile)==1),profiles=plottedProfiles)
  }
  exportResult <- function(){
    result <- get("mixsep.data",envir=.GlobalEnv)[[paste(tclvalue(dataid))]]$result
    res.prof <- apply(result$profiles,2,paste,collapse="/")
    exportFrame <- rbind(res.prof,result$alternatives)
    if(is.null(result$bm)){
      rownames(exportFrame) <- c("Best match","Alternatives",rep("",nrow(exportFrame)-2))
      exportStats <- rbind(c(names(result$stats),rep("",ncol(exportFrame)-length(result$stats))),
                           c(result$stats,rep("",ncol(exportFrame)-length(result$stats))))
      exportFrame <- rbind(exportFrame,exportStats)
      rownames(exportFrame)[nrow(exportFrame)] <- "Best match parameters"
      selected <- as.numeric(unlist(strsplit(tclvalue(plotselect),",")))+1
      if(length(selected)>0){
        for(i in 1:ncol(exportFrame)) exportFrame[selected[i],i] <- paste(exportFrame[selected[i],i],"*")
        if(tclvalue(estAlpha2)=="") exportFrame <- rbind(exportFrame,"Selected (*) parameters"=c(tclvalue(estAlpha),tclvalue(estTau),rep("",i-2)))
        else exportFrame <- rbind(exportFrame,"Selected (*) parameters"=c(tclvalue(estAlpha),tclvalue(estAlpha2),tclvalue(estTau),rep("",i-3)))
      }
    }
    else{
      rwnm <- paste(unlist(lapply(dimnames(result$profiles)[[1]], cropName)),sep="/",collapse="/")
      rownames(exportFrame) <- c(rwnm,result$bm,"Alternatives",rep("",nrow(exportFrame)-2))[1:nrow(exportFrame)]
      exportStats <- rbind(c(names(result$stats),rep("",ncol(exportFrame)-length(result$stats))),
                           c(result$stats,rep("",ncol(exportFrame)-length(result$stats))))
      exportStats <- rbind(exportStats,cbind(result$bmstats,matrix("",nrow(result$bmstats),ncol(exportFrame)-length(result$stats))))
      rownames(exportStats)[-1] <- c(rwnm,result$bm)
      exportFrame <- rbind(exportFrame,exportStats)
      selected <- as.numeric(unlist(strsplit(tclvalue(plotselect),",")))+1
      if(length(selected)>0){
        for(i in 1:ncol(exportFrame)) exportFrame[selected[i],i] <- paste(exportFrame[selected[i],i],"*")
        if(tclvalue(estAlpha2)=="") exportFrame <- rbind(exportFrame,"Selected (*) parameters"=c(tclvalue(estAlpha),tclvalue(estTau),rep("",i-2)))
        else exportFrame <- rbind(exportFrame,"Selected (*) parameters"=c(tclvalue(estAlpha),tclvalue(estAlpha2),tclvalue(estTau),rep("",i-3)))
      }
    }
    exportFileName <- tclvalue(tkgetSaveFile(initialfile=paste(unlist(strsplit(tclvalue(dataid),"\\."))[1],"_result.csv",sep=""),
                                             initialdir=tclvalue(path),filetypes="{{CVS File} {.csv}}"))
    if(nzchar(exportFileName)) write.csv2(exportFrame,file=exportFileName)
  }
  cropName <- function(z){
    z <- paste(substr(z,1,1),substr(z,nchar(z),nchar(z)),sep="")
    if(substr(z,1,1)=="U") z <- "U"
    z
  }
  ### TAB 2: DATA ###  
  TAB2 <- function(){
    tkdestroy(frame2)
    frame2 <<- tkframe(tab2)
    if(paste(tclvalue(dataid))==""){
      tkgrid(tklabel(frame2,text="\n  Select file in tab \"Files\" for analysis"))
    }
    else if(paste(tclvalue(dataid))!="" & paste(tclvalue(colselected))==""){
      header <- tclVar()
      msdata <- get("mixsep.data",envir=.GlobalEnv)
      data <- msdata[[tclvalue(dataid)]]$data
      nc <- ncol(data)
      tclvalue(header) <- paste("Analysis of case:",tclvalue(dataid))
      tkgrid(tklabel(frame2,text=paste(tclvalue(header)),font=font9bf),columnspan=nc+2)
      tkgrid(tklabel(frame2,text="Data preview (select columns with the indicated data):"),columnspan=nc+2)
      tkgrid(tklabel(frame2,text="Locus"),column=0,row=2,sticky="w")
      resetLocus <- tklabel(frame2,text="[RESET]",font=font7,foreground="blue")
      tkgrid(resetLocus,column=1,row=2,sticky="w")
      tkbind(resetLocus,"<Button-1>",function()tcl("set",locusCol,"0"))
      tkgrid(tklabel(frame2,text="Allele"),column=0,row=3,sticky="w")
      resetAllele <- tklabel(frame2,text="[RESET]",font=font7,foreground="blue")
      tkgrid(resetAllele,column=1,row=3,sticky="w")
      tkbind(resetAllele,"<Button-1>",function()tcl("set",alleleCol,"0"))
      tkgrid(tklabel(frame2,text="Height"),column=0,row=4,sticky="w")
      resetHeight <- tklabel(frame2,text="[RESET]",font=font7,foreground="blue")
      tkgrid(resetHeight,column=1,row=4,sticky="w")
      tkbind(resetHeight,"<Button-1>",function()tcl("set",heightCol,"0"))
      tkgrid(tklabel(frame2,text="Area"),column=0,row=5,sticky="w")
      resetArea <- tklabel(frame2,text="[RESET]",font=font7,foreground="blue")
      tkgrid(resetArea,column=1,row=5,sticky="w")
      tkbind(resetArea,"<Button-1>",function()tcl("set",areaCol,"0"))
      tclvalue(locusCol) <<- unlist(lapply(c("marker","Marker","locus","Locus","system","sys","DnaSystem"),grep,names(data)))[1]
      tclvalue(alleleCol) <<- unlist(lapply(c("type","Type","allele","Allele","Top_Allel_type"),grep,names(data)))[1]
      tclvalue(heightCol) <<- unlist(lapply(c("height","Height","hojde","Hojde","Top_Hoejde","hoejde"),grep,names(data)))[1]
      tclvalue(areaCol) <<- unlist(lapply(c("area","Area","areal","Areal","Top_Areal"),grep,names(data)))[1]
      if(as.numeric(tclvalue(locusCol))<0) tclvalue(locusCol) <<- ""
      if(as.numeric(tclvalue(alleleCol))<0) tclvalue(alleleCol) <<- ""
      if(as.numeric(tclvalue(heightCol))<0) tclvalue(heightCol) <<- ""
      if(as.numeric(tclvalue(areaCol))<0) tclvalue(areaCol) <<- ""
      ## Check whether the auto-selected 'height' and 'area' columns contain zero or NA-observations
      if(!(paste(tclvalue(heightCol))=="" | paste(tclvalue(heightCol))=="0")){
        if(is.element("0",paste(data[,as.numeric(paste(tclvalue(heightCol)))]))) tclvalue(heightCol) <- "0"
      }
      if(!(paste(tclvalue(areaCol))=="" | paste(tclvalue(areaCol))=="0")){
        if(is.element("0",paste(data[,as.numeric(paste(tclvalue(areaCol)))]))) tclvalue(areaCol) <- "0"
      }
      for(i in 1:nc){
        tkgrid(tkradiobutton(frame2,variable=locusCol,value=paste(i)),column=i+1,row=2)
        tkgrid(tkradiobutton(frame2,variable=alleleCol,value=paste(i)),column=i+1,row=3)
        tkgrid(tkradiobutton(frame2,variable=heightCol,value=paste(i)),column=i+1,row=4)
        tkgrid(tkradiobutton(frame2,variable=areaCol,value=paste(i)),column=i+1,row=5)
        tkgrid(tklabel(frame2,text=names(data)[i],font=font9bf),column=i+1,row=6)
        for(j in 1:4) tkgrid(tklabel(frame2,text=paste(data[j,i])),column=i+1,row=6+j)
      }
      tkgrid(tklabel(frame2,text=""))
      selCols <- tkbutton(frame2,text="Select columns",command=colSelected,default="active")
      tkgrid(selCols,columnspan=nc+1)
      tkgrid(tklabel(frame2,text="\nIf only height or area information is available leave the missing column blank above",foreground="red",font=font8),columnspan=nc+1,sticky="w")
      tkfocus(selCols)
      tkgrid(tklabel(frame2,text=""))
    }
    else{
      cols <- as.numeric(unlist(strsplit(tclvalue(colselected),",")))
      header <- tclVar()
      msdata <- get("mixsep.data",envir=.GlobalEnv)
      data <- msdata[[tclvalue(dataid)]]$data
      data <- data[,cols]
      tclvalue(header) <- paste("Analysis of case:",tclvalue(dataid))
      tkgrid(tklabel(frame2,text=paste(tclvalue(header)),font=font9bf),columnspan=4)
      tkgrid(tklabel(frame2,text="Selected columns:"),columnspan=4)
      tkgrid(tklabel(frame2,text="Locus",font=font9bf),column=0,row=2)
      tkgrid(tklabel(frame2,text="Allele",font=font9bf),column=1,row=2)
      tkgrid(tklabel(frame2,text="Height",font=font9bf),column=2,row=2)
      tkgrid(tklabel(frame2,text="Area",font=font9bf),column=3,row=2)
      for(i in 1:4){
        tkgrid(tklabel(frame2,text=names(data)[i],font=font9it),column=i-1,row=3)
        for(j in 1:4) tkgrid(tklabel(frame2,text=paste(data[j,i])),column=i-1,row=3+j)
      }
      tkgrid(tklabel(frame2,text=""))
      contButton <- tkbutton(frame2,text="Continue",default="active",command=function()tk2notetab.select(tabwin,"Parameters and known profiles"))
      resetButton <- tkbutton(frame2,text="Reset",command=resetData)
      tkgrid(contButton,row=9,column=0)
      tkgrid(resetButton,row=9,column=2)
      tkgrid.configure(contButton,columnspan=2)
      tkgrid.configure(resetButton,columnspan=2)
      tkfocus(contButton)
    }
    tkgrid(frame2)
  }

  ### TAB 3: PARAMETERS AND SUSPECT ###
  TAB3 <- function(){
    tkdestroy(frame3)
    frame3 <<- tkframe(tab3)
    if(paste(tclvalue(dataid))=="" & paste(tclvalue(colselected))==""){
      tkgrid(tklabel(frame3,text="\n  Select file in tab \"Files\" for analysis"))
    }
    else if(paste(tclvalue(dataid))!="" & paste(tclvalue(colselected))==""){
      tkgrid(tklabel(frame3,text="\n  Select the appropriate columns in the \"Data\"-tab for analysis"))
    }
    else{
      parFrame <- tkframe(frame3)
      contribFrame <- tkframe(parFrame,relief="groove",borderwidth=2)
      known1Frame <- tkframe(parFrame,relief="groove",borderwidth=2)
      known2Frame <- tkframe(parFrame,relief="groove",borderwidth=2)
      known3Frame <- tkframe(parFrame,relief="groove",borderwidth=2)
      cols <- as.numeric(unlist(strsplit(tclvalue(colselected),",")))
      header <- tclVar()
      msdata <- get("mixsep.data",envir=.GlobalEnv)
      data <- msdata[[tclvalue(dataid)]]$data
      data <- data[,cols]
      names(data) <- c("locus","allele","height","area")
      locusorder <- unique(paste(data$locus))
      datas <- split(data,data$locus)
      datas <- datas[match(locusorder,names(datas))]
      ns <- unlist(lapply(datas,nrow))
      maxns <- max(ns)
      header <- tclVar()
      tclvalue(header) <- paste("Analysis of case:",tclvalue(dataid))
      tkgrid(tklabel(parFrame,text=paste(tclvalue(header)),font=font9bf),columnspan=7) 
      noText <- tklabel(contribFrame,text="Number of contributors:") 
      noContrib <<- tclVar(ceiling(maxns/2))
      tkgrid(noText,column=0,row=2,sticky="e")
      m2 <- tkradiobutton(contribFrame,variable=noContrib,value="2",text="2")
      if(maxns<=4) tkgrid(m2,column=2,row=2,sticky="w")
      m3 <- tkradiobutton(contribFrame,variable=noContrib,value="3",text="3")
      tkgrid(m3,column=3,row=2,sticky="w")
      tkgrid(tklabel(contribFrame,text="Search for alternatives:"),column=0,row=4,sticky="e")
      altQ <- tklabel(contribFrame,text="[?]",font=font7,foreground="blue")
      tkgrid(altQ,column=1,row=4)
      altHelp <- function(){ tkmessageBox(title="Help on alternative search",message="Searching for alternative DNA profile configurations with an acceptable fit to the data compared to the best matching or fixed profile configuration.",icon="info",type="ok") }
      tkbind(altQ,"<Button-1>",altHelp)
      tkgrid(tkcheckbutton(contribFrame,variable=searchalt),column=2,row=4,sticky="w")
      tkgrid(tklabel(contribFrame,text="Specify significance level:"),column=0,row=5,sticky="e")
      sigQ <- tklabel(contribFrame,text="[?]",font=font7,foreground="blue")
      tkgrid(sigQ,column=1,row=5)
      sigHelp <- function(){ tkmessageBox(title="Help on significance level",message="Used when testing for alternative configurations.\nThe higher the significance level the fewer alternatives are accepted by the test.\nThat is, a level of 0.001 gives the most alternatives and 0.1 the fewest.",icon="info",type="ok") }
      tkbind(sigQ,"<Button-1>",sigHelp)
      tkgrid(tkradiobutton(contribFrame,variable=altp,value="0.001",text="0.001"),column=2,row=5,sticky="w")
      tkgrid(tkradiobutton(contribFrame,variable=altp,value="0.01",text="0.01"),column=3,row=5,sticky="w")
      tkgrid(tkradiobutton(contribFrame,variable=altp,value="0.05",text="0.05"),column=4,row=5,sticky="w")
      tkgrid(tkradiobutton(contribFrame,variable=altp,value="0.1",text="0.1"),column=5,row=5,sticky="w")
      tkgrid(tklabel(contribFrame,text="Drop non-fitting loci:"),row=6,column=0,sticky="e")
      dropQ <- tklabel(contribFrame,text="[?]",font=font7,foreground="blue")
      dropHelp <- function(){ tkmessageBox(title="Help on dropping non-fitting loci",message="Testing whether the assumption of a DNA mixture of the specified number of contributors is supported by the data. The test uses the same significance level as when testing for alternative profile combinations",icon="info",type="ok") }
      tkgrid(dropQ,column=1,row=6)
      tkbind(dropQ,"<Button-1>",dropHelp)
      tkgrid(tkcheckbutton(contribFrame,variable=dropLoci),column=2,row=6,sticky="w")
      tkgrid(tkcheckbutton(known1Frame,variable=known1Set,text="Use fixed profile 1",font=font9bf),column=0,row=0,columnspan=8,sticky="w")
      for(i in 1:length(datas)){
        tkgrid(tklabel(known1Frame,text=names(datas)[i],font=font9bful),column=(i-1)*2,row=1,columnspan=2)
        for(j in 1:maxns){
          if(j<=ns[i]){
            tkgrid(tkcheckbutton(known1Frame,text=paste(datas[[i]]$allele[j]),variable=paste(tclvalue(dataid),"known1",datas[[i]]$locus[j],datas[[i]]$allele[j],sep=":")),column=(i-1)*2,row=1+j,sticky="w")
            tkgrid(tklabel(known1Frame,text="  "),column=(i-1)*2+1,row=1+j)
          }
        }
      }
      tkgrid(tkcheckbutton(known2Frame,variable=known2Set,text="Use fixed profile 2",font=font9bf),column=0,row=0,columnspan=8,sticky="w")
      for(i in 1:length(datas)){
        tkgrid(tklabel(known2Frame,text=names(datas)[i],font=font9bful),column=(i-1)*2,row=1,columnspan=2)
        for(j in 1:maxns){
          if(j<=ns[i]){
            tkgrid(tkcheckbutton(known2Frame,text=paste(datas[[i]]$allele[j]),variable=paste(tclvalue(dataid),"known2",datas[[i]]$locus[j],datas[[i]]$allele[j],sep=":")),column=(i-1)*2,row=1+j,sticky="w")
            tkgrid(tklabel(known2Frame,text="  "),column=(i-1)*2+1,row=1+j)
          }
        }
      }
      tkgrid(tkcheckbutton(known3Frame,variable=known3Set,text="Use fixed profile 3",font=font9bf),column=0,row=0,columnspan=8,sticky="w")
      for(i in 1:length(datas)){
        tkgrid(tklabel(known3Frame,text=names(datas)[i],font=font9bful),column=(i-1)*2,row=1,columnspan=2)
        for(j in 1:maxns){
          if(j<=ns[i]){
            tkgrid(tkcheckbutton(known3Frame,text=paste(datas[[i]]$allele[j]),variable=paste(tclvalue(dataid),"known3",datas[[i]]$locus[j],datas[[i]]$allele[j],sep=":")),column=(i-1)*2,row=1+j,sticky="w")
            tkgrid(tklabel(known3Frame,text="  "),column=(i-1)*2+1,row=1+j)
          }
        }
      }
      tkgrid(parFrame)
      tkgrid(contribFrame)
      tkgrid(tklabel(parFrame,text=""))
      tkgrid(known1Frame)
      tkgrid(tklabel(parFrame,text=""))
      tkgrid(known2Frame)
      tkgrid(tklabel(parFrame,text=""))
      tkgrid(known3Frame)
      tkgrid(tklabel(parFrame,text=""))
      buttonAnalyse <- tkbutton(parFrame,text="Analyse mixture!",command=callMixsep,default="active")
      tkgrid(buttonAnalyse)
      tkfocus(buttonAnalyse)
      tkgrid(tklabel(parFrame,text=""))
    }
    tkgrid(frame3)
  }
  
  ### TAB 4: RESULTS ###
  TAB4 <- function(){
    tkdestroy(frame4)
    frame4 <<- tkframe(tab4)
    if(paste(tclvalue(dataid))!="" & paste(tclvalue(colselected))!="" & paste(tclvalue(pars))!="" & paste(tclvalue(res))!=""){
      resFrame <- tkframe(frame4)
      combFrame <- tkframe(resFrame,relief="groove",borderwidth=2)
      estFrame <- tkframe(resFrame,relief="groove",borderwidth=2)            
      header <- tclVar()
      tclvalue(header) <- paste("Analysis of case:",tclvalue(dataid))
      tkgrid(tklabel(resFrame,text=paste(tclvalue(header)),font=font9bf),columnspan=7,row=0)
      result <- get("mixsep.data",envir=.GlobalEnv)[[paste(tclvalue(dataid))]]$result
      res.prof <- apply(result$profiles,2,paste,collapse="/")
      res.alt <- result$alternatives
      plotMe <- as.numeric(unlist(strsplit(tclvalue(plotselect),",")))
      loci <- names(res.prof)
      nloci <- length(loci)
      if(length(plotMe)!=nloci) plotMe <- rep(0,nloci)
      altmax <- max(nrow(res.alt))
      noAltsLocus <- apply(res.alt,2,function(z) sum(z!=""))
      altboxHeight <- min(c(max(noAltsLocus-length(result$bm)),5))
      altboxWidth <- max(nchar(res.prof))+3
      altMax <- altboxHeight+length(result$bm)+1
      if(nloci<=5) colid <- (1:nloci-1)*4+1 else colid <- rep((1:5-1)*4+1,5)
      if(nloci<=5) rowid <- rep(3,nloci) else rowid <- rep(1+(0:5)*altMax+c(0,4+(1:5)*2),each=5)
      selCombsNotSet <- all(plotMe==0)
      dropped <- as.numeric(paste(unlist(strsplit(paste(tclvalue(dropLocus)),","))))
                                        #
      SCR <<- list(tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[1]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[2]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[3]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[4]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[5]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[6]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[7]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[8]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[9]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[10]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[11]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[12]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[13]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[14]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[15]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[16]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[17]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[18]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[19]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[20]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[21]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[22]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[23]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[24]],...)),
                   tkscrollbar(combFrame, repeatinterval=(altboxHeight+1), command=function(...)tkyview(LOCI[[25]],...)))      
      LOCI <<- list(tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[1]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[2]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[3]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[4]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[5]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[6]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[7]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[8]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[9]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[10]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[11]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[12]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[13]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[14]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[15]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[16]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[17]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[18]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[19]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[20]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[21]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[22]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[23]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[24]],...)),
                    tklistbox(combFrame,height=altboxHeight,selectmode="single",
                              background="white",width=altboxWidth,exportselection="false",yscrollcommand=function(...)tkset(SCR[[25]],...)))
      ##
      for(i in 1:nloci){ ## Max 25 loci due to hard code in LOCISCR.R (does not work if created using for-loop)
        locusHead <- tclVar("")
        if(!is.null(result$bm)){
          row1 <- dimnames(result$profiles)[[1]]
          row1 <- paste(unlist(lapply(row1,cropName)),sep="/",collapse="/")
          row1p <- result$bm      
          tkgrid(tklabel(combFrame,text=paste(row1,":",sep="")),column=0,row=rowid[i],sticky="w")
          for(k in 1:length(row1p)) tkgrid(tklabel(combFrame,text=paste(row1p[k],":",sep="")),column=0,row=rowid[i]+k,sticky="w")
          if(length(row1p)!=altmax) tkgrid(tklabel(combFrame,text="Alternatives:"),column=0,row=rowid[i]+1+k,sticky="nw")
        }
        else{
          tkgrid(tklabel(combFrame,text="Best match:"),column=0,row=rowid[i],sticky="w")
          if(max(noAltsLocus)>0) tkgrid(tklabel(combFrame,text="Alternatives:"),column=0,row=rowid[i]+1,sticky="nw",rowspan=6)
        }
        if(length(dropped)>0){
          if(is.element(i,dropped)) locusHead <- tklabel(combFrame,text=paste(loci[i]," (",noAltsLocus[i]-length(result$bm),")",sep=""),font=font9bful,foreground="red")
          else locusHead <- tklabel(combFrame,text=paste(loci[i]," (",noAltsLocus[i]-length(result$bm),")",sep=""),font=font9bful)
        }
        else locusHead <- tklabel(combFrame,text=paste(loci[i]," (",noAltsLocus[i]-length(result$bm),")",sep=""),font=font9bful)
        tkgrid(locusHead,column=colid[i],row=rowid[i]-1,columnspan=3)
        if(selCombsNotSet){
          if(is.null(result$bm)){
            tmpSelcomb <- tklabel(combFrame,text=paste(res.prof[i]),font=font9it)
            tmpSelRadio <- tkradiobutton(combFrame,variable=paste(tclvalue(dataid),loci[i],sep=":"),value="0")
          }
          else{
            tmpSelcomb <- tklabel(combFrame,text=paste(res.prof[i]),font=font9)
            tmpSelRadio <- tkradiobutton(combFrame,variable=paste(tclvalue(dataid),loci[i],sep=":"),value="0")
          }
          tkgrid(tmpSelRadio,column=colid[i],row=rowid[i],sticky="w")
          tkgrid(tmpSelcomb,column=colid[i]+1,row=rowid[i],sticky="w")
          tcl(tmpSelRadio,"select")
          tkselection.set(LOCI[[i]],0)
        }
        else{
          if(is.null(result$bm)) tmpSelcomb <- tklabel(combFrame,text=paste(res.prof[i]),font=font9it)
          else tmpSelcomb <- tklabel(combFrame,text=paste(res.prof[i]),font=font9)
          tmpSelRadio <- tkradiobutton(combFrame,variable=paste(tclvalue(dataid),loci[i],sep=":"),value="0")
        }
        tkgrid(tmpSelRadio,column=colid[i],row=rowid[i],sticky="w")
        tkgrid(tmpSelcomb,column=colid[i]+1,row=rowid[i],sticky="w")
        tkgrid(tklabel(combFrame,text="  "),column=colid[i]+3,row=rowid[i])
        k <- 1
        if(!is.null(result$bm)){ ## First length(result$bm) rows in res.alt are alternatives including fixed profiles and unknowns
          for(k in 1:length(result$bm)){ 
            tkgrid(tkradiobutton(combFrame,variable=paste(tclvalue(dataid),loci[i],sep=":"),value=paste(k)),column=colid[i],row=rowid[i]+k,sticky="w")
            tkgrid(tklabel(combFrame,text=paste(res.alt[k,i])),column=colid[i]+1,row=rowid[i]+k,sticky="w")
          }
          fromRow <- length(result$bm)+1
        }
        else fromRow <- 1
        if(nrow(res.alt)>=fromRow){
          if(res.alt[fromRow,i]!=""){
            tkgrid(tkradiobutton(combFrame,variable=paste(tclvalue(dataid),loci[i],sep=":"),value="A"),column=colid[i],row=rowid[i]+fromRow,sticky="nw")
            tkgrid(LOCI[[i]],column=colid[i]+1,row=rowid[i]+fromRow,sticky="w")
            if(fromRow<=altmax){
              for(j in fromRow:altmax){
                if(res.alt[j,i]!="") tkinsert(LOCI[[i]],"end",res.alt[j,i])
              }
            }
            if(sum(res.alt[fromRow:altmax,i]!="")>altboxHeight) tkgrid(SCR[[i]],column=colid[i]+2,row=rowid[i]+fromRow,rowspan=altboxHeight,sticky="nsw")
            tcl(LOCI[[i]], "yview", "scroll", plotMe[i]-1,"units")
            tkselection.set(LOCI[[i]],plotMe[i]-1)
          }
        }
        tkgrid(tklabel(combFrame,text="  "),column=colid[i]+3,row=rowid[i])
      }
      tkgrid(tklabel(combFrame,text=""))
      tkgrid(resFrame)
      tkgrid(combFrame)
      tkgrid(tklabel(resFrame,text=paste("\nNumber of combinations: ",format(result$noCombs,big.mark=",",scientific=2),"\n",sep="")))
      
      tkgrid(tklabel(estFrame,text=""),column=0,row=0)
      tkgrid(tklabel(estFrame,text="Selected",font=font9ul),column=1,row=0,sticky="e")
      if(tclvalue(estAlpha2)!=""){
        printAlpha1 <- tklabel(estFrame,text="")
        tkconfigure(printAlpha1,textvariable=estAlpha)
        printAlpha2 <- tklabel(estFrame,text="")
        tkconfigure(printAlpha2,textvariable=estAlpha2)
        printTau <- tklabel(estFrame,text="")
        tkconfigure(printTau,textvariable=estTau)
        tkgrid(tklabel(estFrame,text="Estimated alpha 1:"),column=0,row=1,sticky="e")
        tkgrid(tklabel(estFrame,text="Estimated alpha 2:"),column=0,row=2,sticky="e")
        tkgrid(tklabel(estFrame,text="Estimated tau:"),column=0,row=3,sticky="e")
        tkgrid(printAlpha1,sticky="e",column=1,row=1)
        tkgrid(printAlpha2,sticky="e",column=1,row=2)
        tkgrid(printTau,sticky="e",column=1,row=3)
      }
      else{
        printAlpha1 <- tklabel(estFrame,text="")
        tkconfigure(printAlpha1,textvariable=estAlpha)
        printTau <- tklabel(estFrame,text="")
        tkconfigure(printTau,textvariable=estTau)
        tkgrid(tklabel(estFrame,text="Estimated alpha:"),column=0,row=1,sticky="e")
        tkgrid(tklabel(estFrame,text="Estimated tau:"),column=0,row=2,sticky="e")
        tkgrid(printAlpha1,sticky="e",column=1,row=1)
        tkgrid(printTau,sticky="e",column=1,row=2)
      }
      if(!is.null(result$bm)){
        if(length(result$stats)==3){
          tkgrid(tklabel(estFrame,text=row1,font=font9itul),column=2,row=0,sticky="e")
          tkgrid(tklabel(estFrame,text=paste(result$stats[["alpha1"]]),font=font9it),sticky="e",column=2,row=1)
          tkgrid(tklabel(estFrame,text=paste(result$stats[["alpha2"]]),font=font9it),sticky="e",column=2,row=2)
          tkgrid(tklabel(estFrame,text=paste(result$stats[["tau"]]),font=font9it),sticky="e",column=2,row=3)
          for(k in 1:length(row1p)){
            tkgrid(tklabel(estFrame,text=row1p[k],font=font9itul),column=2+k,row=0,sticky="e")
            tkgrid(tklabel(estFrame,text=result$bmstats[k,1]),column=2+k,row=1,sticky="e")
            tkgrid(tklabel(estFrame,text=result$bmstats[k,2]),column=2+k,row=2,sticky="e")
            tkgrid(tklabel(estFrame,text=result$bmstats[k,3]),column=2+k,row=3,sticky="e")
          }
        }
        else{
          tkgrid(tklabel(estFrame,text=row1,font=font9itul),column=2,row=0,sticky="e")
          tkgrid(tklabel(estFrame,text=paste(result$stats[["alpha"]]),font=font9it),sticky="e",column=2,row=1)
          tkgrid(tklabel(estFrame,text=paste(result$stats[["tau"]]),font=font9it),sticky="e",column=2,row=2)
          for(k in 1:length(row1p)){
            tkgrid(tklabel(estFrame,text=row1p[k],font=font9itul),column=2+k,row=0,sticky="e")
            tkgrid(tklabel(estFrame,text=result$bmstats[k,1]),column=2+k,row=1,sticky="e")
            tkgrid(tklabel(estFrame,text=result$bmstats[k,2]),column=2+k,row=2,sticky="e")
          }
        }
      }
      else{
        if(length(result$stats)==3){
          tkgrid(tklabel(estFrame,text="Best match",font=font9itul),column=2,row=0,sticky="e")
          tkgrid(tklabel(estFrame,text=paste(result$stats[["alpha1"]]),font=font9it),sticky="e",column=2,row=1)
          tkgrid(tklabel(estFrame,text=paste(result$stats[["alpha2"]]),font=font9it),sticky="e",column=2,row=2)
          tkgrid(tklabel(estFrame,text=paste(result$stats[["tau"]]),font=font9it),sticky="e",column=2,row=3)
        }
        else{
          tkgrid(tklabel(estFrame,text="Best match",font=font9itul),column=2,row=0,sticky="e")
          tkgrid(tklabel(estFrame,text=paste(result$stats[["alpha"]]),font=font9it),sticky="e",column=2,row=1)
          tkgrid(tklabel(estFrame,text=paste(result$stats[["tau"]]),font=font9it),sticky="e",column=2,row=2)
        }
      }
      tkgrid(estFrame)

      tkgrid(tklabel(resFrame,text="\nEstimates of alpha and tau are updated upon plotting"),columnspan=2)
      tkgrid(tklabel(resFrame,text=""))
      tkgrid(tkcheckbutton(resFrame,text="Open plot in new plot window",variable=newPlot))
      tkgrid(tkcheckbutton(resFrame,text="Add profile table to plot",variable=addProfile))
      plotButton <- tkbutton(resFrame,text="Plot selected profiles",command=plotEpg,default="active")
      tkgrid(plotButton,sticky="s")
      tkgrid(tklabel(resFrame,text=""))
      exportButton <- tkbutton(resFrame,text="Export result",command=exportResult)
      tkgrid(exportButton,sticky="s")
      tkgrid(tklabel(resFrame,text=""))
      tkfocus(plotButton)
    }
    else{
      tkgrid(tklabel(frame4,text="\n  No results yet"))
    }
    tkgrid(frame4)
  }
  
  ## Main function
  msmain <- tktoplevel()
  tcl("tk_setPalette","gray93") 
  tkbind(msmain,"<Destroy>",function(){tclvalue(killR) <- "ok"})
  db <- tclVar("")
  dbtab <- tclVar("")
  dbmax <- tclVar(200)
  if(!is.null(unlist(options("mixsep")))){
    tclvalue(db) <- paste(options("mixsep")$mixsep$db)
    tclvalue(dbtab) <- paste(options("mixsep")$mixsep$dbtab)
    tclvalue(dbmax) <- paste(options("mixsep")$mixsep$dbmax)
  }
  path <- tclVar("")
  dataid <- tclVar("")
  locusCol <- tclVar("")
  alleleCol <- tclVar("")
  heightCol <- tclVar("")
  areaCol <- tclVar("")
  colselected <- tclVar("")
  pars <- tclVar("")
  res <- tclVar("")
  estTau <- tclVar("")
  estAlpha <- tclVar("")
  estAlpha2 <- tclVar("")
  altp <- tclVar(0.001)
  dropLoci <- tclVar(0)
  searchalt <- tclVar(1)
  dropLocus <- tclVar("")
  noContrib <- tclVar(2)
  known1Set <- tclVar(0)
  known2Set <- tclVar(0)
  known3Set <- tclVar(0)
  plotselect <- tclVar("")
  newPlot <- tclVar(0)
  addProfile <- tclVar(0) 
  queryRadio <- tclVar("")
  querySample <- tclVar("")
  queryNumber <- tclVar("")
  queryQuery <- tclVar("")
  DBsamples <- "" ## global variable used in DB selection
  LOCI <- list()
  SCR <- list()
  tkwm.title(msmain, "Forensic Genetics DNA Mixture Separator - Version 0.1")
  tabwin <- tk2notebook(msmain, tabs = c("Files", "Data", "Parameters and known profiles", "Results"))
  tkpack(tabwin, fill = "both", expand = 1)
  tab1 <- tk2notetab(tabwin, "Files")
  tab2 <- tk2notetab(tabwin, "Data")
  tab3 <- tk2notetab(tabwin, "Parameters and known profiles")
  tab4 <- tk2notetab(tabwin, "Results")

  #
  frame2 <- tkframe(tab2)
  TAB2()
  tkgrid(frame2)
  #
  frame3 <- tkframe(tab3)
  TAB3()
  tkgrid(frame3)
  #
  frame4 <- tkframe(tab4)
  TAB4()
  tkgrid(frame4)
  
### TAB 1: FILES ###
  tkgrid(tklabel(tab1,text="      Forensic Genetics DNA Mixture Separator - Version 0.1      ",font=font14bf),sticky="n",columnspan=1)
  tkgrid(tklabel(tab1,text="Available files:\n",font=font9bf),sticky="n",columnspan=1)
  filesFrame <- tkframe(tab1)
  scr <- tkscrollbar(filesFrame, repeatinterval=21, command=function(...)tkyview(caselist,...))
  caselist <- tklistbox(filesFrame,height=20,selectmode="extended",background="white",width=80,yscrollcommand=function(...)tkset(scr,...))
  tkbind(caselist,"<Delete>",removeFile)
  tkbind(caselist,"<Return>",openAnalysis)
  tkbind(caselist,"<Double-Button-1>",openAnalysis)
  tkbind(caselist,"<Control-a>",function()tkselection.set(caselist,0,tclvalue(tcl(caselist,"size"))))
  if(!exists("mixsep.data",envir=.GlobalEnv)){
    tkgrid(caselist,scr)
    tkgrid.configure(scr,rowspan=20,sticky="nse")
  }
  else{
    msdata <- get("mixsep.data",envir=.GlobalEnv)
    if(length(msdata)==0){
      tkgrid(caselist,scr)
      tkgrid.configure(scr,rowspan=20,sticky="nse")
    }
    else{
      tkgrid(caselist,scr)
      tkgrid.configure(scr,rowspan=20,sticky="nse")
      cases <- names(msdata)
      for(i in (1:length(cases))) tkinsert(caselist,"end",cases[i])
    }
  }
  tkselection.set(caselist,0)
  tkgrid(filesFrame)
  tkgrid(tklabel(tab1,text="\n"))
  frontFrame <- tkframe(tab1)
  button.analyse <- tkbutton(frontFrame,text="  Analyse file  ",command=openAnalysis,default="active")
  button.delete <- tkbutton(frontFrame, text="   Delete file  ",command=removeFile)
  button.file <- tkbutton(frontFrame,   text="    Add file    ",command=function()getFile(mult=FALSE))
  button.multfile <- tkbutton(frontFrame,   text="    Add multi-sample file    ",command=function()getFile(mult=TRUE))
  button.quit <- tkbutton(frontFrame,   text="      Quit      ",command=killMs)
  tkgrid(button.analyse,tklabel(frontFrame,text="  "),button.delete,tklabel(frontFrame,text="  "),
         button.file,tklabel(frontFrame,text="  "),button.multfile,tklabel(frontFrame,text="  "),button.quit)
  tkgrid(frontFrame)
  tkgrid(tklabel(tab1,text="\n\n"))
  tkfocus(button.analyse)
  tkwait.window(msmain)
}

