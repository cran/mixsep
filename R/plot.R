allele2bp <- function(x,kit="SGM"){
  x$allele <- paste(x$allele)
  x$allele[is.element(toupper(paste(x$allele)),c("X"))] <- 1
  x$allele[is.element(toupper(paste(x$allele)),c("Y"))] <- 3
  x$allele <- as.numeric(paste(x$allele))
  x$locus <- toupper(x$locus)
  offsets <- data.frame(locus=c("CSF","D7","D8","D21","D2","D3","D13","D16","TH0","THO","D18","D19","TPO","VWA","D5","FGA","AME","SE33"),
                        offset=c(281,231,91,88,247,64,184,232,147,147,234,66,198,111,106,146,92,100), ## x-offset (fixed length for all kits)
                        ID=c(rep(580,4),rep(390,5+1),rep(200,4),rep(10,3),NA), ## y-offset (kit dependent via dye lane) 
                        SE=c(NA,NA,390,10,580,580,NA,580,200,200,10,200,NA,580,NA,200,390,390), ## y-offset (kit dependent via dye lane) 
                        SGM=c(NA,NA,200,200,390,390,NA,390,10,10,200,10,NA,390,NA,10,200,NA), ## y-offset (kit dependent via dye lane) 
                        PP=c(NA,10,200,200,NA,390,10,NA,NA,NA,200,NA,NA,390,10,390,200,NA)) ## y-offset (kit dependent via dye lane) 
  offsets <- offsets[,c("locus","offset",kit)]
  names(offsets)[3] <- "baseline"
  x <- merge(x,offsets,by="locus",all.x=TRUE)
  x$bp <- (x$offset-110)*4 + 16*round(x$allele)
  x$bp <- x$bp+40*(x$allele-round(x$allele))
  x
}

plotEPG <- function(x,color=TRUE,addProfile=FALSE,profiles=NULL,...){  
  ## check which kit is used:
  CSF <- length(grep("CSF",x$locus))>0
  SE33 <- length(grep("SE33",x$locus))>0
  D5 <- length(grep("D5",x$locus))>0
  D19 <- length(grep("D19",x$locus))>0
  if(CSF) kit <- "ID"
  else if(SE33) kit <- "SE"
  else if(D19 & !SE33) kit <- "SGM"
  else if(D5 & !CSF) kit <- "PP"
  else{
    stop("Plotting for the used kit is not possible")
    tkmessageBox(message="Plotting for the used kit is not possible",icon="error",type="ok")
  }
  kits <- list("ID"=list("B"=c("CSF","D7","D8","D21"),"G"=c("D2","D3","D13","D16","TH0","THO"),"Y"=c("D18","D19","TPO","VWA"),"R"=c("D5","FGA","AME")),
               "SE"=list("B"=c("D2","D3","D16","VWA"),"G"=c("D8","SE33","AME"),"Y"=c("D19","FGA","TH0","THO"),"R"=c("D18","D21")),
               "SGM"=list("B"=c("D2","D3","D16","VWA"),"G"=c("D8","D18","D21","AME"),"Y"=c("D19","FGA","TH0","THO")),
               "PP"=list("B"=c("D3","FGA","VWA"),"G"=c("D8","D18","D21","AME"),"Y"=c("D5","D7","D13")))
  kits <- kits[[kit]]
  DD <- list("ID"=c("R"=10,"Y"=200,"G"=390,"B"=580),"SE"=c("R"=10,"Y"=200,"G"=390,"B"=580),
             "SGM"=c("Y"=10,"G"=200,"B"=390),"PP"=c("Y"=10,"G"=200,"B"=390))
  DD <- DD[[kit]]
  kitcol <- as.data.frame(cbind("D"=rep(names(kits),unlist(lapply(kits,length))),"locus"=unlist(kits)))
  x$locus <- toupper(strtrim(unlist(lapply(strsplit(toupper(x$locus),split="S[1-9]"),function(z) z[1])),3))
  x <- merge(x,kitcol,by="locus")
  kitcolor <- data.frame("D"=c("B","G","Y","R"),color=c("#0076c9","#1eb04e","#ffe51d","#ef2f2f")) ## yellow was: #ffef41
  ## Plot in colors
  if(color) x <- merge(x,kitcolor,by="D",all.x=TRUE)
  else x$color <- "#999999"
  x <- allele2bp(x,kit=kit)
  x$expHeight <- x$exp*(x$height/x$area) ## gives the same proportionality between hat(h) and hat(A) as for observed h and A
  ## was: mh <- max(c(x$area,x$exp),na.rm=TRUE)
  mh <- max(c(x$height,x$expHeight),na.rm=TRUE)
  ## was: x$plotObs <- x$area*170/mh
  x$plotObs <- x$height*170/mh
  ## was: x$plotExp <- x$exp*170/mh
  x$plotExp <- x$expHeight*170/mh
  rx <- range(x$bp,na.rm=TRUE)
  par(mar=c(0,0,0,0))
  topskip <- rep(c(0,40,80,120),each=7) ## to make room for profiles in top of plot
  plot(c(rx[1]-40,rx[2]),c(0,max(x$baseline,na.rm=TRUE)+170+topskip[length(profiles)]*addProfile),type="n",xlab="",ylab="",axes=FALSE,...); 
  N <- nrow(x)
  yy <- rep(x$baseline,each=4)
  yy[seq(from=4,by=4,len=N)] <- NA
  obsPeak <- yy
  expPeak <- yy
  obsPeak[seq(from=2,by=4,len=N)] <- obsPeak[seq(from=2,by=4,len=N)]+x$plotObs
  expPeak[seq(from=2,by=4,len=N)] <- expPeak[seq(from=2,by=4,len=N)]+x$plotExp
  xx <- rep(x$bp,each=4)+rep(c(-8,0,8,NA),N)
  ## Adding 'grid' lines to the plot indicating the actual intensities of the peaks
  gl <- seq(from=250,to=mh,by=250)
  if(length(gl)<4) gl <- seq(from=100,to=mh,by=100)
  if(length(gl)>6) gl <- gl[floor(seq(from=1,to=length(gl),len=6))]
  gl <- c(50,gl)
  gridLines <- gl*170/mh
  abline(h=(hh <- rep(gridLines,length(DD))+rep(DD,each=length(gridLines))),col="#efefef")
  text(rep(rx[1]-30,length(hh)),hh,gl,cex=0.60,adj=c(1,0.5))
  polygon(xx,obsPeak,col=paste(x$color),border=NA)
  polygon(xx,expPeak,border=1,lty=1)
  abline(h=DD)
  ## Adding allele and locus designations to plot
  loci <- aggregate(x$allele,by=list(paste(x$locus)),median)
  names(loci) <- c("locus","allele")
  loci <- allele2bp(loci,kit=kit)
  text(loci$bp,loci$baseline-15,paste(loci$locus),cex=0.75,adj=c(0.5,1))
  x$allele[x$locus=="AME" & x$allele=="1"] <- "X"
  x$allele[x$locus=="AME" & x$allele=="3"] <- "Y"
  text(x$bp,x$baseline-8,paste(x$allele),cex=0.75)
  #box()
  if(addProfile){ ## add profile table to plot
    addx <- rep(seq(from=rx[1],to=rx[2]-40,len=7),28)
    addy <- par("usr")[4]-topskip
    for(i in 1:length(profiles)){
      text(addx[i],addy[i]-20,names(profiles)[i],adj=c(0.5,0.5),cex=0.7)
      text(addx[i],addy[i]-35,profiles[i],adj=c(0.5,0.5),cex=0.7)
    }
  }
}

computeExpArea <- function(x,datacols,y){
  alts <- apply(x$result$profiles,2,paste,collapse="/")
  alts <- rbind(alts,x$result$alternatives)
  comb <- diag(alts[as.numeric(y)+1,1:length(y)])
  names(comb) <- dimnames(x$result$profiles)[[2]]
  comb <- lapply(comb,function(z){
    pp <- lapply(strsplit(z,"/"),strsplit,split=",")[[1]]
    alleles <- sort(unique(unlist(pp)))
    loc <- as.data.frame(cbind(alleles,matrix(0,length(alleles),length(pp))))
    names(loc) <- c("allele",paste("P",1:length(pp),sep=""))
    for(i in 1:length(pp)) loc[,i+1] <- (loc$allele==pp[[i]][1])+(loc$allele==pp[[i]][2])
    loc
  }) 
  comb <- as.data.frame(cbind(locus=rep(names(comb),unlist(lapply(comb,nrow))),do.call("rbind",comb)))
  data <- x$data[,datacols]
  names(data) <- c("locus","allele","height","area")
  d <- merge(data,comb,by=c("locus","allele"))
  aa <- ahat(split(d,d$locus),m=ncol(comb)-2)
  tt <- that(split(d,d$locus),alpha=aa,m=ncol(comb)-2)
  expArea <- expectedAreas(split(d,d$locus),alpha=aa,m=ncol(comb)-2)
  list(data=expArea[,c("locus","allele",paste("P",1:(ncol(comb)-2),sep=""),"exp")],
       alpha=aa,tau=tt)
}

