allele2bp <- function(x,kit="SGM"){
  x$allele <- paste(x$allele)
  x$allele[is.element(toupper(paste(x$allele)),c("X"))] <- 1
  x$allele[is.element(toupper(paste(x$allele)),c("Y"))] <- 3
  x$allele <- as.numeric(paste(x$allele))
  x$locus <- toupper(x$locus)
  offsets <- data.frame(locus=c("CSF","D7","D8","D21","D2","D3","D13","D16","TH0","THO","D18","D19","TPO","VWA","D5","FGA","AME","SE33"),
                        offset=c(281,231,91,88,247,64,184,232,147,147,234,66,198,111,106,146,92,100), ## x-offset (fixed length for all kits)
##                        ID=c(rep(580,4),rep(390,5+1),rep(200,4),rep(10,3),NA), ## y-offset (kit dependent via dye lane) 
                        ID=c(rep(620,4),rep(420,5+1),rep(220,4),rep(20,3),NA), ## y-offset (kit dependent via dye lane) 
##                        SE=c(NA,NA,390,10,580,580,NA,580,200,200,10,200,NA,580,NA,200,390,390), ## y-offset (kit dependent via dye lane) 
                        SE=c(NA,NA,420,20,620,620,NA,620,220,220,20,220,NA,620,NA,220,420,420), ## y-offset (kit dependent via dye lane) 
##                        SGM=c(NA,NA,200,200,390,390,NA,390,10,10,200,10,NA,390,NA,10,200,NA), ## y-offset (kit dependent via dye lane) 
                        SGM=c(NA,NA,220,220,420,420,NA,420,20,20,220,20,NA,420,NA,20,220,NA), ## y-offset (kit dependent via dye lane) 
##                        PP=c(NA,10,200,200,NA,390,10,NA,NA,NA,200,NA,NA,390,10,390,200,NA)) ## y-offset (kit dependent via dye lane) 
                        PP=c(NA,20,220,220,NA,420,20,NA,NA,NA,220,NA,NA,420,20,420,220,NA)) ## y-offset (kit dependent via dye lane) 
  offsets <- offsets[,c("locus","offset",kit)]
  names(offsets)[3] <- "baseline"
  x <- merge(x,offsets,by="locus",all.x=TRUE)
  x$bp <- (x$offset-110)*4 + 16*round(x$allele)
  x$bp <- x$bp+40*(x$allele-round(x$allele))
  x
}

plotEPG <- function(x,color=TRUE,addProfile=FALSE,profiles=NULL,contributor=NULL,...){  
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
##   DD <- list("ID"=c("R"=10,"Y"=200,"G"=390,"B"=580),"SE"=c("R"=10,"Y"=200,"G"=390,"B"=580),
##              "SGM"=c("Y"=10,"G"=200,"B"=390),"PP"=c("Y"=10,"G"=200,"B"=390))
  DD <- list("ID"=c("R"=20,"Y"=220,"G"=420,"B"=620),"SE"=c("R"=20,"Y"=220,"G"=420,"B"=620),
             "SGM"=c("Y"=20,"G"=220,"B"=420),"PP"=c("Y"=20,"G"=220,"B"=420))
  DD <- DD[[kit]]
  kitcol <- as.data.frame(cbind("D"=rep(names(kits),unlist(lapply(kits,length))),"locus"=unlist(kits)))
  x$locus <- toupper(strtrim(unlist(lapply(strsplit(toupper(x$locus),split="S[1-9]"),function(z) z[1])),3))
  x <- merge(x,kitcol,by="locus")
  kitcolor <- data.frame("D"=c("B","G","Y","R"),color=c("#1e7ba6","#2ea61e","#ffe51d","#ef2f2f")) ## yellow was: #ffef41, blue was: #0076c9, green was: #1eb04e
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
  polygon(xx,expPeak,border=1,lty=1,lwd=1)
  abline(h=DD)
  ## Adding allele and locus designations to plot
  loci <- aggregate(x$allele,by=list(paste(x$locus)),median)
  names(loci) <- c("locus","allele")
  loci <- allele2bp(loci,kit=kit)
  text(loci$bp,loci$baseline-25,paste(loci$locus),cex=0.75,adj=c(0.5,1)) ## was: -15
  x$allele[x$locus=="AME" & x$allele=="1"] <- "X"
  x$allele[x$locus=="AME" & x$allele=="3"] <- "Y"
  text(x$bp,x$baseline-17,paste(x$allele),cex=0.75) ## was: -8
  #box()
  if(addProfile){ ## add profile table to plot
    addprofiles2plot(x=mean(rx),y=par("usr")[4],profiles=profiles,contributor=contributor,just=c(0.5,-0.6))
    ## Jakob plot
    addHooks2plot(profiles,kit=kit)
    ## 
  }
}

computeExpArea <- function(x,y,tauhat){
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
  data <- x$result$data #[,datacols]
  names(data) <- c("locus","allele","height","area")
  d <- merge(data,comb,by=c("locus","allele"))
  ds <- split(d,d$locus)
  m <- ncol(comb)-2
  N <- nrow(data)-length(ds)-(m-1)
  aa <- ahat(ds,m=m)
  tt <- that(ds,alpha=aa,m=m)
  R2 <- tauhat/tt
  r2 <- (tauhat/tt)^N
  expArea <- expectedAreas(split(d,d$locus),alpha=aa,m=m)
  list(data=expArea[,c("locus","allele",paste("P",1:m,sep=""),"exp")],alpha=aa,tau=tt,R2=R2,r2=r2,N=N)
}

## Modified from function addtable2plot in package 'plotrix'
addprofiles2plot <- function (x, y, profiles, contributor, cex = c(0.85,1), just = c(0,1)) {
  tabdim <- dim(profiles)
  if(is.null(tabdim)){ ## profiles is a vector
    loci <- names(profiles)
    profiles <- do.call("cbind",strsplit(profiles,"/"))
    colnames(profiles) <- loci
    rownames(profiles) <- ifelse(rep(is.null(contributor),nrow(profiles)),"",contributor)
    tabdim <- dim(profiles)
  }
  else{ ## profiles is a table
    loci <- colnames(profiles)
    contributor <- rownames(profiles)
  }
  if(is.null(tabdim)) return(NULL) ## something went wrong (profiles is neither table nor vector)
  mwidth <- strwidth("M", cex = cex[1])
  cellwidth <- max(strwidth(c(loci, contributor, as.vector(unlist(profiles))), cex = cex[1])) + mwidth
  nvcells <- tabdim[1] + 1
  nhcells <- tabdim[2] + 1
  cellheight <- max(strheight(c(loci, contributor, as.vector(unlist(profiles))), cex = cex[1])) * 2
  xleft <- x - just[1] * nhcells * cellwidth
  ytop <- y + just[2] * nvcells * cellheight
  for (row in 1:tabdim[1]) {
    if (row <= nvcells - 1){ 
      segments(xleft, ytop - row * cellheight, xleft + nhcells * cellwidth, ytop - row * cellheight, lwd = 1, col = 1)
      text(xleft + 0.5 * cellwidth, ytop - (row + 0.5) * cellheight, contributor[row], cex = cex[1], col = row)
      for (col in 1:tabdim[2]){
        text(xleft + (col + 0.5) * cellwidth, ytop - (row + 0.5) * cellheight, profiles[row, col], cex = cex[1], col = 1)
        text(xleft + (col + 0.5) * cellwidth, ytop - 0.5 * cellheight, loci[col],  cex = cex[1], col = 1, font = 2)
        segments(xleft, ytop - cellheight, xleft + nhcells * cellwidth, ytop - cellheight, lwd = 1, col = 1)
      }
    }
  }
  text(xleft + (nhcells * cellwidth)/2, ytop + cellheight/2, "Plotted profiles", cex = cex[2], col = 1, font = 2)
  segments(xleft, ytop, xleft + nhcells * cellwidth, ytop, lwd = 2, col = 1)
  segments(xleft, ytop - nvcells * cellheight, xleft + nhcells * cellwidth, ytop - nvcells * cellheight, lwd = 2, col = 1)
}

profileHook <- function(x,kit="SGM"){
  allele <- as.data.frame(cbind(locus=names(x),do.call("rbind",strsplit(x,","))))
  allele1 <- allele[,c(1,2)] ## locus and first allele
  allele2 <- allele[,c(1,3)] ## locus and second allele
  names(allele1) <- names(allele2) <- c("locus","allele")
  allele1 <- allele2bp(allele1,kit=kit)
  allele2 <- allele2bp(allele2,kit=kit)
  names(allele1) <- paste(names(allele1),"1",sep=".")
  names(allele2) <- paste(names(allele2),"2",sep=".")
  m.var <- c("locus","offset","baseline")
  allele <- merge(allele1,allele2,by.x=paste(m.var,"1",sep="."),by.y=paste(m.var,"2",sep="."))
  names(allele) <- c("locus","offset","baseline","allele1","bp1","allele2","bp2")
  allele
}

addHooks2plot <- function(x,kit="SGM"){
  x <- do.call("cbind",strsplit(x,"/"))
  colnames(x) <- toupper(strtrim(unlist(lapply(strsplit(toupper(colnames(x)),split="S[1-9]"),function(z) z[1])),3))
  x <- apply(x,1,profileHook,kit=kit)
  m <- length(x)
  bpskip <- ifelse(m==rep(2,m),c(-2,2),c(-2.5,0,2.5))
  for(j in 1:m){
    for(i in 1:nrow(x[[j]])){
      with(x[[j]][i,],lines(c(bp1,bp1,bp2,bp2)+bpskip[j],baseline-c(0,4,4,0)*j,col=j,lwd=2))
      if(x[[j]]$allele1[i]==x[[j]]$allele2[i]) with(x[[j]][i,],points(bp1+bpskip[j],baseline-4*j,pch=16,cex=0.8,col=j)) ## hom
    }
  }
}
