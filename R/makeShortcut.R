makeShortcut <- function(dbfile="",wang2desktop=FALSE){
#  if(grepl("win",tolower(.Platform$OS.type))){ ## Windows
    mixsepdir <- .find.package("mixsep")
    ## Create runmixsep.R script
    if(dbfile!=""){ ## If a file of DB connector information is given:
      dblines <- readLines(file(dbfile,'r')) 
      comments <- unlist(lapply(dblines,grepl,pattern="#"))
      ## removes comments from lines/or entire comment lines
      if(any(comments)){
        dblines[comments] <- unlist(lapply(strsplit(dblines[comments],"#"),function(x) x[1]))
      }
      ## removes leading and tailing white space
      dblines <- gsub("^\\s+|\\s+$", "", dblines)
      dblines <- dblines[dblines!=""]
      if(length(dblines)!=4){
        warning("Something went wrong when setting database parameters")
        cat(" library(mixsep)\n mixsep()",file=paste(mixsepdir,"R","runmixsep.R",sep=.Platform$file.sep))
      }
      else{
        ## Locates the line with 'db'-connection information
        dbconnect <- rep(0,4)
        for(line in c("dbtab","dbcase","dbcols")) dbconnect <- dbconnect + grepl(line,dblines)
        dbconnect <- dblines[!dbconnect]
        browser()
#        dbAttempt <- try(odbcDriverConnect(dbconnect),silent=TRUE)
#        if(class(dbAttempt)=="RODBC"){
#          tkmessageBox(title="Database connection successful",message="Database connection successfully established",icon="info")
          ## Writes information to runmixsep.R
          dbcols <- grepl("dbcols",dblines)
          dbcol <- unlist(lapply(strsplit(dblines[dbcols],"="),function(x) x[2]))
          dbcol <- gsub("^\\s+|\\s+$","",unlist(strsplit(gsub("\"", "", dbcol),";")))
          dblines[dbcols] <- paste("dbcols = c(\"",paste(gsub("^\\s+|\\s+$","",unlist(strsplit(gsub("\"", "", dbcol),";"))),collapse="\",\"",sep=""),"\")",sep="",collapse="")
          cat(" library(mixsep)\n options(mixsep=list(",paste(dblines,collapse=","),"))\n mixsep()",file=paste(mixsepdir,"R","runmixsep.R",sep=.Platform$file.sep))
#        }
#        else{
#          ## DB connection failed - This disables [DB] in GUI
#          tkmessageBox(title="Database connection refused",message="Connection to the database refused",icon="error")
#          cat(" library(mixsep)\n mixsep()",file=paste(mixsepdir,"R","runmixsep.R",sep=.Platform$file.sep))
#        }
 #       odbcClose(dbAttempt)
      }
    } ## If no DB connector is supplied - This disables [DB] in GUI
    else cat(" library(mixsep)\n mixsep()",file=paste(mixsepdir,"R","runmixsep.R",sep=.Platform$file.sep))
    ## Create .bat-file:
    cat(paste(' @echo off \n start /MIN ', paste(R.home(),"bin","Rterm.exe",sep=.Platform$file.sep), ' -q -f ',
               paste(mixsepdir,"R","runmixsep.R",sep=.Platform$file.sep), '\n'),
        file=paste(mixsepdir, "R", "mixsep.bat",sep=.Platform$file.sep))
    ## Create Visual Basic Script for shortcut creation:
    cat(paste('Set objShell = WScript.CreateObject("WScript.Shell") \n
             strDesktopFld = objShell.SpecialFolders("Desktop") \n
             strMyDocumentFld = objShell.SpecialFolders("MyDocuments") \n
             Set objMixsepShortcut = objShell.CreateShortcut(strDesktopFld & "\\mixsep.lnk") \n
             objMixsepShortcut.TargetPath = "', mixsepdir, '/R/mixsep.bat" \n
             objMixsepShortcut.WorkingDirectory = strMyDocumentFld \n
             objMixsepShortcut.Save \n', sep=""), file=paste(mixsepdir,"R","shortcut.vbs",sep=.Platform$file.sep))
    ## Check if user wants path/to/mixsep/package/data/wang.csv copied to the desktop:
    if(wang2desktop){
      wangfile <- paste(mixsepdir,"data","wang.csv",sep=.Platform$file.sep)
      cat(paste('\n\n dim filesys \n
      set filesys=CreateObject("Scripting.FileSystemObject") \n
      If filesys.FileExists(\"',wangfile,'\") Then \n
      filesys.CopyFile \"',wangfile,'\", strDesktopFld + "',.Platform$file.sep,'", true \n
      End If',sep=""), file=paste(mixsepdir,"R","shortcut.vbs",sep=.Platform$file.sep), append=TRUE)
    }
    ## Create shortcut by calling VB script:
    shell(paste("CSCRIPT",paste(mixsepdir,"R","shortcut.vbs",sep=.Platform$file.sep)))
    ## Remove VB script:
    file.remove(paste(mixsepdir,"R","shortcut.vbs",sep=.Platform$file.sep))
#  }
#  else stop("Creation of shortcut is only possible for Windows operating systems")
}
