makeShortcut <- function(){
  if(length(grep("window",tolower(.Platform$OS.type)))>0){ ## Windows
    mixsepdir <- .find.package("mixsep")
    ## Create runmixsep.R script
    cat(" library(mixsep)\n mixsep()",file=paste(mixsepdir,"R","runmixsep.R",sep=.Platform$file.sep)) 
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
             objMixsepShortcut.Save \n', sep=""), file="shortcut.vbs")
    ## Create shortcut by calling VB script:
    shell("CSCRIPT shortcut.vbs")
    ## Remove VB script:
    file.remove("shortcut.vbs")
  }
  else stop("Creation of shortcut is only possible for Windows operating systems")
}
