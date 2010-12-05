\name{makeShortcut}
\alias{makeShortcut}
\concept{Genetics}
\concept{Deconvolution}
\concept{Forensic Genetics}
\title{Make Desktop shortcut on Windows operating system}
\description{
  Creates a shortcut on the desktop - Windows only.
}
\usage{makeShortcut(dbfile="",wang2desktop=FALSE)}
\arguments{
  \item{dbfile}{A file containing the information needed to connect to a
    database with the necessary data. The file is expected to have four
    (4) rows of the form:
    db = "Connection information to the database"
    dbtab = "Which table should be attached"
    dbcase = "The case containing the unique 'case' identifier used to
    extract case specific information" 
    dbcols = "Additional database columns needed to uniquely identify
    different PCR runs, re-runs, etc. with the same 'case' identifier"
    However, lines with comments (identified by # are allowed). If a
    variable has more arguments these are separated by semicolons
    ";", e.g. dbcols = "PCR_run; Replicate".
  }
  \item{wang2desktop}{Logical: If TRUE the example data file 'wang.csv'
    is copied to the Windows desktop. Default is FALSE which only
    creates a shortcut for lauching the mixture separator.}
}
\value{
  Creates a shortcut on the desktop. The function identifies the
  location of the mixsep-package in the file system and adds a
  .bat-file, which launches R in the terminal. This is the best and
  easiet way of using the mixture separator.
}
\examples{
## run this in Windows to create shortcut on the desktop. 
\dontrun{makeShortcut()}
}

