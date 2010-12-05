\name{makeShortcut}
\alias{makeShortcut}
\concept{Genetics}
\concept{Deconvolution}
\concept{Forensic Genetics}
\title{Make Desktop shortcut on Windows operating system}
\description{
  Creates a shortcut on the desktop - Windows only. If the arguments are
  used the function enables the connection to a database (dbfile) and
  copies the sample-file 'wang.csv' to the desktop
  (wang2desktop). Please see below (and details) for description of
  using RODBC to connect to a database.
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
\details{
  The package RODBC enables R to connect to a ODBC-connection. ODBC
  connections can either be connections to data files (e.g. Access- or
  Excel-files) or it can be a proper database. The mixsep() GUI connects
  to the ODBC source using the odbcDriverConnect-function, which takes
  different arguments depending on the data source. It is recommended to
  verify that the 'db'-argument provided in the 'dbfile'-file enables the
  connection to the database prior to using makeShortcut. Please refer
  to the help pages of the RODBC-package for further details.
}
\seealso{odbcDriverConnect}
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

