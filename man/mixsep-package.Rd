\name{mixsep-package}
\alias{mixsep-package}
\docType{package}
\title{
Forensic Genetics DNA Mixture Separation
}
\description{
Separates DNA mixtures using a statistical model within a greedy algorithm.
The use of the package is controlled by an user friendly GUI (Graphical
User Interface). The implementation is able to separate two- and
three-person DNA mixtures and provide diagnostic plots and parameter values.
}
\details{
\tabular{ll}{
Package: \tab mixsep\cr
Version: \tab 0.1-2\cr
Date: \tab 2011-01-06\cr
Depends: \tab R (>= 2.0.0), tcltk, tcltk2, MASS, RODBC\cr
License: \tab GPL version 2 or newer\cr
URL: \tab http://people.math.aau.dk/~tvede/mixsep\cr
}

Index:
\preformatted{
makeShortcut                                                Make Desktop shortcut on Windows operating system
mixsep                                                      Starts the DNA mixture separation
wang                                                        Two-person DNA mixture published by Wang et al. (2006)
}

After the package is loaded by library(mixsep) the GUI is started by
writing mixsep() in the R prompt. However, on Windows operating systems
it is recommended to use the makeShortcut()-function for easy and fast
execution of the user interface.

Please refer to http://people.math.aau.dk~/tvede/mixsep for further
details and descriptive guides and examples with screen shots.  }
\author{ Torben Tvedebrink <tvede@math.aau.dk>

Maintainer: Torben Tvedebrink <tvede@math.aau.dk>
}
\references{
  http://people.math.aau.dk~/tvede/mixsep
  
  T Tvedebrink, PS Eriksen, HS Mogensen and N Morling (2011). 'Identifying
  contributors of DNA mixtures by means of quantitative information of
  STR typing'. To Appear in 'Journal of Computational Biology'.

  T Tvedebrink, PS Eriksen, HS Mogensen and N Morling (2011). 'mixsep -
  An R-package for separating forensic DNA mixtures and performing
  statistical analysis of EPGs'. Manuscript in preparation.
}
\keyword{ package }
\seealso{
\code{\link[RODBC:RODBC-package]{RODBC}}
}
\examples{
## How to start the mixture separating tool in R: ##
  \dontrun{
   library(mixsep)
   mixsep()
   }
}
