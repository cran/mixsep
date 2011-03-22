\name{mixsep}
\alias{mixsep}
\concept{Genetics}
\concept{Deconvolution}
\concept{Forensic Genetics}
\title{DNA mixture separation}
\description{
  Separates DNA mixtures using the algorithm and statistical model
  described in Tvedebrink et al. (2010), see references. The
  mixsep()-function invokes a GUI written in the tcl/tk-language. The
  GUI makes it easy to load data files, analyse the mixture with fixed
  profiles and interpret the results. Specialised plotting functions are
  made available for visual inspection of the data and proposed DNA
  profiles.

  See also http://people.math.aau.dk/~tvede/mixsep/ for further details
  and guides with screen shots. 
}
\usage{mixsep()}
\value{
  Results of the mixture separation.
}
\references{
  T Tvedebrink, PS Eriksen, HS Mogensen and N Morling (2011). 'Identifying
  contributors of DNA mixtures by means of quantitative information of
  STR typing'. To Appear in 'Journal of Computational Biology'. 
}
\examples{
## The file wang.csv in the installation directory may be used for exemplification.
## The 'wang.csv' is located in the folder quoted below:
cat(paste(.find.package("mixsep"),"data",sep=.Platform$file.sep))
\dontrun{mixsep()}
}

