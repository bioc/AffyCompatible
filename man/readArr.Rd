\name{readArr}
\alias{readArr}
\title{Read ARR sample attribute files}
\description{

  This functions reads the content of Affymetrix ARR sample attribute
  files created by GeneChip(R) Command Console. Objects are represented
  as R classes. Navigation and extraction of data are described in the
  vignette `Retrieving MAGE and ARR sample attributes'.

}
\usage{
readArr(fls, ...)
}
\arguments{
  \item{fls}{A character vector containing the paths to files to be
    read.} 
  \item{\dots}{Additional arguments, currently unused.}
}
\value{
  An object or list of objects of class \code{ArrayFileSet}, depending
  on whether one or several file paths are provided. 
}
\author{Martin Morgan}
\seealso{

  The vignette `Retrieving MAGE and ARR sample attributes' for detailed
  illustration of use.

  \code{\link{xclass}} and \code{\link{readXml}} for advanced access
  methods. 

}
\examples{
dir <- system.file("extdata", "ARR", package="AffyCompatible")
fls <- list.files(dir, full=TRUE)
readArr(fls[[1]])
readArr(fls)
}
\keyword{classes}
