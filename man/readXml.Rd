\name{readXml}
\alias{readXml}

\title{

  (Advanced) Read XML files of Affymetrix sample attriubtes or
  annotation

}

\description{
  
  This functions reads the content of Affymetrix DTT (MAGE) or ARR
  sample files, or AffxNetAffxAnnotFile array annotation files, return
  an internal representation of the underlying XML. The return value can
  be used with \code{\link{xclass}} to rapidly create R objects
  representing deeply nested nodes of the document. The return value can
  also be used with \code{xpathApply} in the XML package for
  access to character-based representation of the data.

}
\usage{
readXml(fl, ...)
}
\arguments{
  \item{fl}{A length-1 character vector giving the full path to the file
    to be parsed.}
  \item{\dots}{Additional arguments, passed to
    \code{xmlTreeParse}.}
}
\value{
  An object of class \code{XMLInternalDocument}.
}
\author{Martin Morgan}
\seealso{
  
  The vignettes `Retrieving MAGE and ARR sample attributes' and
  `Annotation retrieval with NetAffxResource' for detailed illustration
  of use.

  \code{\link{xclass}} for instantiating objects from xpath queries.
}
\examples{
mageDir <- system.file("extdata", "DTT", package="AffyCompatible")
mageFiles <- list.files(mageDir, full=TRUE)
xml <- readXml(mageFiles[[1]])
xclass(xml, "//MeasuredBioAssay")[[1]]
}
\keyword{manip}
