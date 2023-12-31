\name{xmlValue.XMLAttributeValue}
\alias{xmlValue.XMLAttributeValue}
\title{Additional functions defined in AffyCompatible}
\description{
  This page documents additional, miscellaneous, functions defined by
  AffyCompatible
}
\usage{
\method{xmlValue}{XMLAttributeValue}(x, ignoreComments = FALSE, recursive = TRUE,
    encoding=XML:::CE_NATIVE, trim=FALSE)
}
\arguments{
  \item{x}{An object of class \code{XMLAttributeValue}.}
  \item{ignoreComments}{Logical; ignored by the 
    \code{xmlValue.XMLAttributeValue} method}
  \item{recursive}{ignored}
  \item{encoding}{File encoding}
  \item{trim}{Remove trailing whitespace? Ignored by this method}
}
\value{
  A character vector (usually lenth 1) containing the character
  representation of the XML attribute.
}
\keyword{manip}
