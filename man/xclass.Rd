\name{xclass}
\alias{xclass}
\title{
  (Advanced) Construct R objects from XML document queries
}

\description{

  This function takes an xml object, created from \code{\link{readXml}},
  and an xpath query referencing an element of the document,and returns
  an R object represneting the element.

  The xpath query must retrive an element, not an attribute.

}
\usage{
xclass(doc, xpathq, prefix = character(0))
}
\arguments{
  \item{doc}{An \code{XMLInternalDocument} retrieved using
    \code{\link{readXml}}.}
  \item{xpathq}{A length-1 character string representing an xpath query
    into the document.}
  \item{prefix}{A prefix used to map, when necessary, between the XML
    class name and the corresponding R class. For \code{NetAffx}-based
    classes, the prefix is \sQuote{Affx}; no prefix is required for DTT
    or ARR documents.}
}
\value{
  An R object corresponding to the class(es) referenced by the xpath
  query.
}
\references{

  \url{http://www.w3.org/TR/xpath} provides a very useful description of
  xpath. Addiitonal references are in the vignette `Retrieving MAGE and
  ARR sample attributes'.

}
\author{Martin Morgan}
\seealso{
  
  The vignettes `Retrieving MAGE and ARR sample attributes' and
  `Annotation retrieval with NetAffxResource' for detailed illustration
  of use.

  \code{\link{readXml}} for appropriately reading XML documents.
}  
\examples{
example(readXml)
}
\keyword{manip}
