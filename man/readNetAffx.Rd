\name{readNetAffx}
\alias{readNetAffx}
\title{Update or read a NetAffx data base}
\description{

  This function provides a way to read or update a NetAffx data
  base. The usual use for this is to force the data base description to
  be updated (by accessing the Affymetrix web site) or to retrieve the
  entire list of NetAffx resource data base. A more normal use case is to

}
\usage{
readNetAffx(netAffxResource, update = FALSE, ...)
}
\arguments{
  \item{netAffxResource}{
    An object of class \code{\linkS4class{NetAffxResource}} containing
    the path to the local data base, and permissions for access the
    Affymetrix NetAffx resource.
  }
  \item{update}{
    An object of \code{logical(1)} indicating whether the data base is
    to be updated (\code{TRUE}) or not.
  }
  \item{\dots}{Additional arguments, not currently used.}
}
\value{
  An object representing the annotations found; see
  \code{\linkS4class{NetAffxResource}} for additional details.
}
\author{Martin Morgan}
\examples{
## retrieve all annotations
\dontrun{
example("NetAffxResource-class")
readNetAffx(rsrc)
}
}
\keyword{database}
