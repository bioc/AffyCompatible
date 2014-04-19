\name{NetAffxResource}

\alias{NetAffxResource}
\alias{acpassword}

\title{Create and access a NetAffxResource database}
\description{

  Use this function to create and access a NetAffxResource database.

}
\usage{
NetAffxResource(user = character(0),
    password = character(0),
    affxLicence = "FHCRC0607",
    directory = tempdir(), ...)
}

\arguments{

  \item{user}{Lenght 1 character vector containing the Affymetrix user
    name authorized to access the NetAffx resource.}
  \item{password}{Length 1 character vector containing the Affymetrix
    password of the user.}
  \item{affxLicence}{Length 1 character vector containing the Affymetrix
    licence (typically application specific, i.e., the specified licence
    covers AffyCompatible).}
  \item{directory}{(Optional) length 1 character vector providing the
    full path to the local directory where the NetAffx database and
    files will be managed. Defaults to a session-specific temporary
    directory.}
  \item{\dots}{Additional arugments, not currently used.}
}
\details{

  \code{NetAffxResource} creates a \code{\linkS4class{NetAffxResource}} object that
  manages a data base of NetAffx resources. The data base is located on
  the user file system as indicated by the \code{directory} arugment. It
  is populated when the user requests specific files.

  Users should provide a \code{directory} argument, so that the data
  base can be reused across sessions. This means that large files are
  only downloaded once.

  Additional details are provided in the vignette accompanying this
  package.
  
}
\value{
  \code{\linkS4class{NetAffxResource}}.
}
\author{Martin Morgan}
\seealso{
  \code{\linkS4class{NetAffxResource}}. The vignette `Annotations with
  NetAffx'.
}

\keyword{database}
