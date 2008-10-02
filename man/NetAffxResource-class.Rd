\name{NetAffxResource-class}
\docType{class}
\alias{NetAffxResource-class}
\alias{[[,NetAffxResource,numeric,missing-method}
\alias{[[,NetAffxResource,numeric,character-method}
\alias{[[,NetAffxResource,numeric,numeric-method}
\alias{[[,NetAffxResource,character,missing-method}
\alias{[[,NetAffxResource,character,numeric-method}
\alias{[[,NetAffxResource,character,character-method}
\alias{affxLicence,NetAffxResource-method}
\alias{affxUrl,NetAffxResource-method}
\alias{annotationsFile,NetAffxResource-method}
\alias{directory,NetAffxResource-method}
\alias{names,NetAffxResource-method}
\alias{password,NetAffxResource-method}
\alias{readAnnotation}
\alias{readAnnotation,NetAffxResource,character,character-method}
\alias{readAnnotation,NetAffxResource,character,numeric-method}
\alias{readAnnotation,NetAffxResource,missing,AffxAnnotation-method}
\alias{readAnnotation,NetAffxResource,numeric,character-method}
\alias{readAnnotation,NetAffxResource,numeric,numeric-method}
\alias{show,NetAffxResource-method}
\alias{user,NetAffxResource-method}

\title{Manage NetAffx annotations}
\description{This class is for accessing and retrieving NetAffx
  annotation files for Affymetrix chips. It requires that the user has
  established an account with Affymetrix. Detailed usage is described in
  the `Annotations with NetAffx' vignette.}
\section{Objects from the Class}{
  Objects are created by calls of the form
  \code{NetAffxResource(user="user@research.org", password="terces")},
  as illustrated below and in the `Annotations with NetAffx vignette.
}
\section{Slots}{
  \describe{
    \item{\code{user}:}{Object of class \code{"character"} containing
      the full user name (typically an email address) obtained when the
      user registers with Affymetrix.}
    \item{\code{password}:}{Object of class \code{"character"}
      containing the password associated with the Affymetrix user
      name. The password is entered and stored in plain text, and should
      not be considered secure within Bioconductor.}
    \item{\code{affxLicence}:}{Object of class \code{"character"}
      containing the licence, obtained from Affymetrix, for retrieving
      NetAffx annotations. The licence is usually software specific
      (e.g., for the AffyCompatible package of Bioconductor). Users will
      not usually change the defeault value.}
    \item{\code{directory}:}{Object of class \code{"character"},
      containing the local directory path where the NetAffx data base
      and annotation files will reside.}
    \item{\code{annotationsFile}:}{Object of class \code{"character"}
      containing the file name of the NetAffx data base. Users will not
      usually change the default value.}
    \item{\code{affxUrl}:}{Object of class \code{"character"} containing
      the URL used to download the NetAffx annotations file. Users will
      not usually change the default value.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{NetAffxCompatibility}"}, directly.
Class \code{"\linkS4class{AffyCompatible}"}, by class "NetAffxCompatibility", distance 2.
}
\section{Methods}{
  \describe{
    \item{[[}{
      \code{signature(x = "NetAffxResource", i = "numeric", j = "missing")}}
    \item{[[}{
      \code{signature(x = "NetAffxResource", i = "character", j = "missing")}:
      Retrieve an object representing all NetAffx annotations
      available for an array. The argument \code{i} may be a numeric index
      or character string corresponding to an array name returned by
      \code{names(x)}.
    }
    \item{[[}{
      \code{signature(x = "NetAffxResource", i = "numeric", j = "numeric")}}
    \item{[[}{
      \code{signature(x = "NetAffxResource", i = "character", j = "numeric")}}
    \item{[[}{
      \code{signature(x = "NetAffxResource", i = "numeric", j = "character")}}
    \item{[[}{
      \code{signature(x = "NetAffxResource", i = "character", j = "character")}:
      Retrieve an object representing a specific annotation for an
      array. The argument \code{i} may be a numeric index or
      character string corresponding to an array name returned by
      \code{names(x)}. The argument \code{i} may be a numeric index or
      character string corresonding to an annotation returned by
      \code{description(x[[i]])}.
    }
    \item{affxLicence}{\code{signature(x = "NetAffxResource")}: 
      the character string representation of the licence used to access
      the NetAffx site.}
    \item{affxUrl}{\code{signature(x = "NetAffxResource")}: the
      url used to access the NetAffx site.}
    \item{annotationsFile}{\code{signature(x = "NetAffxResource")}: the
      file name of NetAffx data base, as stored on the local disk of the
      user.}
    \item{directory}{\code{signature(x = "NetAffxResource")}: the
      directory in wich the NetAffx data base and annotation files are
      stored on the local disk of the user.}
    \item{names}{\code{signature(x = "NetAffxResource")}: the names of
      the arrays for which annotations are available.}
    \item{password}{\code{signature(x = "NetAffxResource")}: The
      password associated with the user name obtained when the user
      registered with Affymetrix. The password is entered and stored as
      plain text, and is not secure.}
    \item{readAnnotation}{
      \code{signature(netAffxResource = "NetAffxResource", array = "numeric", annotation = "numeric", ...)}}
    \item{readAnnotation}{
      \code{signature(netAffxResource = "NetAffxResource", array = "numeric", annotation = "character", ...)}}
    \item{readAnnotation}{
      \code{signature(netAffxResource = "NetAffxResource", array = "character", annotation = "numeric", ...)}}
    \item{readAnnotation}{
      \code{signature(netAffxResource = "NetAffxResource", array = "character", annotation = "character", ...)}}
    \item{readAnnotation}{
      \code{signature(netAffxResource = "NetAffxResource", array = "missing", annotation = "AffxAnnotation", ...)}: 
      Read a specific anntotation file as an R object. for the first
      four methods, \code{array}, if present, indexes or names the array
      while \code{annotation} indexes or names the
      annotation. All methods also accept \code{content = "logical"} to
      indicate whether the content of the file should be returned if
      possible (\code{content=TRUE}, default) or only the path to the local file
      (\code{content=FALSE}) and \code{update = "logical"} indicating
      whether the file should be read from disk if available
      (\code{update=FALSE}, default) or retrieved from the NetAffx web
      site (\code{update=TRUE}). \code{readAnnotation} parses \code{CSV,
	Tabular, PSI} to data frames, and reads \code{FASTA} files to a
      list of description / sequence pairs (see
      \code{Biostrings::readFASTA}).
    }
    \item{show}{\code{signature(object = "NetAffxResource")}: compactly
      show the \code{NetAffxResource} object.}
    \item{user}{\code{signature(x = "NetAffxResource")}: The user name
      (typcially email address) obtained when the user registers with
      the Affymetrix web site.}
  }
}
\author{Martin Morgan <mtmorgan@fhcrc.org>}
\seealso{
  Vignette `Annotations with NetAffx'. Affymetrix user login site
  \url{https://www.affymetrix.com/site/login/login.affx}.

  \code{\link{.Rprofile}} for ways to automatically set variables (such
  as \code{password}, and \code{directory}) for each session.
}
\examples{
\dontshow{
    password="jnkdna"
}
## password must be obtained by the user
rsrc <- NetAffxResource(user="mtmorgan@fhcrc.org", password=password)
head(names(rsrc))
affxDescription(rsrc[["Bovine"]])
## paradigm to display all elements of 'annotation'
sapply(affxAnnotation(rsrc[["Bovine"]]), force)
## obtain and read a single annotation
anno <- rsrc[["Porcine", "BLASTP annotations, CSV Format"]]
df <- readAnnotation(rsrc, annotation=anno)
}
\keyword{classes}
