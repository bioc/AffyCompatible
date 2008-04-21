\name{zzz-NetAffxCompatibility-class}
\docType{class}
\alias{zzz-NetAffxCompatibility-class}

\alias{affxAnnotation}
\alias{affxArray}
\alias{affxComment}
\alias{affxCompression}
\alias{affxCrc32}
\alias{affxDate}
\alias{affxDescription}
\alias{affxFile}
\alias{affxLicence}
\alias{affxName}
\alias{affxSize}
\alias{affxStatus}
\alias{affxType}
\alias{affxUrl}

\alias{affxArray,AffxNetAffxAnnotFileList-method}
\alias{affxComment,AffxAnnotation-method}
\alias{affxDate,AffxFile-method}
\alias{affxFile,AffxAnnotation-method}
\alias{affxUrl,AffxFile-method}
\alias{affxAnnotation,AffxArray-method}
\alias{affxCompression,AffxUrl-method}
\alias{affxCrc32,AffxFile-method}
\alias{affxCrc32,AffxUrl-method}
\alias{affxDescription,AffxAnnotation-method}
\alias{affxDescription,AffxArray-method}
\alias{affxName,AffxArray-method}
\alias{affxName,AffxFile-method}
\alias{names,AffxArray-method}
\alias{affxSize,AffxFile-method}
\alias{affxSize,AffxUrl-method}
\alias{affxStatus,AffxNetAffxAnnotFileList-method}
\alias{affxType,AffxAnnotation-method}
\alias{affxType,AffxFile-method}

\title{

  (Auto-generated) classes and methods for navigating NetAffx resources

}
\description{

  These classes and methods facilitate navigation of the NetAffx
  resource data base.  They are R representations of the Affymetrix
  classes defined in NetAffxAnnotFileList.dtd. The 'show' methods for
  these classs indicate how objects are to be accessed; details are in
  the `Annotations with NetAffx' vignette.

  The auto-generated classes are discoverable as indicated in the
  example below.

}
\section{Objects from the Class}{

  Objects are created internally, by parsing XML retrieved from the
  local NetAffx data base or from the Affymetrix web site.

}
\section{Slots}{

  Objects have a variety of slots. To discover slot names and content
  type, see the \code{slotNames} portion of the example, below.

}
\section{Extends}{
Class \code{"\linkS4class{NetAffxCompatibility}"}, directly.
Class \code{"\linkS4class{AffyCompatible}"}, by class "NetAffxCompatibility", distance 2.
}
\section{Methods}{

  'Read' methods are defined on auto-generated classes to access slot
  content.

  Accessors are apparent from displaying the object; each line of the
  display is of the form \code{accessor: content}. To retrieve the
  content of an instance \code{object}, invoke \code{accessor(object)}.

  Some objects contains several instances of another object. These
  'typed associations' are represented by the
  \code{\linkS4class{.TypedAssociation}} class, and are navigated or
  subset as one would a standard R vector, e.g., using \code{[},
  \code{[[}. A useful paradigm for displaying all instances is
  illustrated on the \code{\linkS4class{NetAffxResource}} page.

}
\author{Martin Morgan <mtmorgan@fhcrc.org>}
\seealso{
  Vignette `Annotations with NetAffx'. Affymetrix user login site
  \url{https://www.affymetrix.com/site/login/login.affx}.

  \code{\link{NetAffxResource}} for creating and managing the NetAffx
  data base.

  \code{\linkS4class{NetAffxResource}} for navigating the data base.

}

\examples{
## defined and auto-generated classes
getClass("NetAffxCompatibility", where=getNamespace("AffyCompatible"))

## slots of an auto-generated class
getSlots(getClass("AffxNetAffxAnnotFileList",
    where=getNamespace("AffyCompatible")))

\dontrun{
example("NetAffxrResource-class")
}
}

\keyword{classes}
