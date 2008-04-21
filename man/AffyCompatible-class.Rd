\name{AffyCompatible-class}
\docType{class}
\alias{zzz-AffyCompatible-class}
\alias{AffyCompatible-class}
\alias{DTTCompatibility-class}
\alias{ARRCompatibility-class}
\alias{NetAffxCompatibility-class}
\alias{show,AffyCompatible-method}

\title{Virtual classes defining the "AffyCompatibility" hierarchy}
\description{

  The classes \code{AffyCompatible}, \code{DTTCompatibility},
  \code{ARRCompatibility}, and \code{NetAffxCompatibility} define the
  overall class hierarchy in this package. Information about these
  classes is not usually relevant to the user.

}
\section{Objects from the Class}{
  These are virtual classes: no objects may be created from them.
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "AffyCompatible")}:
    for each slot, provide the slot name, and then invoke \code{show}
    for  the slot content.}
  }
}
\author{Martin Morgan}
\examples{
showClass(getClass("NetAffxCompatibility",
    where=getNamespace("AffyCompatible")))
}
\keyword{classes}
