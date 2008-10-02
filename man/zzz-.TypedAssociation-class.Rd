\name{Internal-class}
\docType{class}
\alias{zzz-.TypedAssociation-class}
\alias{.TypedAssociation-class}
\alias{[,.TypedAssociation,ANY,ANY,ANY-method}
\alias{[,.TypedAssociation,numeric,missing,ANY-method}
\alias{show,.TypedAssociation-method}

\title{(Internal) Represent 'typed associations' from XML documents}
\description{

  This internal class represents `typed associates' (vectors of
  identically-typed objects) that appear as vectors to the R user.

}
\section{Objects from the Class}{
  Objects are created internally, during XML document parsing.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} containg the
      association objects}
    \item{\code{.extends}:}{Object of class \code{"character"} and
      length 1 describing the class each element in \code{.Data} must
      extend}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = ".TypedAssociation", i = "numeric", j = "missing")}:
      Subset the typed association, returning a typed association with
      elements i.
    }
    \item{[}{\code{signature(x = ".TypedAssociation", i = "ANY", j = "ANY")}:
      This method is present to stop access to subsetting methods for
      class \code{list}; it has no user-visibile functionality.
    }
    \item{show}{\code{signature(object = ".TypedAssociation")}:
      Compactly display the typed associate, e.g., for
      \code{.extneds=Foo} and length 5, display \code{Foo(5)}.
    }
  }
}
\author{Martin Morgan}
\keyword{classes}
