.TypedAssociation_validity <- function(object) {
  msg <- NULL
  extends <- .extends(object)
  if (!all(sapply(object, is, extends)))
    msg <- c(msg,
             sprintf("all elements must extend '%s'", extends))
  if (is.null(msg)) TRUE
  else msg
}

setMethod(".validity",
          signature=c(object=".TypedAssociation"),
          .TypedAssociation_validity)

.getters(".TypedAssociation")

.TypedAssociation <- function(extends, ...) {
  new(".TypedAssociation", .extends=extends, ...)
}

setMethod("[",
          signature=signature(
            x=".TypedAssociation"),
          function(x, i, j, ..., drop=TRUE) {
            stop("'[' with subscript(s) i='",
                 if (missing(i)) "missing" else class(i),
                 "', j='",
                 if (missing(j)) "missing" else class(j),
                 "' not supported")
          })

setMethod("[",
          signature=signature(
            x=".TypedAssociation",
            i="numeric",
            j="missing"),
          function(x, i, j, ..., drop=TRUE) {
            if (length(x)==0)
              stop("cannot subset 0-length '", .extends(x), "'")
            if (any(!i %in% seq(1, length(x))))
              stop("'[' subscript i must be in 1:", length(x))
            slot(x, ".Data") <- .Data(x)[i]
            x
          })

setMethod("show",
          signature=signature(
            object=".TypedAssociation"),
          function(object) {
            cat(.extends(object), "(", length(object), ")\n", sep="")
          })
