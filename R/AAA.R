## Functions used after class definition, to define accessors, by
## default, to all slots not beginning with .*

.nameAll <- function(x) {
    ## Add names to character vector x.  Elements of x without names get
    ## a name matching the element.
    ##
    if (is.null(x)) return(character(0))
    else if (!is.character(x))
      stop("argument 'x' must be a character vector")
    names(x) <- 
      if (length(names(x)) > 0)
        ifelse(nzchar(names(x)), names(x), x)
      else
        x
    x
}

.newSlotNames <- function(class) {
    ## slot name sdefined in this class, not inheritted
    nms <- names(slot(getClass(class), "contains"))
    ext <- unique(unlist(lapply(nms, slotNames)))
    nms <- slotNames(class)
    nms[!nms %in% ext]
}

.accessors <- function(class, slots=.newSlotNames(class),
                       where=topenv(parent.frame()), ...) {
    .getters(class, slots, where, ...)
    .setters(class, slots, where, ...)
}

.getters <- function(class, slots=.newSlotNames(class),
                     where=topenv(parent.frame()), ...) {
    slots <- .nameAll(slots)
    nms <- names(slots)
    for (i in seq_along(slots)) {
        tryCatch({
          eval(substitute({
            if (!isGeneric(GENERIC, where=where) &&
                GENERIC != "length")
                setGeneric(GENERIC,
                           function(x) standardGeneric(GENERIC),
                           useAsDefault=FALSE,
                           where=WHERE)
            if (!existsMethod(GENERIC,
                              signature=signature(x=CLASS),
                              where=where))
              setMethod(GENERIC,
                        signature=signature(x=CLASS),
                        function(x) slot(x, SLOT),
                        where=WHERE)
        }, list(CLASS = class,
                GENERIC = nms[[i]],
                SLOT = slots[[i]],
                WHERE = where)))
        }, error=function(err) {
          warning(sprintf("could not create '%s': %s",
                          nms[[i]], conditionMessage(err)))
        })
    }
}

.setters <- function(class, slots=.newSlotNames(class),
                     where=topenv(parent.frame()), ...) {
    slots <- .nameAll(slots)
    nms <- names(slots)
    for (i in seq(along=slots)) {
        try(eval(substitute({
            if (!isGeneric(SETTER, where=where))
                setGeneric(SETTER, function(x, value)
                           standardGeneric(SETTER),
                           where = WHERE)
            setReplaceMethod(GENERIC,
                             signature=signature(
                               x=CLASS,
                               value=getSlots(CLASS)[[SLOT]]),
                             function(x, value) {
                                 slot(x, SLOT) <- value
                                 validObject(x)
                                 x
                             },
                             where = WHERE)
        }, list(CLASS=class,
                GENERIC=nms[[i]],
                SETTER=paste(nms[[i]], "<-", sep=""),
                SLOT=slots[[i]],
                WHERE=where))))
    }
}

## .validity

.validity <- function(object) TRUE

setGeneric(".validity")

## .TypedAssociation

setClass(".TypedAssociation",
         representation=representation(
           .extends="character"),
         contains="list",
         validity=.validity)

.TypedAssociation <- function(extends) {
  new(".TypedAssociation", .extends=extends)
}
