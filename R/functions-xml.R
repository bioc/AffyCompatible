.xreserved <- function()
  c( ## attributes
    'class', 'comment', 'dim', 'dimnames', 'names',
    'row.names', 'tsp',
    ## incorrect implict generic
    'order', 'row', 'array', 'sequence', 'url', 'file',
    'title', 'text', 'person', 'image', 'start', 'end',
    ## no suitable arguments to dispatch methods
    'date', 'category')

.xnames <- function(names, prefix=character(0)) {
    ## how element names are mapped to slot names -- lowercase, except for reserved
    if (length(prefix)==1) {
        idx <- grep(paste("^",  prefix, sep=""), names, perl=TRUE)
        updt <- setdiff(seq_len(length(names)), idx)
        names[updt] <- sub("^([A-Z]+)", "\\L\\1", names[updt], perl=TRUE)
        repl <- paste(prefix, "\\U\\1", sep="")
        names[updt] <- sub("^([[:alpha:]])", repl, names[updt], perl=TRUE)
    }
    names <- sub("^([A-Z]+)", "\\L\\1", names, perl=TRUE)
    idx <- names %in% .xreserved()
    names[idx] <- sub("^([[:alpha:]])", "affx\\U\\1", names[idx], perl=TRUE)
    names
}

.xclassnames <- function(cls, prefix=character(0)) {
    if (length(prefix)==1) {
        cls <- .xnames(cls, prefix)
        sub("^([[:alpha:]])", "\\U\\1", cls, perl=TRUE)
    } else {
        cls
    }
}

.xvalue <- function(xmlNode, xpathq) {
  unlist(xpathApply(xmlDoc(xmlNode), xpathq, xmlValue))
}

.xpcdata <- function(xmlNode) {
  ## FIXME: comments?
  xpathApply(xmlDoc(xmlNode), "/*/text()", xmlValue)
}

.xattrs <- function(xmlNode, prefix) {
  attrs <- as.list(xmlAttrs(xmlNode))
  names(attrs) <- .xnames(names(attrs), prefix)
  attrs
}

.xassn <- function(xmlNode, xpath, type_constructor, assn_constructor) {
  types <- xpathApply(xmlDoc(xmlNode), xpath, type_constructor)
  assn_constructor(types)
}

.xclass <- function(node, ..., prefix) {
  if (!is(node, "XMLInternalNode"))
    stop("'node' must be of class 'XMLInternalNode'")
  nodeNm <- .xclassnames(xmlName(node), prefix)
  pcdata <- list()
  if (".Data" %in% slotNames(getClass(nodeNm)))
    pcdata <- .xpcdata(node)
  attrs <- .xattrs(node, prefix)
  assnNodes <- xpathApply(xmlDoc(node), "/*/*")
  if (length(assnNodes)>0) {
    nms <- .xclassnames(sapply(assnNodes, xmlName), prefix=prefix)
    elts <- lapply(assnNodes, .xclass, prefix=prefix)
    assnNms <- unique(nms)
    assns <- lapply(assnNms, function(nm, ...) {
      new(".TypedAssociation", elts[nms==nm], .extends=nm)
    })
    names(assns) <- .xnames(assnNms, prefix)
  } else {
    assns <- list()
  }
  do.call("new", c(nodeNm, pcdata, attrs, assns))
}

xclass <- function(doc, xpathq, prefix=character(0)) {
  xpathApply(doc, xpathq, .xclass, prefix=prefix)
}

.readXmlAsClass <- function(fl, ..., prefix=character(0)) {
  xml <- xmlTreeParse(fl, useInternal=TRUE, ...)
  xclass(xml, "/*", ..., prefix=prefix)
}

readXml <- function(fl, ...)
  xmlTreeParse(fl, useInternal=TRUE, ...)
