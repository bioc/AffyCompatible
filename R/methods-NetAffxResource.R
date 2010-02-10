.NetAffxResource_validity <- function(object) {
  msg <- NULL
  slts <- slotNames(getClass("NetAffxResource"))
  len_ok <- sapply(slts, function(slt) {
    elt <- slot(object, slt)
    if (length(elt) == 1) nzchar(elt)
    else FALSE
  })
  if (!all(len_ok))
    msg <- c(msg,
             sprintf("slot(s) '%s' must be length 1, non-empty character vectors",
                     paste(slts[!len_ok], collapse="' '")))
  if (is.null(msg)) TRUE
  else msg
}

setMethod(".validity", "NetAffxResource", .NetAffxResource_validity)

.getters("NetAffxResource")

NetAffxResource <- function(user=character(0), password=character(0),
                            affxLicence="FHCRC0607",
                            directory=tempdir(), ...) {
  new("NetAffxResource", ..., user=user, password=password,
      affxLicence=affxLicence, directory=directory)
}

setMethod("names",
          signature=signature(x="NetAffxResource"),
          function(x) {
            .xvalue(.readNetAffxXml(x), "//Array/@name")
          })

.Array_names <- function(x) sapply(affxAnnotation(x), affxDescription)

setMethod("names", signature(x="AffxArray"), .Array_names)

setMethod("affxDescription", signature(x="AffxArray"), .Array_names)

.idxOk <- function(idx, str) {
  len <- length(str)
  if (idx<1 || idx>len)
    stop("'", deparse(substitute(idx)), "' must be >0 and <", len)
  TRUE
}

setMethod("[[",
          signature=signature(
            x="NetAffxResource",
            i="numeric",
            j="missing"),
          function(x, i, j, ...) {
            if (length(i)!=1)
              stop("argument 'i' must be a length 1 vector")
            nms <- names(x)
            .idxOk(i, nms)
            callGeneric(x, i=nms[i], ..., .check=FALSE)
          })

setMethod("[[",
          signature=signature(
            x="NetAffxResource",
            i="character",
            j="missing"),
          function(x, i, j, ..., .check=TRUE) {
            if (.check) {
              if (length(i)!=1)
                stop("argument 'i' must be a length 1 vector")
              if (!i %in% names(x))
                stop("'", i, "' must be in names(x)")
            }
            xclass(.readNetAffxXml(x),
                   paste('//Array[@name="', i, '"]', sep=""),
                   prefix="Affx")[[1]]
          })

.NetAffxResource_descriptions <- function(netAffxResource, name) {
  xpathq <- paste('//Array[@name="', name, '"]//@description', sep="")
  .xvalue(.readNetAffxXml(netAffxResource), xpathq)
}

setMethod("[[",
          signature=signature(
            x="NetAffxResource",
            i="numeric",
            j="numeric"),
          function(x, i, j, ...) {
            if (length(i) != 1 || length(j) != 1)
              stop("arguments 'i', 'j' must be length 1 vectors")
            nms <- names(x)
            .idxOk(i, nms)
            desc <- .NetAffxResource_descriptions(x, nms[i])
            .idxOk(j, desc)
            callGeneric(x, i=nms[i], j=desc[j], ..., .check=FALSE)
          })

setMethod("[[",
          signature=signature(
            x="NetAffxResource",
            i="numeric",
            j="character"),
          function(x, i, j, ...) {
            if (length(i) != 1 || length(j) != 1)
              stop("arguments 'i', 'j' must be length 1 vectors")
            nms <- names(x)
            .idxOk(i, nms)
            callGeneric(x, i=nms[i], j=j, ..., .check=FALSE)
          })

setMethod("[[",
          signature=signature(
            x="NetAffxResource",
            i="character",
            j="numeric"),
          function(x, i, j, ...) {
            if (length(i) != 1 || length(j) != 1)
              stop("arguments 'i', 'j' must be length 1 vectors")
            desc <- .NetAffxResource_descriptions(x, i)
            .idxOk(j, desc)
            callGeneric(x, i, j=desc[j], ..., .check=FALSE)
          })

setMethod("[[",
          signature=signature(
            x="NetAffxResource",
            i="character",
            j="character"),
          function(x, i, j, ..., .check=TRUE) {
            if (.check) {
              if (length(i) != 1 || length(j) != 1)
                stop("arguments 'i', 'j' must be length 1 vectors")
              if (!i %in% names(x))
                stop("'", i, "' must be in names(x)")
              desc <- .NetAffxResource_descriptions(x, i)
              idx <- match(j, desc)
              if (is.na(idx))
                stop("arugment j ('", j, "') must be one of '",
                     paste(desc, collapse="', '"), "'")
            }
            xpathq <-
              paste("/NetAffxAnnotFileList/Array[@name='", i, "']",
                    "/Annotation[@description='", j, "']",
                    sep="")
            xclass(.readNetAffxXml(x), xpathq, prefix="Affx")[[1]]
          })

.NetAffxResource_show <- function(object) {
  for (nm in slotNames(class(object))) {
    val <- 
      if (nm %in% c("password", "affxLicence")) rep("*", 8)
      else slot(object, nm)
    cat(nm, ": ", val, "\n", sep="")
    invisible()
  }
}

setMethod("show",
          signature=signature(object="NetAffxResource"),
          .NetAffxResource_show)

.netAffxUrl <- function(x) {
  paste(affxUrl(x), "?", "licence=", affxLicence(x), "&",
        "user=", user(x), "&", "password=", password(x), sep="")
}

## Annotation file list

.netAffxAnnotListFile <- function(x) {
  file.path(directory(x), annotationsFile(x))
}

.retrieveNetAffx <- function(netAffxResource, update, ...) {
  annotFile <- .netAffxAnnotListFile(netAffxResource)
  if (update || !file.exists(annotFile)) {
    url <- .netAffxUrl(netAffxResource)
    .opts <- curlOptions(ssl.verifypeer=FALSE, followlocation=TRUE)
    rsrc <- getURL(url, .opts=.opts)
    tmpfile <- tempfile()
    cat(rsrc, file=tmpfile) # save to file; XML 1.93 fails to parse long first args
    status <- .xvalue(readXml(tmpfile), "/NetAffxAnnotFileList/Status")
    if (nzchar(status)) {
      unlink(tmpfile)
      stop("NetAffxResource: ", status)
    }
    file.rename(tmpfile, annotFile)
    unlink(tmpfile)
  }
  annotFile
}

readNetAffx <- function(netAffxResource, update=FALSE, ...) {
  annotFile <- .retrieveNetAffx(netAffxResource, update, ...)
  .readXmlAsClass(annotFile, prefix="Affx")[[1]]
}

.readNetAffxXml <- function(netAffxResource, update=FALSE, ...) {
  annotFile <- .retrieveNetAffx(netAffxResource, update, ...)
  readXml(annotFile)
}

## Annotations per se

setGeneric("readAnnotation",
           function(netAffxResource, array, annotation, ...) {
             standardGeneric("readAnnotation")
           })

.readAnnotation2 <- function(netAffxResource, array, annotation, ...) {
  .readAnnotation(netAffxResource, netAffxResource[[array, annotation]],
                  ...)
}

setMethod("readAnnotation",
          signature=signature(
            netAffxResource="NetAffxResource",
            array="character",
            annotation="character"),
          .readAnnotation2)

setMethod("readAnnotation",
          signature=signature(
            netAffxResource="NetAffxResource",
            array="character",
            annotation="numeric"),
          .readAnnotation2)

setMethod("readAnnotation",
          signature=signature(
            netAffxResource="NetAffxResource",
            array="numeric",
            annotation="character"),
          .readAnnotation2)

setMethod("readAnnotation",
          signature=signature(
            netAffxResource="NetAffxResource",
            array="numeric",
            annotation="numeric"),
          .readAnnotation2)

setMethod("readAnnotation",
          signature=signature(
            netAffxResource="NetAffxResource",
            array="missing",
            annotation="AffxAnnotation"),
          function(netAffxResource, array, annotation, ...) {
            .readAnnotation(netAffxResource, annotation, ...)
          })

.netAffxAnnotFile <- function(netAffxResource, url) {
  file.path(directory(netAffxResource), basename(url))
}

.readAnnotation <- function(netAffxResource, annotation, fileIndex=1,
                            ..., content=TRUE, update=FALSE) {
  affxUrl <- affxUrl(affxFile(annotation)[[fileIndex]])[[1]]
  url <- .Data(affxUrl)
  annotFile <- .netAffxAnnotFile(netAffxResource, url)
  if (update || !file.exists(annotFile)) {
    download.file(url, annotFile, mode="wb")
  }
  if (!content)
    return(annotFile)
  conn <- 
    switch(affxCompression(affxUrl),
           "application/zip"={
             fileName <- sub(".zip$", "", basename(annotFile))
             if (length(grep("_psi$", fileName))>0)
               fileName <- sub("_psi$", ".psi", fileName)
             unz(annotFile, fileName)
           },
           stop("unknown affxCompression:", affxCompression(affxUrl)))
  on.exit(unlink(fileName))
  res <-
    switch(sub(".* ", "", affxType(annotation)),
           CSV=read.csv(conn, ...),
           Tabular=read.delim(conn, ...),
           FASTA=readFASTA(conn, ...),
           PSI=read.delim(conn, header=FALSE, skip=1, sep="\t", ...),
           {
             close(conn)
             message("returning path to file of affxType '",
                     affxType(annotation), "'")
             annotFile
           })
  res
}
