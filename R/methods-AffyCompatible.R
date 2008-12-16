## class building

.xmlElementDef <- function(elt, contains, prefix=character(0), verbose=FALSE) {

  .xmlElementContent <- function(contents) {
    if(is.null(contents$elements) && names(contents$type)=="PCData")
      ".PCData"
    else contents$elements
  }

  .xmlOrContent <- function(contents) {
    ## rapply classes="character" is a hack; only 'element' items are class character
    as.vector(rapply(contents$elements, force,
                     classes="character", how="unlist"))
  }

  .xmlSequenceContent <- function(contents) {
    ## rapply classes="character" is a hack; only 'element' items are class character
    as.vector(rapply(contents, force,
                     classes="character", how="unlist"))
  }

  .xmlContent <- function(contents, contentFunction) {
    cls <- contentFunction(contents)
    if (".PCData" %in% cls) {
      contains <- c("character", contains)
      cls <- cls[cls!=".PCData"]
    }
    elts <- .xnames(cls, prefix)
    slts[elts] <- ".TypedAssociation"
    do.call(setClass,
            list(elt$name,
                 representation=do.call("representation", slts),
                 contains=contains))
    
  }

  elt$name <- .xclassnames(elt$name, prefix)
  if (!isClass(elt$name)) {
    slts <- as.list(rep("character", length(elt$attributes)))
    names(slts) <- .xnames(lapply(elt$attributes, "[[", "name"),
                           prefix)

    contents <- elt$contents
    if (verbose) cat(elt$name, "\n")
    switch(class(contents),
           "NULL"={
             do.call(setClass,
                     list(elt$name,
                     representation=do.call("representation", slts),
                     contains=contains))
           },
           XMLElementContent={
             .xmlContent(contents, .xmlElementContent)
           },
           XMLSequenceContent={
             .xmlContent(contents, .xmlSequenceContent)
           },
           XMLOrContent={
             .xmlContent(contents, .xmlOrContent)
           },
           stop("Unhandled class:", class(contents), "\n"))
  }
}

.classBuilder <- function(dtd, contains, prefix=character(0),
                          verbose=FALSE) {
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(dirname(dtd))
  dtd <- XML::parseDTD(basename(dtd))
  for (elt in dtd$elements)
    .xmlElementDef(elt, contains, verbose=verbose, prefix)
  cls <- names(slot(getClass(contains), "subclasses"))
  for (cl in cls) .getters(cl)
}

.buildClasses <- function(classes) {
  if ("DTTCompatibility" %in% classes)
    .classBuilder(system.file("extdata", "MAGE-ML.dtd",
                              package="AffyCompatible"),
                  "DTTCompatibility")
  if ("ARRCompatibility" %in% classes)
    .classBuilder(system.file("extdata", "ArraySetAndTemplateFile.dtd",
                              package="AffyCompatible"),
                  "ARRCompatibility")
  if ("NetAffxCompatibility" %in% classes)
    .classBuilder(system.file("extdata", "NetAffxAnnotFileList.dtd",
                              package="AffyCompatible"),
                  "NetAffxCompatibility", prefix="Affx")
}

.build <- function(pkgname) {
  ## From 'methods' package
  where <- topenv(parent.frame())
  built <-
    if(exists(".builtImage", envir = where, inherits = FALSE))
      get(".builtImage", envir = where)
    else NA
  if(identical(built, FALSE)) {
    on.exit(assign(".builtImage", NA, envir = where))
    .buildClasses(c("DTTCompatibility", "ARRCompatibility",
                    "NetAffxCompatibility"))
    assign(".builtImage", TRUE, envir = where)
    on.exit()
  } else {
    if(!isTRUE(built))
      stop("package ", pkgname, "not installed correctly!")
  }
}

.builtImage <- FALSE

.build("AffyCompatible")

## constructors

.readConstructor <- function(fls, ...) {
    if (length(fls)>1)
        sapply(fls, .readXmlAsClass)
    else
        .readXmlAsClass(fls)[[1]]
}

readMage <- .readConstructor

readArr <- .readConstructor

## show

.AffyCompatible_show <- function(object) {
  for (nm in slotNames(class(object))) {
    val <- slot(object, nm)
    if (!(is(val, ".TypedAssociation") && length(val)==0)) {
        cat(nm, ": ", sep="")
        switch(class(val),
               "character"=cat(noquote(val), "\n"),
               "logical"=cat(noquote(val), "\n"),
               callGeneric(val))
    }
  }
}

setMethod("show",
          signature=signature(object="AffyCompatible"),
          .AffyCompatible_show)
