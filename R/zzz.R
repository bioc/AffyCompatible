.onAttach <- function(libname, pkgname) {
    msg <- sprintf(
        "Package '%s' is deprecated and will be removed from Bioconductor
         version %s", pkgname, "3.17")
    .Deprecated(msg=paste(strwrap(msg, exdent=2), collapse="\n"))
}

xmlValue.XMLAttributeValue <-

    function(x, ignoreComments=FALSE, recursive=TRUE,
             encoding=XML:::CE_NATIVE, trim=FALSE)

{
    as.character(x)
}
