.onLoad <- function(libname, pkgname) {
    ## patch XML?
    if (!"xmlValue.XMLAttributeValue" %in% methods(xmlValue))
    {
        assign("xmlValue.XMLAttributeValue",
               function(x, ignoreComments=FALSE) {
                   as.character(x)
               },
               topenv())
    }
}
