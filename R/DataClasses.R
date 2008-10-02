## Base class for all classes in this package
setClass("AffyCompatible")              # VIRTUAL

## DTT / ARR support

setClass("DTTCompatibility", "AffyCompatible")

setClass("ARRCompatibility", "AffyCompatible")

## NetAffxResource

setClass("NetAffxCompatibility", "AffyCompatible")

setClass("NetAffxResource",
         contains="NetAffxCompatibility",
         representation=representation(
           directory="character",
           annotationsFile="character",
           affxUrl="character",
           affxLicence="character",
           user="character",
           password="character"),
         prototype=prototype(
           directory=tempdir(),
           annotationsFile="NetAffxAnnotFileList.xml",
           affxUrl="https://www.affymetrix.com/analysis/downloads/netaffxapi/GetFileList.jsp",
           affxLicence="FHCRC0607"),
         validity=.validity)
