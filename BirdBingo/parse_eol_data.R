rm(list=ls())

library(Reol)
library(xml2)

# Quick fix for matching EOL entries for instances where search returns more
# than one result for the same EOL ID
insertSource("MatchTaxatoEOLID.R", package = "Reol", functions = "MatchTaxatoEOLID")

MyTaxa <- c("Turdus merula")
MatchTaxatoEOLID(MyTaxa, exact = T)
eolData <- DownloadSearchedTaxa(MyTaxa, to.file=FALSE, exact=TRUE)

# Load example EOL data
#data(MyEOLs)
#PageProcessing(MyEOLs[1])

# Parse XML document
myxml <- read_xml(eolData[[1]])
#xmllist <- as_list(myxml)

nsData <- xml_ns(myxml)

# Find data objects with appropriate mime type (image/jpeg)
dataObjs <- xml_find_all(myxml, ".//d1:dataObject", ns=nsData)
mimeTypes <- xml_text(xml_find_all(dataObjs, ".//d1:mimeType", ns=nsData))
imageObjs <- dataObjs[mimeTypes=="image/jpeg"]

# Get license info
licenseUrl <- sourceUrl <- rightsHolder <- mediaUrl <- list()
for (i in seq_along(imageObjs)) {
  ## Convert to R list object
  imgObj <- as_list(imageObjs[[i]])

  licenseUrl[[i]] <- unlist(imgObj$license)
  sourceUrl[[i]] <- unlist(imgObj$source)
  rightsHolder[[i]] <- ifelse(is.null(imgObj$rightsHolder), NA, unlist(imgObj$rightsHolder))
  mediaUrl[[i]] <- unlist(imgObj$mediaURL)
}

licenseInfo <- cbind(licenseUrl = licenseUrl,
                     #url_parse(licenseUrl),
                     sourceUrl = sourceUrl,
                     rightsHolder = rightsHolder)

# Attribution: Title, Source, Author, License  e.g. Photo by XXXX / CC BY
# Source: dc:source
# Author: dcterms:rightsholder
#
# tags$a(href=sourceUrl, "Photo")
# tags$a(href=licenseUrl, "BY-SA")

mediaURLs <- xml_find_all(imageObjs, ".//d1:mediaURL", ns=xml_ns(myxml))

url.show(xml_text(mediaURLs[1]))

library(dplyr)
urlText <- xml_find_all(imageObjs, ".//d1:mediaURL", ns=xml_ns(myxml)) %>% xml_text()
urlText
