library(Reol)
library(xml2)

MyTaxa <- c("Camelus dromedarius")
MyEOLs <- DownloadSearchedTaxa(MyTaxa, to.file=FALSE)
#data(MyEOLs)
#PageProcessing(MyEOLs[1])

# Parse XML document
myxml <- read_xml(MyEOLs[[1]])
#xmllist <- as_list(myxml)

nsData <- xml_ns(myxml)

# Find data objects with appropriate mime type (image/jpeg)
dataObjs <- xml_find_all(myxml, ".//d1:dataObject", ns=nsData)
mimeTypes <- xml_text(xml_find_all(dataObjs, ".//d1:mimeType", ns=nsData))
imageObjs <- dataObjs[mimeTypes=="image/jpeg"]

# Get license info
licenseUrl <- sourceUrl <- rightsHolder <- mediaUrl <- list()
for (i in seq_along(imageObjs)) {
  obj <- imageObjs[[i]]

  lic <- try(xml_find_one(obj, ".//d1:license", ns=nsData), silent = TRUE)
  licenseUrl[[i]] <- ifelse (inherits(lic, "try-error"), NA, xml_text(lic))

  src <- try(xml_find_one(obj, ".//dc:source", ns=nsData), silent = TRUE)
  sourceUrl[[i]] <- ifelse (inherits(src, "try-error"), NA, xml_text(src))

  rh <- try(xml_find_one(obj, ".//dcterms:rightsHolder", ns=nsData), silent = TRUE)
  rightsHolder[[i]] <- ifelse (inherits(rh, "try-error"), NA, xml_text(rh))

  mediaUrl[[i]] <- xml_text(xml_find_all(obj, ".//d1:mediaURL", ns=nsData))
}

licenseInfo <- cbind(licenseUrl = unlist(licenseUrl),
                     url_parse(unlist(licenseUrl)),
                     sourceUrl = unlist(sourceUrl),
                     rightsHolder = unlist(rightsHolder))

# Attribution: Title, Source, Author, License  e.g. Photo by XXXX / CC BY
# dc:description ? not any good really
# dc:source
# dcterms:rightsholder
#
# tags$a(href=sourceUrl, "Photo")
# tags$a(href=licenseUrl, "BY-SA")

mediaURLs <- xml_find_all(imageObjs, ".//d1:mediaURL", ns=xml_ns(myxml))

url.show(xml_text(mediaURLs[1]))

library(dplyr)
urlText <- xml_find_all(imageObjs, ".//d1:mediaURL", ns=xml_ns(myxml)) %>% xml_text()
urlText
