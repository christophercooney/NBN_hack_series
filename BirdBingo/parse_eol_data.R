library(Reol)
library(xml2)

MyTaxa <- c("Camelus dromedarius")
MyEOLs <- DownloadSearchedTaxa(MyTaxa, to.file=FALSE)
#data(MyEOLs)
#PageProcessing(MyEOLs[1])

# Parse XML document
myxml <- read_xml(MyEOLs[[1]])
#xmllist <- as_list(myxml)

# Find data objects with appropriate mime type (image/jpeg)
dataObjs <- xml_find_all(myxml, ".//d1:dataObject", ns=xml_ns(myxml))
mimeTypes <- xml_text(xml_find_all(dataObjs, ".//d1:mimeType", ns=xml_ns(myxml)))
imageObjs <- dataObjs[mimeTypes=="image/jpeg"]

# Get license info
licenses <- xml_text(xml_find_all(imageObjs, ".//d1:license", ns=xml_ns(myxml)))
url_parse(licenses)

mediaURLs <- xml_find_all(imageObjs, ".//d1:mediaURL", ns=xml_ns(myxml))

url.show(xml_text(mediaURLs[1]))

library(dplyr)
urlText <- xml_find_all(imageObjs, ".//d1:mediaURL", ns=xml_ns(myxml)) %>% xml_text()
urlText
