MatchTaxatoEOLID <- function (ListOfTaxa, exact = TRUE, ...)
{
    eolPageNumbers <- rep(NA, length(ListOfTaxa))
    speciesNameForRef <- rep(NA, length(ListOfTaxa))
    for (i in sequence(length(ListOfTaxa))) {
        taxon <- APItaxon(ListOfTaxa[i])
        web <- paste("http://eol.org/api/search/1.0.xml?q=",
            taxon, "&exact=", exact, "&page=1", sep = "")
        a <- getURL(web, ...)
        searchRes <- NULL
        searchRes <- xmlToList(xmlRoot(xmlParse(a, getDTD = FALSE),
            ...), simplify = FALSE)
        if (searchRes$totalResults == 1) {
            eolPageNumbers[i] <- searchRes$entry$id
            speciesNameForRef[i] <- searchRes$entry$title
        }
        else {
            ids <- sapply(searchRes[names(searchRes) == "entry"],
                function(x) x$id)
            if (length(rle(ids)$values) == 1) {
                eolPageNumbers[i] <- searchRes$entry$id
                speciesNameForRef[i] <- searchRes$entry$title
            }
        }
    }
    return(data.frame(ListOfTaxa, speciesNameForRef, eolPageNumbers,
        stringsAsFactors = F))
}
