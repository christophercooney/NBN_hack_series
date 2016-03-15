
rm(list=ls())

library(shiny); library(Reol); library(xml2); library(rnbn)

#taxo <- read.csv("/Users/chriscooney/Google Drive/CRCStorage/Datasets/TaxonomicData/Edited checklists/BLIOCPhyloMasterTax_2015_05_06.csv")

nbnLogin(username = "drcrc", password = "Turdusmerula")

load("/Users/chriscooney/Documents/Workflows/NBN_hack_series/BirdBingo/shiny_app/shef_data.rdata")
 
birdTVKs <- getGroupSpeciesTVKs("bird")
shef_data <- shef_data[shef_data$pTaxonVersionKey %in% birdTVKs,]

spp <- table(shef_data$pTaxonName)
spp <- spp[order(spp, decreasing = T)]

spp.names <- names(spp)[1:50]

sppdat <- MatchTaxatoEOLID(spp.names, exact = T)
sppdat <- sppdat[!is.na(sppdat$eolPageNumbers),]

spp <- spp[names(spp) %in% sppdat$ListOfTaxa]

spp.list <- as.numeric(sample(sppdat$eolPageNumbers, size = 25, prob = spp, replace=F))

chosen.names <- sppdat$ListOfTaxa[match(spp.list, sppdat$eolPageNumbers)]

myEOL <- DownloadEOLpages(spp.list, to.file = FALSE)
DataObjectOverview(myEOL)

#PageProcessing(data1[1])

url.list <- as.list(rep(NA, length(myEOL)))

for (i in 1:length(myEOL)) {
    furls <- c()
    myxml <- read_xml(myEOL[[i]])
    nsData <- xml_ns(myxml)

    # Find data objects with appropriate mime type (image/jpeg)
    dataObjs <- xml_find_all(myxml, ".//d1:dataObject", ns=nsData)
    mimeTypes <- xml_text(xml_find_all(dataObjs, ".//d1:mimeType", ns=nsData))
    imageObjs <- dataObjs[mimeTypes=="image/jpeg"]

    # Get media URL and license info
    licenseUrl <- sourceUrl <- rightsHolder <- mediaUrl <- list()
    for (j in seq_along(imageObjs)) {
      ## Convert to R list object for convenience
      imgObj <- as_list(imageObjs[[j]])

      licenseUrl[[j]] <- unlist(imgObj$license)
      sourceUrl[[j]] <- unlist(imgObj$source)
      rightsHolder[[j]] <- ifelse(is.null(imgObj$rightsHolder), NA, unlist(imgObj$rightsHolder))

      # There are two mediaURL entries (original image and EOL copy), this only gets the first:
      mediaUrl[[j]] <- unlist(imgObj$mediaURL)
      # Use this to get both URLs:
      #mediaUrl[[j]] <- xml_text(xml_find_all(imageObjs[[j]], ".//d1:mediaURL", ns=nsData))
    }
    url.list[[i]] <- list(mediaUrl = unlist(mediaUrl),
                          licenseUrl = unlist(licenseUrl),
                          sourceUrl = unlist(sourceUrl),
                          rightsHolder = unlist(rightsHolder))
}

# # # UI # # #

img.height <- "200px"
img.width <- "150px"
grid.size <- 4
col.width <- 3

ui <- fluidPage(
    # Application title
    titlePanel("Bird Bingo!"),
    hr(),
    lapply(1:grid.size, function(i) {
      fluidRow(
        lapply(1:grid.size, function(j) {
          column(col.width,
                 h5(chosen.names[(i-1)*grid.size+j]),
                 imageOutput(paste0("image", i, ".", j),
                             width=img.width, height=img.height,
                             click = paste0("image_click", i, ".", j)),
                 textOutput(paste0("image_info", i, ".", j)))
        })
      )
    }),
    textOutput("grid_check_info")
)

checkGrid <- function(input) {
  grid <- array(dim = rep(grid.size,2))
  for (i in 1:grid.size) {
    for (j in 1:grid.size) {
      grid[i,j] <- !is.null(input[[paste0('image_click', i, ".", j)]]$x)
    }
  }
  res <- any(apply(grid, MARGIN=1, all)) |
    any(apply(grid, MARGIN=2, all)) |
    all(diag(grid)) | all(diag(grid[,grid.size:1]))

  return(res)
}

server <- function(input, output, session) {

  output$grid_check_info <- renderPrint({
    if (checkGrid(input)) {
      cat("BINGO!\n")
    }
  })

  # Image output:
  lapply(1:grid.size, function(i) {
    lapply(1:grid.size, function(j) {
      imgId <- paste0('image', i, ".", j)
      output[[imgId]] <- renderImage({
        index <- (i-1)*grid.size+j
        imageInfo <- url.list[[index]]

        # A temp file to save the output
        outfile <- tempfile(fileext='.jpg')
        # TODO catch problem downloading image
        download.file(imageInfo$mediaUrl, outfile)

        list(
          src = outfile,
          width = img.width,
          contentType = "image/jpeg",
          alt = chosen.names[index]
        )
      }, deleteFile = FALSE)

      # interaction click in image
      observeEvent(input[[paste0('image_click', i, ".", j)]], {
        output[[paste0('image_info', i, ".", j)]] <- renderPrint({
          cat("Seen it!\n")
        })
      })
    })
  })
}

shinyApp(ui, server)
