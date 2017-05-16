
rm(list=ls())

library(shiny); library(Reol); library(xml2); library(rnbn); library(stringr)

# Quick fix for Reol bug when matching EOL entries for instances where search
# returns more than one result for the same EOL ID
insertSource("./NBN_hack_series/BirdBingo/MatchTaxatoEOLID.R", package = "Reol", functions = "MatchTaxatoEOLID")

nbnLogin(username = "drcrc", password = "Turdusmerula")

load("./NBN_hack_series/BirdBingo/shiny_app/shef_data.rdata")

birdTVKs <- getGroupSpeciesTVKs("bird")
shef_data <- shef_data[shef_data$pTaxonVersionKey %in% birdTVKs,]

save(shef_data, file = "./NBN_hack_series/BirdBingo/ShefBirdData.Rdata")

load("./NBN_hack_series/BirdBingo/ShefBirdData.Rdata")

spp <- table(shef_data$pTaxonName)
spp <- spp[order(spp, decreasing = T)]

spp.names <- names(spp)[1:40] # Pick number of species to include

# FIXME: drop species if name contains < 2 words, as some entries are genus only

sppdat <- MatchTaxatoEOLID(spp.names, exact = T)
sppdat <- sppdat[!is.na(sppdat$eolPageNumbers),]

sppdat$N_records <- spp[match(sppdat$ListOfTaxa, names(spp))] # Attach number of records (i.e. commonness)

#spp.list <- as.numeric(sample(sppdat$eolPageNumbers, size = length(sppdat$eolPageNumbers), prob = spp, replace=F))
#chosen.names <- sppdat$ListOfTaxa[match(spp.list, sppdat$eolPageNumbers)]

myEOL <- DownloadEOLpages(sppdat$eolPageNumbers, to.file = FALSE)
#DataObjectOverview(myEOL)

#PageProcessing(data1[1])

url.list <- as.list(rep(NA, length(myEOL)))

for (i in 1:length(myEOL)) {
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
      #mediaUrl[[j]] <- unlist(imgObj$mediaURL)
      # Use this to get both URLs:
      mediaUrl[[j]] <- xml_text(xml_find_all(imageObjs[[j]], ".//d1:mediaURL", ns=nsData))
    }
    url.list[[i]] <- list(mediaUrl = unlist(mediaUrl),
                          licenseUrl = unlist(licenseUrl),
                          sourceUrl = unlist(sourceUrl),
                          rightsHolder = unlist(rightsHolder))
}

save(sppdat, spp, url.list, file = "./NBN_hack_series/BirdBingo/AppData.Rdata")

load(file = "./NBN_hack_series/BirdBingo/AppData.Rdata")

# # # UI # # #

img.height <- "150px"
img.width <- "150px"
grid.size <- 3
col.width <- 3

ui <- fluidPage(
    includeCSS("./NBN_hack_series/BirdBingo/shiny_app/styles.css"),

    # Application title
    titlePanel("Bird Bingo!"),
    hr(),
    fluidRow(
      column(3*col.width,
        uiOutput("grid_check_info")
      )
    ),
    lapply(1:grid.size, function(i) {
      fluidRow(
        lapply(1:grid.size, function(j) {
          column(col.width,
                 div(
                   uiOutput(paste0("image_title", i, ".", j),
                            class = "bb-image-title"),
                   div(
                     imageOutput(paste0("image", i, ".", j),
                                 height=img.height,
                                 click = paste0("image_click", i, ".", j)),
                     uiOutput(paste0("image_overlay", i, ".", j),
                              class = "bb-image-overlay"),
                     class="bb-image"
                   ),
                   uiOutput(paste0("image_info", i, ".", j),
                            class = "bb-photo-credit"),
                   class = "bb-square"
                 )
          )
        })
      )
    })
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
  addResourcePath("images", "./NBN_hack_series/BirdBingo/shiny_app/images")

  output$grid_check_info <- renderUI({
    if (checkGrid(input)) {
      div("BINGO!", class="bb-bingo")
    }
  })

  # Image output:
  n.urls <- grid.size^2
  spp.index <- sample(c(1:length(sppdat$eolPageNumbers)),
                      size = n.urls, ##prob = spp[sppdat$ListOfTaxa],
                      replace=F)

  lapply(1:grid.size, function(i) {
    lapply(1:grid.size, function(j) {
      index <- (i-1)*grid.size+j
      imageInfo <- url.list[[spp.index[index]]]

      titleId <- paste0('image_title', i, ".", j)
      output[[titleId]] <- renderUI ({
        h2(sppdat$ListOfTaxa[spp.index[index]])
      })

      imgId <- paste0('image', i, ".", j)
      output[[imgId]] <- renderImage({

        # A temp file to save the output
        outfile <- tempfile(fileext='.jpg')
        # FIXME catch problem downloading image
        download.file(imageInfo$mediaUrl[2], outfile, method = "libcurl")

        # FIXME specify width/height according to image orientation
        list(
          src = outfile,
          width = "100%",
          contentType = "image/jpeg",
          alt = sppdat$ListOfTaxa[index]
        )
      }, deleteFile = FALSE)

      infoId <- paste0('image_info', i, ".", j)
      output[[infoId]] <- renderUI ({

        # Attribution: Title, Source, Author, License  e.g. Photo by XXXX / CC BY
        # Source: dc:source
        # Author: dcterms:rightsholder

        # Title (extract file name from source URL)
        src <- imageInfo$sourceUrl[1]
        src_path <- unlist(strsplit(url_parse(src)$path, split="[/]"))
        src_path <- src_path[length(src_path)]
        src_path <- str_replace(src_path, "^File:", "")
        src_path <- str_replace_all(src_path, "_", "-")

        # Author (use rights holder)
        rh <- imageInfo$rightsHolder[1]

        # License e.g.
        # http://creativecommons.org/licenses/by-sa/2.5/
        lic_text <- lic_url <- imageInfo$licenseUrl[1]
        lic <- url_parse(lic_url)
        # Parse known license types
        if (match("creativecommons.org", lic$server)) {
          path <- unlist(strsplit(lic$path, split="[/]"))
          if ("licenses" %in% path) {
            lic_text <- paste("CC", str_to_upper(path[3]), sep = "-")
          } else {
            lic_text <- "Public domain"
          }
          lic_text <- str_c("(", lic_text, ")")
        }

        list(
          tags$a(href=src, src_path),
          tags$span(ifelse(!is.na(rh), paste("by", rh), "")),
          tags$a(href=lic_url, lic_text)
        )
      })

      # interaction click in image
      observeEvent(input[[paste0('image_click', i, ".", j)]], {
        output[[paste0('image_overlay', i, ".", j)]] <- renderUI({
          list(
            tags$img(
              src = "images/overlay.png",
              width = "100%",
              class = "bb-image-overlay"
            )
          )
        })
      })
    })
  })
}

shinyApp(ui, server)
