#
# Simple Shiny example, draws checkbox grid and reacts to completed lines
#
library(shiny)

load("./NBN_hack_series/BirdBingo/testAppData.Rdata")

grid.size <- 3
img.width <- "150px"
img.height <- "150px"
col.width <- 4

ui <- shinyUI(fluidPage(
  includeCSS("./NBN_hack_series/BirdBingo/styles.css"),

  titlePanel("Test bingo"),
  mainPanel(
    lapply(1:grid.size, function(i) {
      fluidRow(
        lapply(1:grid.size, function(j) {
          column(col.width,
                 imageOutput(paste0("image", i, ".", j),
                             width=img.width, height=img.height,
                             click = paste0("image_click", i, ".", j)),
                 textOutput(paste0("image_info", i, ".", j)))
        })
      )
    }),

    textOutput("grid_check_info")
  )
)
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

server <- shinyServer(function(input, output) {

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
})

# Run the application
shinyApp(ui = ui, server = server)
