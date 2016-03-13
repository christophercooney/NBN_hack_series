#
# Simple Shiny example, draws checkbox grid and reacts to completed lines
#
library(shiny)

ui <- shinyUI(fluidPage(

  titlePanel("Test bingo"),
  mainPanel(

    lapply(1:4, function(i) {
      fluidRow(
        lapply(1:4, function(j) {
          column(3,
                 checkboxInput(paste0('cb', i, ".", j), label = paste0(i, ".", j)))
        })
      )
    }),

    textOutput("grid_check_info")
  )
)
)

checkGrid <- function(input) {
  grid <- array(dim = c(4,4))
  for (i in 1:4) {
    for (j in 1:4) {
      cbId <- paste0("cb", i, ".", j)
      grid[i,j] <- input[[cbId]]
    }
  }
  res <- any(apply(grid, MARGIN=1, all)) |
    any(apply(grid, MARGIN=2, all)) |
    all(diag(grid)) | all(diag(grid[,4:1]))

  return(res)
}

server <- shinyServer(function(input, output) {
  output$grid_check_info <- renderPrint({
    if (checkGrid(input)) {
      cat("BINGO!\n")
    }
  })
})

# Run the application
shinyApp(ui = ui, server = server)
