#
# Simple Shiny example, draws checkbox grid and reacts to completed lines
#
library(shiny)

ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Test"),
  mainPanel(
    fluidRow(
      column(3,
             checkboxInput("cb1.1", label="1.1")),
      column(3,
             checkboxInput("cb1.2", label="1.2")),
      column(3,
             checkboxInput("cb1.3", label="1.3")),
      column(3,
             checkboxInput("cb1.4", label="1.4"))
    ),
    fluidRow(
      column(3,
             checkboxInput("cb2.1", label="2.1")),
      column(3,
             checkboxInput("cb2.2", label="2.2")),
      column(3,
             checkboxInput("cb2.3", label="2.3")),
      column(3,
             checkboxInput("cb2.4", label="2.4"))
    ),
    fluidRow(
      column(3,
             checkboxInput("cb3.1", label="3.1")),
      column(3,
             checkboxInput("cb3.2", label="3.2")),
      column(3,
             checkboxInput("cb3.3", label="3.3")),
      column(3,
             checkboxInput("cb3.4", label="3.4"))
    ),
    fluidRow(
      column(3,
             checkboxInput("cb4.1", label="4.1")),
      column(3,
             checkboxInput("cb4.2", label="4.2")),
      column(3,
             checkboxInput("cb4.3", label="4.3")),
      column(3,
             checkboxInput("cb4.4", label="4.4"))
    ),
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
