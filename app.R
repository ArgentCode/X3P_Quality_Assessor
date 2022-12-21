#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

predict_one <- function(Forest, X3P) {
  library(DS401)
  df <- data.frame(assess_bottomempty = double(),
                   assess_col_na = double(),
                   assess_median_na_proportion = double(),
                   assess_middle_na_proportion = double(),
                   extract_na = double(),
                   assess_rotation = double()
  )
  df[1,] = c(assess_bottomempty(X3P), assess_col_na(X3P), assess_median_na_proportion(X3P),
          assess_middle_na_proportion(X3P), extract_na(X3P), assess_rotation(X3P))
  return(predict(Forest, df, type = 'prob')[,2])
}

standardQualityForest = get(load("TestForest.RData"))
library(shiny)
library(x3ptools)
options(shiny.maxRequestSize = 15 * 1024^2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose x3p File", accept = ".x3p"),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {


  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)


    validate(need(ext == "x3p", "Please upload a x3p file"))

    x3p <- read_x3p(file$datapath)
    pred = as.numeric(predict_one(standardQualityForest, x3p)) * 100
    output$prediction <- renderPrint({
      paste("The probability that this is a good scan is", pred, "%")
    })
  })


}

shinyApp(ui, server)
