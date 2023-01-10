#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require(DS401)) install.packages('DS401')
if (!require(shiny)) install.packages('shiny')
if (!require(x3ptools)) install.packages('x3ptools')
if (!require(tidyverse)) install.packages('tidyverse')
library(DS401)
library(shiny)
library(x3ptools)
library(tidyverse)

source("crop_x3p.R")
source("assess_col_na_cropped.R")
source("assess_bottomempty_cropped.R")
source("extract_na_cropped.R")
source("predict_one.R")
source("returnproblem.R")

options(shiny.maxRequestSize = 15 * 1024^2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose x3p File", accept = ".x3p"),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      textOutput("prediction"),
      plotOutput("scan")
    )
  )
)

server <- function(input, output) {


  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    output$prediction <- renderPrint({
      validate(need(ext == "x3p", "Please only upload X3P files"))
    })

    validate(need(ext == "x3p", ""))
    print(file$name)
    fau = str_extract(file$name, "(?<=LAPD...)[0-9]+")
    bullet = str_extract(file$name, "(?<=Bullet.)[A-G]")
    land = str_extract(file$name, "(?<=Land).[0-9]+")

    x3p <- read_x3p(file$datapath)
    pred = predict_one(x3p)
    outputText <- paste("FAU:", fau, "Bullet:", bullet, "Land:", land,
                        "The probability that this is a good scan is", pred[1], "%")
    problem = ""

    if (pred[1] <= 75) {
      problem = paste("The problem is predicted to be", pred[2])
      outputText <- paste(outputText, problem)
    }
    output$prediction <- renderPrint({
      outputText
    })
    output$scan <- renderPlot({
      outfile <- image(x3p)
    })
  })


}

shinyApp(ui, server)