library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)
library(shinyWidgets)

dd <- iris

shinyServer(function(input, output, session) {
  
  
  
  data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
    
  })
 
  
  output$contents <- renderTable({data()
  })
  
  
  output$xvariable <- renderUI({
    req(data())
    xa<-colnames(data()) 
    pickerInput(inputId = 'xvar',
                label = 'Select X variable',
                
                choices = c(xa[1:length(xa)]), selected=xa[1],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    req(data())
    ya<-colnames(data()) 
    pickerInput(inputId = 'yvar',
                label = 'Select Y variable',
                choices = c(ya[1:length(ya)]), selected=ya[2],
                options = list(`style` = "btn-info"))
    
  })
  
  
  
  
  
  #lmModel <- reactive({
   # req(data(),input$xvar,input$yvar)
    #x <- as.numeric(data()[[as.name(input$xvar)]])
    #y <- as.numeric(data()[[as.name(input$yvar)]])
    #if (length(x) == length(y)){
    #  model <- lm(x ~ y, data = data(), na.action=na.exclude)
   # }else model <- NULL
   # return(model)
  #})
  
  
  
  
  
  
  
  
  
  
  ###########################################################
 

  InputDataset <- reactive({
    iris
  })
  
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- iris
    }
    else{
      dt <- iris[, c(input$SelectX)]
    }
    
  })
 
  
observe({
   lstname <- names(InputDataset())
   updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(InputDataset()))
  output$structure <- renderPrint(str(InputDataset()))
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  output$Data <- renderDT(InputDataset())
  
  
  cormat <- reactive({
    round(cor(InputDataset()), 1)
  })
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
 
  
})

