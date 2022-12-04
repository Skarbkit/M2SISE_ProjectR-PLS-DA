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
source("split_train_test1.R")
source("pls_fit.R")
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
    selectInput(inputId = "xvar",
                label = "Select X variable",
                choices = xa,
                multiple=TRUE)
   
    
  })
  output$yvariable <- renderUI({
    req(data())
    
    ya<-colnames(data())
   
    selectInput(inputId = "yvar",
                label = "Select Y variable",
                choices = ya,
                multiple=FALSE)
    
  })
  
  
  
  
  
  
  
  
  
  
  ###########################################################
 

  InputDataset <- reactive({
    iris
  })
  
  
  InputDataset_model <- reactive({
    req(data(),input$xvar,input$yvar)
   
   
    dt <-data()
   
  })
  newData=eventReactive(input$Slider1,{
    req(data(),input$xvar,input$yvar)
    df=split_train_test(data(),(input$Slider1/100))
  })
  
  #creation train and test
  train=reactive({
    train=newData()$Train
    return(train)
  })
  
  test=reactive({
    test=newData()$Test
    return(test)
  })
  
  
#observe({
 #  lstname <- names(InputDataset())
  # updateSelectInput(session = session,
   #                   inputId = "SelectY",
    #                  choices = lstname)
  #})
  
 # splitSlider <- reactive({
  #  input$Slider1 / 100
  #})
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
  

  
  
  output$cntTrain <-
    renderText(paste("Train Data:", dim(train())[1], "records"))
  output$cntTest <-
    renderText(paste("Test Data:", dim(test())[1], "records"))
  
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
  
  
  #Code section for Pls Regression-----------------------------------------------------------------------------
  
  resFit=eventReactive(input$fit,{
    req(data(),input$xvar,input$yvar)
    formul=as.formula(paste(input$yvar,"~",".",sep=" "))
    if(is.null(input$xvar)){
      newtrain=train()
    }else{
      newtrain=train()[,c(input$xvar,input$yvar)]
    }
    res=pls.fit(formula=formul,data=newtrain,ncomp=as.numeric(input$ncomp))
    return(res)
  })
  
  output$showFit=renderTable({
    req(resFit())
    resFit()$coefs
         }) 
  

})


