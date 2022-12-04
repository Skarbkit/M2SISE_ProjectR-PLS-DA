

library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(datadigest)
library(rio)
library(DT)
library(stargazer)

############################################
# Define UI for random distribution app ----
############################################

dashboardPage(
  dashboardHeader(title = "PLS SISE", dropdownMenuOutput("msgOutput")),
  
  dashboardSidebar(
    
    
    
    sliderInput(
      "Slider1",
      label = h3("Train/Test Split %"),
      min = 0,
      max = 100,
      value = 75
    ),
    textOutput("cntTrain"),
    textOutput("cntTest"),
    
    
    
    
    br(),
    
    # Options to center and reduce the data
    h3("Scalling the X variables "),
    checkboxInput("center", "Center all X variables", TRUE),
    checkboxInput("reduce", "Reduce all X variables", FALSE),
    
    br(),
    
    
    # Download the results
    menuItem(
      "Generate Report",
      tabName = "sectors",
      icon = icon("download"),
      radioButtons(
        'format',
        'Document format',
        c('HTML', 'Word'),
        inline = FALSE,
        selected = 1
      ),
      downloadButton("report", "Download Report", class = "butt"),
      tags$head(tags$style(".butt{color: blue !important;}"))
    )
    
    
  ),
  dashboardBody(
    fluidPage(
      
      tabItem(tabName = "data",
              column(width = 4,
                     fluidRow(
                       inputPanel(
                         fileInput("file1", "Choose CSV File to fit the PLS Model",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         # Input: Checkbox if file has header ----
                         checkboxInput("header", "Header", TRUE),
                         
                         # Input: Select separator ----
                         radioButtons("sep", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t"),
                                      selected = ","),
                         # Input: Select number of rows to display ----
                         radioButtons("disp", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head"
                                      
                         )
                         
                         
                         # mainPanel(tableOutput("contents"))
                        
                       )
                     )
              )
      ),
      
      
      
      
      
      br(),
      
      box(
        
        uiOutput("xvariable"),
        solidHeader = TRUE,
        width = "3",
        status = "primary",
        title = "Choose the X variables"
      ),
      box(
       
        uiOutput("yvariable"),
        solidHeader = TRUE,
        width = "3",
        status = "primary",
        title = "Chose the Y variable"
      ),
      
      box(
        numericInput(
          "ncomp",
          label="ncomp",
          value="2",
          min = "2",
          max = "6"
          #step = 1,
          #width = NULL
        ),
       
        
        
        solidHeader = TRUE,
        width = "3",
        status = "primary",
        title = "Choose the number of components for the PLS"
      ),
      
      
      
    ),
    
    fluidPage(  
      
      tabBox(
        id = "tabset1",
        height = "1000px",
        width = 12,
        
        
        
        tabPanel("DataFile",
                 box(mainPanel(tableOutput("contents")), width = 12)),
        
        tabPanel("Data Iris",
                 box(withSpinner(DTOutput(
                   "Data"
                 )), width = 12)),
        tabPanel(
          "Data Summary",
          box(withSpinner(verbatimTextOutput("Summ")), width = 6),
          box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
        ),
        
       
        
        tabPanel("Plots",
                 box(withSpinner(plotOutput(
                   "Corr"
                 )), width = 12)),
        #box(withSpinner(verbatimTextOutput("CorrMatrix")), width = 12),
        tabPanel(
          "Fit",
          actionButton("fit","Fit the Data"),
         box(#mainPanel(tableOutput("showFit")),width = 6,
             textOutput("showFit"),
            title = "Coefficients"
          ),
          # box(
          #   withSpinner(verbatimTextOutput("Model_new")),
          #   width = 6,
          #   title = "Model Summary"
          # ),
          # 
         
        ),
        #textOutput("correlation_accuracy"),
        tabPanel(
          "Prediction",
          box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
          box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
        )
      )
    )
  )
)