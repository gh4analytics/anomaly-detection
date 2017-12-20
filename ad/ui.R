# ----------------------------
# Description: Shiny dashboard for automated anomaly detection
# Author: Biju
# Date: 2017-12-19
# Version: 00.01
# Date: 
# Version: 
# Changed: 
# ----------------------------
#

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(magrittr)

path <- "F:/RProjects/automated-ad/"
appPath <- paste0(path, "ad/")
dataPath <- paste0(path, "data/")

stepsChoices = read_csv(paste0(dataPath, "DF_EvCodeDataProject.csv")) %$% sort(EventText)

dashboardPage(
  dashboardHeader(title = "Automated Anomaly Detection"),
  dashboardSidebar(
    dateInput(inputId = "DateStart", label = "Insert Start Date", value = "2017-01-01", format = "yyyy-mm-dd"),
    dateInput(inputId = "DateEnd", label = "Insert End Date", format = "yyyy-mm-dd"),
    helpText("Note: Set Desired dates of interest",
             "and select plots below to visualize",
             "specific steps of interest."),
    selectInput(inputId = "selInput", label = "Add Machine Steps to Analysis", choices = stepsChoices, 
                selected = stepsChoices[1], multiple = TRUE,
                selectize = TRUE, width = '100%', size = NULL),
    checkboxInput(inputId = "cboxSE", label = "Add Stat Error?", value = FALSE, width = NULL),
    checkboxInput(inputId = "points", label = "Add Points?"),
    div(style="display:inline-block;width:65%;text-align: right;",downloadButton(outputId = "downloadPlot",label = "Download Plot"))
  ),
  dashboardBody(
    mainPanel(
      headerPanel("Visualization of Process parameters"),
      tabsetPanel(
        tabPanel("Plot - Overview", plotOutput(outputId = "plot")),
        tabPanel("Plot - Box Plot", plotOutput(outputId = "plot2")),
        tabPanel("Plot - Anomaly", 
                 column(4, selectInput(inputId = "Step",label = "ChooseStep", choices = stepsChoices, 
                                       selected = stepsChoices[1], multiple = FALSE, selectize = TRUE, size = NULL)), 
                 column(4, numericInput(inputId = "numClasses", label = "Select Number of Classes", 
                                        value = 2, min = 1, max = 4, step = 1)),
                 column(4, checkboxInput(inputId = "scaled", label = "Scale Data?", value = FALSE)),
                 hr(),
                 plotOutput(outputId = "plot3"))
      )
      
    )
  )
)

