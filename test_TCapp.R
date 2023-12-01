# Load Packages ---
library(shiny)
library(tidyverse)
library(openxlsx)
library(readxl)
library(DT)

# user interface ----
ui <- navbarPage("Table Converter",
                 tabPanel("File Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select software options then upload data"),
                              br(),
                              radioButtons("SearchEngine", "Select Seach Engine Used:",
                                           choices = c("MQ", "PD", "Byos", "Spectronaut")),
                              br(),
                              radioButtons("PostProcessing", "Select PostProcessing Software used:",
                                           choices = (c("None", "Perseus")),
                                           selected = "None"),
                              conditionalPanel(
                                h3("Byos"),
                                condition = "input.SearchEngine == 'Byos'", # Do i need Byos to be in single quotes?
                                fileInput("byosFile", "Select the preprocessed results:",
                                          multiple = FALSE,
                                          placeholder = ".xlsx",
                                          accept = c(".xlsx")),
                                br(),
                                fileInput("byosExpectedFile", "Optional: Select a xlsx file containing expected masses:"),
                                br(),
                                selectInput("tab", "Select tab", choices = NULL),
                                br(),
                                textInput("expectedMasses", label = "Optional: Write in the expected masses, only separated by a comma:",
                                          value = ""),
                                br(),
                                textInput("outputFileName", label = "Export file name:", value = "")
                              )),
                            mainPanel(DT::dataTableOutput("dataframe"))
                          )))
                              
server = function(input, output, session){
  
  byosDf = reactive({
    
    req(input$byosFile)
    sheet_names = excel_sheets(input$byosFile$datapath) # extract all excel sheet names
    file1 = lapply(sheet_names, function(x){
      as.data.frame(read_excel(input$byosFile$datapath, sheet = x)) # read in each tab
    })
    names(file1) = sheet_names
    return(file1)
    
  })
  
  ByosSheetNames = reactive({
    req(input$byosFile)
    sheet_names = excel_sheets(input$byosFile$datapath) # will get "path" must be string error if I don't have the $datapath part
    return(sheet_names)
  })
  
  byosExpectedFile = reactive({
    
    req(input$byosExpectedFile)
    file = read.xlsx(input$byosExpectedFile$datapath, sheet=1)
    return(file)
    
  })
  
  byosExpectedInput = reactive({
    
    expected = input$expectedMasses
    
  })
  
  observeEvent(ByosSheetNames(), {

    updateSelectInput(session, "tab", choices = ByosSheetNames())

  })
  

  newDT = reactive({
    
    new = byosDf()[[input$tab]]
    
  })
  
  
  output$dataframe = DT::renderDataTable(newDT())
  

}

shinyApp(ui, server)
