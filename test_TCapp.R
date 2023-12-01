# Load Packages ---
library(shiny)
library(tidyverse)
library(openxlsx)
library(readxl)
library(DT)

extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",")[[1]]
  as.numeric(split)
}


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
                                radioButtons("expectedInput", "Choose method for expected masses:",
                                             choices = (c("Manual", "xlsx"))),
                                br(),
                                fileInput("byosExpectedFile", "Optional: Select a xlsx file containing expected masses"),
                                br(),
                                textInput("expectedMasses", label = "Optional: Add expected masses, only separated by a comma",
                                          value = ""),
                                selectInput("tab1", "Original Tab", choices = NULL),
                                selectInput("tab2", "Transformed Tab", choices = NULL),
                                br(),
                                textInput("outputFileName", label = "Export file name:", value = "")
                              ),
                              actionButton("convert", label = "Convert", class = "btn btn-success"),
                              downloadButton("downloadByos", label = "Download", icon = icon("download"), class = "btn btn-primary")
                            ),
                            mainPanel(
                              DT::dataTableOutput("df1"),
                              DT::dataTableOutput("df2"))
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
    file = read.xlsx(input$byosExpectedFile$datapath, sheet=1)[,2]
    return(file)
    
  })
  
  byosExpectedInput = reactive({
    
    expected = extract(input$expectedMasses)
    
  })
  
  observeEvent(ByosSheetNames(), {

    updateSelectInput(session, "tab1", choices = ByosSheetNames())
    updateSelectInput(session, "tab2", choices = ByosSheetNames())
    

  })
  

  orig_DT = reactive({
    
    new = byosDf()[[input$tab1]]
    
  })
  
  
  output$df1 = DT::renderDataTable(orig_DT())
  
  run = eventReactive(input$convert,
                      if(input$SearchEngine == "Byos"){
                        
                        if(input$expectedInput == "Manual"){
                          expected == byosExpectedInput()
                        }else{
                          expected = byosExpectedFile()
                        }
                        
                        sheet_names = ByosSheetNames()
                        
                        byosdf = byosDf()
                        
                        nist = byosdf[[1]] %>%
                          dplyr::select(., Name, Mass, `Expected mass`, Intensity) %>%
                          mutate(Mass = as.numeric(Mass),
                                 Intensity = as.numeric(Intensity)) %>%
                          arrange(desc(Intensity)) %>%
                          mutate(`Expected mass` = as.numeric(`Expected mass`)) %>%
                          mutate("Delta mass from expected" = round(Mass - `Expected mass`, 4)) %>%
                          mutate("Delta mass from most intense" = round(dplyr::first(Mass) - Mass, 4))  %>%
                          mutate("Local Rel. Int. (%)" = round(Intensity / dplyr::first(Intensity) * 100, 2), "%") %>%
                          dplyr::rename(., "Measured mass" = Mass) %>%
                          mutate(Intensity = formatC(Intensity, format = "e", digits = 2)) %>%
                          dplyr::select(., Name, `Measured mass`, `Expected mass`,
                                        `Delta mass from expected`,
                                        `Delta mass from most intense`, Intensity, `Local Rel. Int. (%)`)
                        
                        
                        samps = map2(expected, byosdf[2:length(byosdf)], function(x,y){

                          red2 = y %>%
                            dplyr::select(., Name, Mass, `Expected mass`, Intensity) %>%
                            mutate(Mass = as.numeric(Mass),
                                   Intensity = as.numeric(Intensity)) %>%
                            arrange(desc(Intensity)) %>%
                            mutate(`Expected mass` = as.numeric(`Expected mass`)) %>%
                            mutate("Delta mass from expected (ilab)" = Mass - x) %>%
                            mutate("Delta mass from expected (byos)" = Mass - `Expected mass`) %>%
                            mutate("Expected mass (ilab)" = rep(x, nrow(.))) %>%
                            mutate("Delta mass from most intense" = dplyr::first(Mass) - Mass)  %>%
                            mutate("Local Rel. Int. (%)" = round(Intensity / dplyr::first(Intensity) * 100, 2), "%") %>%
                            dplyr::rename(., "Measured mass" = Mass) %>%
                            mutate(Intensity = formatC(Intensity, format = "e", digits = 2)) %>%
                            dplyr::select(., Name, `Measured mass`, `Expected mass (ilab)`, `Delta mass from expected (ilab)`,
                                          `Expected mass (byos)` = `Expected mass`, `Delta mass from expected (byos)`,
                                          `Delta mass from most intense`, Intensity, `Local Rel. Int. (%)`) 
                        })

                        lst = c(list(nist), samps)
                        names(lst) = ByosSheetNames()
                        
                        return(lst)
                        
                      })
  
  
  new_DT = reactive({
    new = run()[[input$tab2]]
  })
  
  output$df2 = DT::renderDataTable(new_DT())
  

   output$downloadByos = downloadHandler(
     filename = function() {
       paste0(format(Sys.time(),'%Y%m%d_%H%M'), "_", input$outputFileName, "_postprocessed.xlsx" )
     },
     content = function(file){
       hs = createStyle(textDecoration = "Bold", wrapText = TRUE)
       write.xlsx(run(), file,
                  sheetName = ByosSheetNames(), overwrite = TRUE, headerStyle = hs)
       
     }
   )

  
  
}

shinyApp(ui, server)
