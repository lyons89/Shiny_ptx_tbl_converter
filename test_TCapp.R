# Load Packages ---
library(shiny)
library(tidyverse)
library(openxlsx)
library(readxl)
library(DT)
library(data.table)

extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",")[[1]]
  as.numeric(split)
}

options(shiny.maxRequestSize=30*1024^2) # increase server.R limit to 30MB file size

# user interface ----
ui <- navbarPage("Table Converter",
                 tabPanel("File Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select data type"),
                              br(),
                              radioButtons("SearchEngine", "Select Seach Engine Used:",
                                           choices = c("MQ-Perseus", "PD-Perseus", "Byos", "Spectronaut")),
                              conditionalPanel(
                                h3("Byos"),
                                condition = "input.SearchEngine == 'Byos'", # Do i need Byos to be in single quotes? yes
                                fileInput("byosFile", "Select the preprocessed results:",
                                          multiple = FALSE,
                                          placeholder = ".xlsx",
                                          accept = c(".xlsx")),
                                br(),
                                h4("Choose one option to supply expected mass values:"),
                                radioButtons("expectedInput", "Choose method for expected masses:",
                                             choices = (c("Manual", "xlsx"))),
                                br(),
                                fileInput("byosExpectedFile", "upload a xlsx file containing expected masses"),
                                br(),
                                textInput("expectedMasses", label = "Optional: Add expected masses, only separated by a comma",
                                          value = ""),
                                #selectInput("tab2", "Transformed Tab", choices = NULL),
                              ),
                              conditionalPanel(
                                h3("Spectronaut"),
                                condition = "input.SearchEngine == 'Spectronaut'",
                                fileInput("SpectroStatsFile", "Choose a Candiate xls stats file:",
                                          multiple = FALSE,
                                          accept = c(".xls")), 
                                br(),
                                fileInput("SpectroQuantFile", "Choose a Report xls quant file:",
                                          multiple = FALSE,
                                          accept = c(".xls")),
                                #selectInput("tab2", "Transformed Tab", choices = NULL),
                              ),
                              selectInput("tab2", "Transformed Tab", choices = NULL),
                              textInput("outputFileName", label = "Export file name:", value = ""),
                              actionButton("convert", label = "Convert", class = "btn btn-success"),
                              downloadButton("download", label = "Download", icon = icon("download"), class = "btn btn-primary")
                            ),
                            mainPanel(
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
  
  spectroStats = reactive({
    
    req(input$SpectroStatsFile)
    file = fread(input$SpectroStatsFile$datapath)
    return(file)
    
  })
  
  spectroQuant = reactive({
    
    req(input$SpectroQuantFile)
    file = fread(input$SpectroQuantFile$datapath)
    return(file)
    
  })
  
  spectroSheetNames = reactive({
    
    df = spectroStats()
    
    sheetnames = unique(df$`Comparison (group1/group2)`) %>%
      gsub(" ", "", .) %>%
      gsub("/", "-", .)
    
    names = c("Proteins", sheetnames)
    
  })
  
  finalSheetnames = reactive({
    
    switch(input$SearchEngine,
           "MQ" = MQ, 
           "PD" =  PD,
           "Byos" = ByosSheetNames(),
           "Spectronaut" = spectroSheetNames())
    
  })
  
  
  observeEvent(finalSheetnames(), {

    updateSelectInput(session, "tab2", choices = finalSheetnames())

  })
  
  
  conv_Byos = eventReactive(list(input$convert, input$SearchEngine == "Byos"),{
                      

        if(input$expectedInput == "Manual"){
          expected == byosExpectedInput()
        }
        else{
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
  
  conv_Spec = eventReactive(list(input$convert, input$SearchEngine == "Spectronaut"),{
    
    stats_df = spectroStats()
    quant_df = spectroQuant()
    
    stats2 = stats_df %>%
      dplyr::select(., comparison = starts_with("Comparison"), accession = ProteinGroups, unique_peptides = `# Unique Total Peptides`,
                    log2FC = `AVG Log2 Ratio`, pvalue = Pvalue, qvalue = Qvalue) %>%
      pivot_wider(., names_from = comparison, values_from = c(log2FC, pvalue, qvalue))
    
    
    quant2 = quant_df %>%
      dplyr::select(., any_of(c("PG.ProteinGroups", "PG.ProteinNames", "PG.Genes", "PG.ProteinDescriptions", "PG.FASTAHeader")), ends_with("PG.Quantity")) %>%
      mutate("SummedQuantity" = rowSums(across(ends_with("PG.Quantity")))) %>%
      left_join(., stats2, by = c("PG.ProteinGroups" = "accession")) %>%
      dplyr::select(., ProteinGroups = PG.ProteinGroups, ProteinNames = PG.ProteinNames, Genes = PG.Genes, 
                    ProteinDescriptions = PG.ProteinDescriptions, FASTAHeader = PG.FASTAHeader, 
                    unique_peptides, SummedQuantity, 
                    ends_with("PG.Quantity"), starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue")) %>%
      rename_all(~str_replace_all(., "\\s+", ""))
    
    
    ind_comps = map(gsub("\\s+", "", unique(stats_df$`Comparison (group1/group2)`)), function(x){
      
      int = quant2 %>%
        dplyr::select(., ProteinGroups, ProteinNames, Genes, 
                      ProteinDescriptions, FASTAHeader, 
                      unique_peptides, contains(x)) %>%
        dplyr::select(., ProteinGroups, ProteinNames, Genes, 
                      ProteinDescriptions, FASTAHeader, 
                      unique_peptides, log2FC = starts_with("log2FC"), 
                      pvalue = starts_with("pvalue"), qvalue = starts_with("qvalue"))
      
    })
    
    lst = c(list(quant2), ind_comps)
    names(lst) = spectroSheetNames()
    
    return(lst) 
  })
  
  
  finalOut = reactive({
    
    switch(input$SearchEngine,
           "MQ" = MQ, 
           "PD" =  PD,
           "Byos" = conv_Byos(),
           "Spectronaut" = conv_Spec())
    
  })
  

  DT = reactive({
    
    newDT = finalOut()[[input$tab2]]
    
  })
  
  output$df2 = DT::renderDataTable(DT())
  
  
  
  
  output$download = downloadHandler(
    filename = function() {
      paste0(format(Sys.time(),'%Y%m%d_%H%M'), "_", input$outputFileName, "_postprocessed.xlsx" )
    },
    content = function(file){
      hs = createStyle(textDecoration = "Bold", wrapText = TRUE)
      write.xlsx(finalOut(), file,
                 sheetName = finalSheetnames(), overwrite = TRUE, headerStyle = hs)
    }
    
  )
  

}

shinyApp(ui, server)
