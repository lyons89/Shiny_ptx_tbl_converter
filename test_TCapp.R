# Load Packages ---
library(shiny)
# library(dplyr)
# library(magrittr)
# library(tidyr)
# library(purrr)
# library(stringr)
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

APMS_MQ = function(df){
  
  
  untilt = df %>%
    dplyr::select(., any_of(c("Majority protein IDs", "Protein names", "Fasta headers", "Gene names", "Gene name", "Potential contaminant", "Peptides",  
                              "Razor + unique peptides", "Unique peptides")), 
                  contains("Difference"), contains("p-value"), contains("Significant"), starts_with("LFQ intensity"), starts_with("MS/MS count")) %>%
    mutate(across(.cols = starts_with("LFQ intensity"), ~round(.x, 2)),
           across(.cols = contains("Difference"), ~round(.x, 2)),
           across(.cols = contains("p-value"), ~round(.x, 8))) %>%
    mutate("Summed LFQ Intensity" = round(rowSums(2^across(.cols = starts_with("LFQ intensity")), na.rm=TRUE)), 0) %>%
    dplyr::select(., any_of(c("Majority protein IDs", "Protein names", "Fasta headers", "Gene names", "Gene name", "Razor + unique peptides", "Potential contaminant")),
                  contains("Difference"), contains("p-value"), contains("Significant"), starts_with("LFQ intensity"), "Peptides",
                  "Unique peptides", starts_with("Sequence coverage"), "Summed LFQ Intensity", starts_with("MS/MS count"),
                  -contains("significant", ignore.case=FALSE)) %>%
    arrange(.,desc(`Summed LFQ Intensity`))
  
  
}

import_pers = function(file_loc){
  
  firstRead = read.delim(file_loc, sep = "\t", na.strings = c("", "NaN", "NA"), check.names = FALSE)
  
  finalRead = read.delim(file_loc, sep = "\t", skip = sum(grepl("#!", firstRead[,1])), 
                         col.names = names(firstRead), na.strings = c("", "NaN", "NA"), check.names = FALSE)
  # skips the first several rows which contain a #!
  # also renames columns from the first read, and converts blank, NaN and NA values to N/A
  
}


options(shiny.maxRequestSize=30*1024^2) # increase server.R limit to 30MB file size

# user interface ----
ui <- navbarPage("Table Converter",
                 tabPanel("File Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select data type"),
                              radioButtons("SearchEngine", "Select Seach Engine Used:",
                                           choices = c("MQ-Perseus", "PD-Perseus", "Byos", "Spectronaut")),
                              conditionalPanel(
                                h3("Byos"),
                                condition = "input.SearchEngine == 'Byos'", # Do i need Byos to be in single quotes? yes
                                fileInput("byosFile", "Select the preprocessed results:",
                                          multiple = FALSE,
                                          placeholder = ".xlsx",
                                          accept = c(".xlsx")),
                                h4("Choose one option to supply expected mass values:"),
                                radioButtons("expectedInput", "Choose method for expected masses:",
                                             choices = (c("Manual", "xlsx"))),
                                fileInput("byosExpectedFile", "upload a xlsx file containing expected masses"),
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
                                fileInput("SpectroQuantFile", "Choose a Report xls quant file:",
                                          multiple = FALSE,
                                          accept = c(".xls")),
                                #selectInput("tab2", "Transformed Tab", choices = NULL),
                              ),
                              conditionalPanel(
                                h3("Perseus"),
                                condition = "input.SearchEngine == 'MQ-Perseus' || input.SearchEngine == 'PD-Perseus'", # can this have 2 options
                                fileInput("perseusUnimputedFile", "Select the UNIMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt")),
                                fileInput("perseusImputedFile", "Select the IMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt")),
                                h6("You can filter the comparison tabs 2 ways, either strickly by pvalue < 0.05 or pvalue<0.05 & FC>1"),
                                radioButtons("PerseusFilterOption", "Choose method to filter data by:",
                                             choices = (c("pval", "pval & FC"))),
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
  
  
  perseusUnImputed = reactive({
    
    req(input$perseusUnimputedFile)
    
    file = import_pers(file_loc = input$perseusUnimputedFile$datapath)
    return(file)
    
  })
  
  perseusImputed = reactive({
    
    req(input$perseusImputedFile)
    
    file = import_pers(file_loc = input$perseusImputedFile$datapath)
    return(file)
    
  })
  
  perseusSheetNames = reactive({
    df = perseusImputed()
    
    tabNames = str_replace_all(str_remove_all(names(dplyr::select(df, contains("p-value"))), "Student's T-test p-value "), 
                               pattern = "_", replacement = " v ")
    
    tabnames = c("Proteins", "Imputed", tabNames)
    
  })
  
  
  finalSheetnames = reactive({
    
    switch(input$SearchEngine,
           "MQ-Perseus" = perseusSheetNames(), 
           "PD-Perseus" =  perseusSheetNames(),
           "Byos" = ByosSheetNames(),
           "Spectronaut" = spectroSheetNames())
    
  })
  
  
  observeEvent(finalSheetnames(), {

    updateSelectInput(session, "tab2", choices = finalSheetnames())

  })
  
  
  conv_Byos = eventReactive(list(input$convert, input$SearchEngine == "Byos"),{
                      

        if(input$expectedInput == "Manual"){
          expected = byosExpectedInput()
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
      dplyr::select(., comparison = starts_with("Comparison"), ProteinGroups, UniquePeptides = `# Unique Total Peptides`,
                    log2FC = `AVG Log2 Ratio`, pvalue = Pvalue, qvalue = Qvalue) %>%
      dplyr::mutate(pvalue = round(pvalue, 8),
                    qvalue = round(qvalue, 8),
                    log2FC = round(log2FC, 4)) %>%
      tidyr::pivot_wider(., names_from = comparison, values_from = c(log2FC, pvalue, qvalue))

    
    quant2 = quant_df %>%
      dplyr::select(., any_of(c("PG.ProteinGroups", "PG.ProteinNames", "PG.Genes", "PG.ProteinDescriptions", "PG.FASTAHeader")), ends_with("PG.Quantity")) %>%
      dplyr::mutate("SummedQuantity" = round(rowSums(across(ends_with("PG.Quantity")))),0) %>%
      dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~round(.x, 2))) %>%
      dplyr::rename_with(., .cols = !ends_with("PG.Quantity"), ~gsub("^.*\\.", "", .x)) %>%      
      dplyr::left_join(., stats2, by = "ProteinGroups") %>%
      dplyr::select(., any_of(c("ProteinGroups", "ProteinNames", "Genes", 
                    "ProteinDescriptions", "FASTAHeader", 
                    "UniquePeptides", "SummedQuantity")), 
                    ends_with("PG.Quantity"), starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue")) %>%
      dplyr::rename_all(~str_replace_all(., "\\s+", ""))
    
    
    ind_comps = lapply(gsub("\\s+", "", unique(stats_df$`Comparison (group1/group2)`)), function(x){
      
      int = quant2 %>%
        dplyr::select(., any_of(c("ProteinGroups", "ProteinNames", "Genes", 
                      "ProteinDescriptions", "FASTAHeader", 
                      "unique_peptides")), contains(x)) %>%
        dplyr::select(., any_of(c("ProteinGroups", "ProteinNames", "Genes", 
                      "ProteinDescriptions", "FASTAHeader", 
                      "unique_peptides")), log2FC = starts_with("log2FC"), 
                      pvalue = starts_with("pvalue"), qvalue = starts_with("qvalue"))

    })
    
    lst = c(list(quant2), ind_comps)
    names(lst) = spectroSheetNames()
    
    return(lst) 
  })
  
  conv_Perseus_MQ = eventReactive(list(input$convert, input$SearchEngine == "MQ-Perseus|PD-Perseus"),{
    
    
    if(input$SearchEngine == "MQ-Perseus"){
      
      cleaned_unimputed = APMS_MQ(df = perseusUnImputed())
      cleaned_imputed = APMS_MQ(df = perseusImputed())
      
      # just separate the comparisons that are on the cleaned_imputed df
      log_names = names(dplyr::select(cleaned_imputed, contains("Difference")))
      pvalue_names = names(dplyr::select(cleaned_imputed, contains("p-value")))
      
      if(input$PerseusFilterOption == "pval"){
        
        filt = map2(log_names, pvalue_names, function(x,y){
          int = cleaned_imputed %>%
            dplyr::filter(., !!as.symbol(y) < 0.05) %>%
            dplyr::arrange(., desc((!!as.symbol(x))))
        })
        
        lst = c(list(cleaned_unimputed, cleaned_imputed), filt)
        names(lst) = perseusSheetNames()
        
        return(lst)
      }
      if(input$PerseusFilterOption == "pval & FC"){
        
        filt = map2(log_names, pvalue_names, function(x,y){
          int = cleaned_imputed %>%
            dplyr::filter(., !!as.symbol(x) > 1 | !!as.symbol(y) < -1) %>%
            dplyr::filter(., !!as.symbol(y) < 0.05) %>% # this method to evaluate "column name" as a variable. first turn into symbol, then !! inject it into expression
            dplyr::arrange(., desc(!!as.symbol(x)))
        })
        
        lst = c(list(cleaned_unimputed, cleaned_imputed), filt)
        names(lst) = perseusSheetNames()
        
        return(lst)
      }
      
      
      
    }
    
  })
  
  finalOut = reactive({
    
    switch(input$SearchEngine,
           "MQ-Perseus" = conv_Perseus_MQ(), 
           "PD-Perseus" =  PD,
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
