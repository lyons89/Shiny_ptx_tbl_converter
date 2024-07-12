# Load Packages ---
# library(shiny)
# library(tidyverse)
# library(openxlsx)
# library(DT)

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}

if(!require(DT)){
  install.packages("DT")
  library(DT)
}
if(!require(vroom)){
  install.packages("vroom")
  library(vroom)
}


# Functions
extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",")[[1]]
  as.numeric(split)
}

APMS_MQ = function(df){
  
  df %>%
    dplyr::select(., any_of(c("Majority protein IDs", "Protein names", "Fasta headers", "Gene names", "Gene name", "Potential contaminant", "Peptides",  
                              "Razor + unique peptides", "Unique peptides", "Sequence coverage [%]")), 
                  contains("Difference"), contains("p-value"), contains("Significant"), starts_with("LFQ intensity"), starts_with("MS/MS count")) %>%
    dplyr::mutate(across(.cols = starts_with("LFQ intensity"), ~round(.x, 2)),
           across(.cols = contains("Difference"), ~round(.x, 2)),
           across(.cols = contains("p-value"), ~round(.x, 8))) %>%
    dplyr::mutate("Summed LFQ Intensity" = round(rowSums(2^across(.cols = starts_with("LFQ intensity")), na.rm=TRUE)), 0) %>%
    dplyr::select(., any_of(c("Majority protein IDs", "Protein names", "Gene names", "Gene name", "Fasta headers", "Razor + unique peptides", "Potential contaminant")),
                  contains("Difference"), contains("p-value"), contains("Significant"), starts_with("LFQ intensity"), "Peptides",
                  "Unique peptides", "Sequence coverage [%]", starts_with("Sequence coverage"), "Summed LFQ Intensity", starts_with("MS/MS count"),
                  -contains("significant", ignore.case=FALSE)) %>%
    dplyr::arrange(.,desc(`Summed LFQ Intensity`))
  
  
}

import_pers = function(file_loc){
  
  firstRead = read.delim(file_loc, sep = "\t", na.strings = c("NaN", "NA"), check.names = FALSE)
  
  finalRead = read.delim(file_loc, sep = "\t", skip = sum(grepl("#!", firstRead[,1])), 
                         col.names = names(firstRead), na.strings = c("NaN", "NA"), check.names = FALSE)
  # skips the first several rows which contain a #!
  # also renames columns from the first read, and converts blank, NaN and NA values to N/A
  
}


options(shiny.maxRequestSize=200*1024^2) # increase server.R limit to 200MB file size


ui <- navbarPage("Table Converter",
                 tabPanel("File Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select data type"),
                              radioButtons("SearchEngine", "Select Seach Engine Used:",
                                           choices = c("MQ-Perseus", "Byos", "Spectronaut")),
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
                                          value = "")
                                #selectInput("tab2", "Transformed Tab", choices = NULL),
                              ),
                              conditionalPanel(
                                h3("Spectronaut"),
                                condition = "input.SearchEngine == 'Spectronaut'",
                                fileInput("SpectroStatsFile", "Select a Candidate Stats file:",
                                          multiple = FALSE,
                                          accept = c(".xls", ".tsv")), 
                                fileInput("SpectroQuantFile", "Select a Report quant file:",
                                          multiple = FALSE,
                                          accept = c(".xls", ".tsv")),
                                h4("Do you want to import the SpN Condition Setup File?"),
                                radioButtons("AddConditions", "Select yes/no", choices = c("Yes", "No"), selected = "No"),
                                conditionalPanel(
                                  condition = "input.AddConditions == 'Yes'",
                                  fileInput("SpectroConditions", "Select a Condition file:",
                                            multiple = FALSE,
                                            accept = c(".tsv"))
                                ),
                                radioButtons("transformSpectro", "How do you want the sample quantity values",
                                             choices = c("non-transformed", "Log2", "Both"), selected = "Log2"),
                                radioButtons("statsFilter", "stats value to filter on:", c("p-value" = "pvalue","q-value" = "qvalue"), selected = "qvalue"),
                                numericInput("statsValue", "stats value to filter by:", value = "0.05")
                              ),
                              conditionalPanel(
                                h3("Perseus"),
                                condition = "input.SearchEngine == 'MQ-Perseus'", # can this have 2 options
                                fileInput("perseusUnimputedFile", "Select the UNIMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt")),
                                fileInput("perseusImputedFile", "Select the IMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt")),
                                h6("You can filter the comparison tabs 2 ways, either strickly by pvalue < 0.05 or pvalue<0.05 & FC>1"),
                                radioButtons("PerseusFilterOption", "Choose method to filter data by:",
                                             choices = (c("pval", "pval & log2FC")))
                              ),
                              selectInput("tab2", "Select tab to view", choices = NULL),
                              textInput("outputFileName", label = "Export file name:", value = ""),
                              actionButton("convert", label = "Convert", class = "btn btn-success"),
                              downloadButton("download", label = "Download", icon = icon("download"), class = "btn btn-primary")
                            ),
                            mainPanel(
                              DT::dataTableOutput("df2"))
                            )),
                 tabPanel("Manual",
                          includeMarkdown("manual.md")))
                              
server = function(input, output, session){
  
  
  byosDf = reactive({
    
    req(input$byosFile)
    sheet_names = getSheetNames(input$byosFile$datapath) # extract all excel sheet names
    file1 = lapply(sheet_names, function(x){
      as.data.frame(read.xlsx(input$byosFile$datapath, sheet = x, sep.names = " ")) # read in each tab
    })
    names(file1) = sheet_names
    return(file1)
    
  })
  
  ByosSheetNames = reactive({
    req(input$byosFile)
    sheet_names = getSheetNames(input$byosFile$datapath) # will get "path" must be string error if I don't have the $datapath part
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
    #file = read.delim(input$SpectroStatsFile$datapath, sep = "\t", check.names = FALSE)
    file = vroom(input$SpectroStatsFile$datapath, delim = "\t", na = c("NA", "NaN"), show_col_types = FALSE, progress = FALSE, num_threads = 2)
    return(file)
    
  })
  
  spectroQuant = reactive({
    
    req(input$SpectroQuantFile)
    #file = read.delim(input$SpectroQuantFile$datapath, sep = "\t", check.names = FALSE)
    file = vroom(input$SpectroQuantFile$datapath, delim = "\t", na = c("NA", "NaN"), show_col_types = FALSE, progress = FALSE, num_threads = 2)
    return(file)
    
  })
  
  spectroCond = reactive({
    
    req(input$SpectroConditions)
    #file = read.delim(input$SpectroQuantFile$datapath, sep = "\t", check.names = FALSE)
    file = vroom(input$SpectroQuantFile$datapath, delim = "\t", na = c("NA", "NaN"), show_col_types = FALSE, progress = FALSE, num_threads = 1)
    return(file)
    
  })
  
  
  spectroSheetNames = reactive({
    
    df = spectroStats()
    
    rm_pool = df %>%
      filter(!grepl("pool", tolower(`Comparison (group1/group2)`)))
      
    sheetnames = unique(rm_pool$`Comparison (group1/group2)`) %>%
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
            mutate("Delta mass from expected (ilab)" = round((Mass - x), 4)) %>%
            mutate("Delta mass from expected (byos)" = round((Mass - `Expected mass`), 4)) %>%
            mutate("Expected mass (ilab)" = rep(x, nrow(.))) %>%
            mutate("Delta mass from most intense" = round((dplyr::first(Mass) - Mass), 4))  %>%
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
    
    # report_column_names_keep = c("ProteinGroups", "ProteinNames", "Genes", "ProteinDescriptions","FastaFiles", "FastaHeaders",
    #                              "NrOfStrippedSequencesIdentified (Experiment-wide)",
    #                              "CellularComponent", "BiologicalProcess", "MolecularFunction")
    
    report_column_names_keep = c("ProteinGroups", "ProteinNames", "Genes", "ProteinDescriptions","FastaFiles", "FastaHeaders",
                                 "CellularComponent", "BiologicalProcess", "MolecularFunction", "UniquePeptides")
    
    stats_df = spectroStats()
    quant_df = spectroQuant()

    stats2 = stats_df %>% # remember this data is in the long format until the end when i pivot_wider
      dplyr::select(., comparison = starts_with("Comparison"), Group,
                    log2FC = `AVG Log2 Ratio`, pvalue = Pvalue, qvalue = Qvalue) %>%
      dplyr::filter(!grepl("pool", tolower(comparison))) %>% # remove comparisons that contain the word "pool"
      tidyr::pivot_wider(., names_from = comparison, values_from = c(log2FC, pvalue, qvalue))

    quant2 = quant_df %>%
      dplyr::select(., any_of(c("PG.ProteinGroups", "PG.ProteinNames", "PG.Genes", "PG.ProteinDescriptions", "PG.FastaFiles", "PG.NrOfStrippedSequencesIdentified (Experiment-wide)",
                                "PG.FastaHeaders", "PG.CellularComponent", "PG.BiologicalProcess", "PG.MolecularFunction")), ends_with("PG.Quantity")) %>%
      dplyr::rename(., "UniquePeptides" = `PG.NrOfStrippedSequencesIdentified (Experiment-wide)`) %>%
      dplyr::rename_with(., .cols = !ends_with("PG.Quantity"), ~gsub("^.*\\.", "", .x)) %>%      
      dplyr::select(., any_of(report_column_names_keep), ends_with("PG.Quantity")) %>%
      dplyr::mutate("SummedQuantity" = round(rowSums(across(ends_with("PG.Quantity")), na.rm=TRUE)),0) %>%
      dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~round(.x, 4))) %>%
      dplyr::left_join(., stats2, by = c("ProteinGroups" = "Group")) %>%
      dplyr::select(., any_of(c(report_column_names_keep[1:10], "SummedQuantity")), # unique peptides column comes from the candidates dataframe
                    starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue"), ends_with("PG.Quantity")) %>%
      dplyr::rename_all(~str_replace_all(., "\\s+", "")) %>%
      dplyr::arrange(., desc(SummedQuantity)) %>%
      Filter(function(x) !all(is.na(x)), .) # removes any columns that only contain NA's, mostly used for GO term columns that are empty.

    
    if(input$transformSpectro == "Log2"){
      
      quant2 = quant2 %>%
        dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~log2(.x), .names = "log2_{.col}"), .keep = "unused") %>%
        dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~round(.x, 4))) %>%
        dplyr::arrange(., desc(SummedQuantity))
       
    }
    if(input$transformSpectro == "Both"){
      
      cols_to_keep = names(quant2)[!grepl("PG.Quantity", names(quant2))]
      
      quant2 = quant2 %>%
        dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~log2(.x), .names = "log2_{.col}")) %>%
        dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~round(.x, 4))) %>%
        dplyr::select(., any_of(cols_to_keep), starts_with("log2_"), starts_with("[")) %>%
        dplyr::arrange(., desc(SummedQuantity))
    }  
    
    
    # if condition file was added, rename the quant column to match
    # for some reason this code works when run on its own but not when run within Shiny??
    if(input$AddConditions == "Yes"){
      
      cond_df = spectroCond()
      
      var_names = cond_df %>%
        dplyr::select(., all_of(c("num", "Run Label", "Condition", "Replicate"))) %>%
        dplyr::mutate(current_names = paste0("log2_", "[", num, "]", `Run Label`, ".PG.Quantity")) %>%
        dplyr::mutate(samp_names = paste0("Log2_", Condition, "_", Replicate, "_Quantity")) %>%
        dplyr::select(., all_of(c("samp_names", "current_names"))) %>%
        tibble::deframe()
      
      quant2 = quant2 %>%
        dplyr::rename(!!!var_names)
      
    }
    
    # removes any comparisons that have "pool" in them
    stats_rmPool = stats_df %>%
      dplyr::filter(!grepl("pool", tolower(`Comparison (group1/group2)`)))
    
    # creates the comparison tabs by filtering on either p-value or q-value
    ind_comps = lapply(gsub("\\s+", "", unique(stats_rmPool$`Comparison (group1/group2)`)), function(x){
      
      int = quant2 %>%
        #dplyr::filter(., !!as.symbol(paste0("log2FC_",x)) > 0.6 | !!as.symbol(paste0("log2FC_", x)) < -0.6) %>%
        dplyr::filter(., !!as.symbol(paste0(input$statsFilter, "_", x)) < input$statsValue) %>%
        dplyr::arrange(., desc(!!as.symbol(paste0("log2FC_", x))))
      
    })
    
    lst = c(list(quant2), ind_comps)
    names(lst) = spectroSheetNames()
    
    # change the order of the p-values or q-values based on which was filtered on
    if(input$statsFilter == "pvalue"){
      lst = lapply(lst, function(x){
        
        tmp = x %>%
          dplyr::select(., any_of(c(report_column_names_keep[1:10], "SummedQuantity")),
                        starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue"), ends_with("Quantity"))
      
        })
    }
    if(input$statsFilter == "qvalue"){
      lst = lapply(lst, function(x){
        
        tmp = x %>%
          dplyr::select(., any_of(c(report_column_names_keep[1:10], "SummedQuantity")), 
                        starts_with("log2FC"), starts_with("qvalue"), starts_with("pvalue"), ends_with("Quantity"))
      
        })
    }
    
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
      if(input$PerseusFilterOption == "pval & log2FC"){
        
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
      paste0(format(Sys.time(),'%Y%m%d'), "_", input$outputFileName, "_Results.xlsx" )
    },
    content = function(file){
      hs = createStyle(textDecoration = "Bold", wrapText = TRUE)
      write.xlsx(finalOut(), file,
                 sheetName = finalSheetnames(), overwrite = TRUE, headerStyle = hs, keepNA = TRUE)
    }
    
  )
  

}

shinyApp(ui, server)
