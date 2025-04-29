# Load Packages ---

if(!require(markdown)){
  install.packages("markdown")
  library(markdown)
}

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
                  contains("Difference"), contains("p-value"), contains("q-value", ignore.case = FALSE), contains("Significant"), starts_with("LFQ intensity"), starts_with("MS/MS count")) %>%
    dplyr::mutate(across(.cols = starts_with("LFQ intensity"), ~round(.x, 4)),
           across(.cols = contains("Difference"), ~round(.x, 4))) %>%
    dplyr::mutate("Summed LFQ Intensity" = round(rowSums(2^across(.cols = starts_with("LFQ intensity")), na.rm=TRUE)), 0) %>%
    dplyr::select(., any_of(c("Majority protein IDs", "Protein names", "Gene names", "Gene name", "Fasta headers", "Razor + unique peptides", "Potential contaminant")),
                  contains("Difference"), contains("p-value"), contains("q-value"), contains("Significant"), starts_with("LFQ intensity"), "Peptides",
                  "Unique peptides", "Sequence coverage [%]", starts_with("Sequence coverage"), "Summed LFQ Intensity", starts_with("MS/MS count"),
                  -contains("significant", ignore.case=FALSE)) %>%
    dplyr::arrange(.,desc(`Summed LFQ Intensity`))
  
  
}

APMS_FP = function(df){
  
  df %>%
    dplyr::select(., any_of(c("Protein ID", "Entry Name", "Gene", "Description" ,"Organism", "Indistinguishable Proteins", "Protein Length", 
                              "Combined Total Peptides")), 
                  contains("Difference"), contains("p-value"), contains("q-value", ignore.case = FALSE), contains("Significant"), 
                  ends_with("MaxLFQ Intensity"), ends_with("Spectral Count"), -any_of(c("Combined Unique Spectral Count", "Combined Spectral Count"))) %>%
    dplyr::mutate(across(.cols = starts_with("LFQ intensity"), ~round(.x, 4)),
                  across(.cols = contains("Difference"), ~round(.x, 4))) %>%
    dplyr::mutate("Summed LFQ Intensity" = round(rowSums(2^across(.cols = ends_with("MaxLFQ Intensity")), na.rm=TRUE)), 0) %>%
    dplyr::select(., any_of(c("Protein ID", "Entry Name", "Gene", "Description" ,"Organism", "Indistinguishable Proteins", "Protein Length", 
                              "Combined Total Peptides", "Summed LFQ Intensity")), 
                  contains("Difference"), contains("p-value"), contains("q-value", ignore.case = FALSE), contains("Significant"), 
                  ends_with("MaxLFQ Intensity"), ends_with("Spectral Count"),
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


options(shiny.maxRequestSize=1000*1024^2) # increase server.R limit to 200MB file size


ui <- navbarPage("Table Converter",
                 tabPanel("File Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select data type"),
                              radioButtons("SearchEngine", "Select Seach Engine Used:",
                                           choices = c("Spectronaut", "Byos", "MQ-Perseus", "FP-Perseus", "MIBS-SpN")),
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
                                radioButtons("SpectroDataType", "Data Type:", choices = c("Protein", "PTM"), selected = "Protein"),
                                conditionalPanel(
                                  condition = "input.SpectroDataType == 'PTM'",
                                  checkboxGroupInput("SpNPTMsKeep", "Which PTMs do you want to keep:",
                                                     choices = NULL),
                                ),
                                radioButtons("AddConditions", "Import SpN condition setup FIle: yes/no", choices = c("Yes", "No"), selected = "No"),
                                conditionalPanel(
                                  condition = "input.AddConditions == 'Yes'",
                                  fileInput("SpectroConditions", "Select a Condition file:",
                                            multiple = FALSE,
                                            accept = c(".tsv"))
                                ),
                                checkboxGroupInput("SpNcomparisons", "Choose which comparisons to keep:",
                                                   choices = NULL),
                                radioButtons("transformSpectro", "How do you want the sample quantity values",
                                             choices = c("non-transformed", "Log2", "Both"), selected = "Both"),
                                radioButtons("statsFilter", "stats value to filter on:", c("p-value" = "pvalue","q-value" = "qvalue"), selected = "qvalue"),
                              ),
                              conditionalPanel(
                                h3("MQ-Perseus"),
                                condition = "input.SearchEngine == 'MQ-Perseus'", # can this have 2 options
                                fileInput("perseusUnimputedFile", "Select the UNIMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt")),
                                fileInput("perseusImputedFile", "Select the IMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt"))
                              ),
                              conditionalPanel(
                                h3("FP-Perseus"),
                                condition = "input.SearchEngine == 'FP-Perseus'", # can this have 2 options
                                fileInput("perseusUnimputedFile", "Select the UNIMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt")),
                                fileInput("perseusImputedFile", "Select the IMPUTED Perseus txt file:",
                                          multiple = FALSE,
                                          accept = c(".txt")),
                                radioButtons("statsFilter", "stats value to filter on:", c("p-value" = "pvalue","q-value" = "qvalue"), 
                                             selected = "pvalue"),
                                
                              ),
                              conditionalPanel(
                                h3("MIBS-SpN"),
                                condition = "input.SearchEngine == 'MIBS-SpN'", # can this have 2 options
                                fileInput("SpectroStatsFile", "Select a Candidate Stats file:",
                                          multiple = FALSE,
                                          accept = c(".tsv")), 
                                fileInput("SpectroQuantFile", "Select the Report quant file:",
                                          multiple = FALSE,
                                          accept = c(".tsv")),
                                fileInput("SpectroConditions", "Select a Condition file:",
                                          multiple = FALSE,
                                          accept = c(".tsv")),
                                fileInput("KinaseDatabase", "Select the Kinase Database:",
                                          multiple = FALSE,
                                          accept = c(".xlsx")),
                                checkboxGroupInput("KinaseSpecies", "Choose which Species to use:",
                                                   choices = NULL)
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
    #file = fread(input$SpectroStatsFile$datapath, sep = "\t", na.strings = c("NA", "NaN"), nThread = 2)
    file = vroom(input$SpectroStatsFile$datapath, delim = "\t", na = c("NA", "NaN"), show_col_types = FALSE, progress = FALSE, num_threads = 2)
    return(file)
    
  })
  
  spectroQuant = reactive({
    
    req(input$SpectroQuantFile)
    #file = read.delim(input$SpectroQuantFile$datapath, sep = "\t", check.names = FALSE)
    file = vroom(input$SpectroQuantFile$datapath, delim = "\t", na = c("NA", "NaN", "Filtered"), show_col_types = FALSE, progress = FALSE, num_threads = 2)
    return(file)
    
  })
  
  spectroCond = reactive({
    
    req(input$SpectroConditions)
    #file = read.delim(input$SpectroQuantFile$datapath, sep = "\t", check.names = FALSE)
    file = vroom(input$SpectroConditions$datapath, delim = "\t", na = c("NA", "NaN"), show_col_types = FALSE, progress = FALSE, num_threads = 1)
    return(file)
    
  })
  
  
  spectroSheetNames = reactive({
    
    df = spectroStats()
    
    rm_pool = df %>%
      #filter(!grepl("pool", tolower(`Comparison (group1/group2)`))) %>%
      dplyr::filter(., `Comparison (group1/group2)` %in% input$SpNcomparisons)
      
    sheetnames = unique(rm_pool$`Comparison (group1/group2)`) %>%
      gsub(" ", "", .) %>%
      gsub("/", " v ", .)
    
    names = c("summary_stats", "Proteins", sheetnames)
    
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
    
    
    tab_names = paste0("comp_", 1:length(tabNames))
    
    # truncate_to_30 <- function(strings) {
    #   sapply(strings, function(x) {
    #     if (nchar(x) > 30) {
    #       substr(x, 1, 30)
    #     } else {
    #       x
    #     }
    #   }, USE.NAMES = FALSE)
    # }
    # 
    # tabnames = truncate_to_30(tabNames)
    
    # tabNames = remove_common_words(tabNames)
    
    tabnames = c("Proteins", "Imputed", tab_names)
    
  })
  
  
  kinaseDatabase = reactive({
    
    req(input$KinaseDatabase)
    #file = read.delim(input$SpectroQuantFile$datapath, sep = "\t", check.names = FALSE)
    file = read.xlsx(input$KinaseDatabase$datapath)
    return(file)
    
  })
  
  observeEvent(kinaseDatabase(), {

    df = kinaseDatabase()
    updateCheckboxGroupInput(session, "KinaseSpecies", choices = c(names(df)[4:length(df)], "None"))

  })
  
  
  observeEvent(spectroStats(), {
    
    
    updateCheckboxGroupInput(session, "SpNcomparisons", choices = sort(unique(spectroStats()$`Comparison (group1/group2)`)))
    
  })
  
  # make this only observe if data type == "PTM"
  observeEvent(input$SpectroDataType == "PTM",{
    
    updateCheckboxGroupInput(session, "SpNPTMsKeep", choices = unique(spectroQuant()$`PTM.ModificationTitle`))
    
  })
  
  
  finalSheetnames = reactive({
    
    switch(input$SearchEngine,
           "MQ-Perseus" = perseusSheetNames(), 
           "Byos" = ByosSheetNames(),
           "Spectronaut" = spectroSheetNames(),
           "FP-Perseus" = perseusSheetNames())
    
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
    
    stats_df = spectroStats()
    quant_df = spectroQuant()

    stats2 = stats_df %>% # remember this data is in the long format until the end when i pivot_wider
      dplyr::filter(., if_any(matches("Valid"), ~Valid == TRUE)) %>% # if the valid column exists, filter it for only true values
      dplyr::select(., comparison = starts_with("Comparison"), Group,
                    log2FC = `AVG Log2 Ratio`, pvalue = Pvalue, qvalue = Qvalue) %>%
      dplyr::filter(., comparison %in% input$SpNcomparisons) %>%
      #dplyr::filter(!grepl("pool", tolower(comparison))) %>% # remove comparisons that contain the word "pool", should not need now with SpNcomparisons
      tidyr::pivot_wider(., names_from = comparison, values_from = c(log2FC, pvalue, qvalue))

    
    if(input$SpectroDataType == "Protein"){
      
      report_column_names_keep = c("ProteinGroups", "ProteinNames", "Genes", "ProteinDescriptions","FastaFiles", "FastaHeaders",
                                   "CellularComponent", "BiologicalProcess", "MolecularFunction", "UniquePeptides")
      
      
      quant2 = quant_df %>%
        dplyr::select(., any_of(c("PG.ProteinGroups", "PG.ProteinNames", "PG.Genes", "PG.ProteinDescriptions", "PG.FastaFiles", "PG.NrOfStrippedSequencesIdentified (Experiment-wide)",
                                  "PG.FastaHeaders", "PG.CellularComponent", "PG.BiologicalProcess", "PG.MolecularFunction")), ends_with("PG.Quantity")) %>%
        dplyr::rename(., "UniquePeptides" = `PG.NrOfStrippedSequencesIdentified (Experiment-wide)`) %>%
        dplyr::rename_with(., .cols = !ends_with("PG.Quantity"), ~gsub("^.*\\.", "", .x)) %>%      
        dplyr::select(., any_of(report_column_names_keep), ends_with("PG.Quantity")) %>%
        dplyr::mutate("SummedQuantity" = round(rowSums(across(ends_with("PG.Quantity")), na.rm=TRUE)),0) %>%
        dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~round(.x, 4))) %>%
        dplyr::left_join(., stats2, by = c("ProteinGroups" = "Group")) %>%
        dplyr::select(., any_of(c(report_column_names_keep, "SummedQuantity")), # unique peptides column comes from the candidates dataframe
                      starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue"), ends_with("PG.Quantity")) %>%
        dplyr::rename_all(~str_replace_all(., "\\s+", "")) %>%
        dplyr::mutate("Contaminant" = grepl("contaminants", FastaFiles)) %>%
        dplyr::arrange(., desc(SummedQuantity)) %>%
        Filter(function(x) !all(is.na(x)), .) # removes any columns that only contain NA's, mostly used for GO term columns that are empty.
      
    }
    
    if(input$SpectroDataType == "PTM"){
      
      report_column_names_keep = c("ProteinId", "ProteinNames", "Genes", "ProteinDescriptions","FastaFiles", "FastaHeaders",
                                   "CellularComponent", "BiologicalProcess", "MolecularFunction", "UniquePeptides",
                                   "CollapseKey", "Multiplicity", "ModificationTitle", "SiteAA", "SiteLocation", "FlankingRegion")
      
      
      quant2 = quant_df %>%
        dplyr::select(., any_of(c("PTM.ProteinId", "PG.Genes", "PG.ProteinDescriptions", "PG.FastaFiles", "PG.FastaHeaders", 
                                  "PTM.CollapseKey", "PTM.Multiplicity", "PTM.ModificationTitle", "PTM.SiteAA", "PTM.SiteLocation", "PTM.FlankingRegion",
                                  "PG.CellularComponent", "PG.BiologicalProcess", "PG.MolecularFunction")), ends_with("PTM.Quantity")) %>%
        #dplyr::rename(., "UniquePeptides" = `PG.NrOfStrippedSequencesIdentified (Experiment-wide)`) %>%
        dplyr::rename_with(., .cols = !ends_with("PTM.Quantity"), ~gsub("^.*\\.", "", .x)) %>%   # removes everything before . in column names, except quant values   
        dplyr::select(., any_of(report_column_names_keep), ends_with("PTM.Quantity")) %>%
        dplyr::mutate("SummedQuantity" = round(rowSums(across(ends_with("PTM.Quantity")), na.rm=TRUE)),0) %>%
        dplyr::mutate(across(.cols = ends_with("PTM.Quantity"), ~round(.x, 4))) %>%
        dplyr::left_join(., stats2, by = c("CollapseKey" = "Group")) %>%
        dplyr::select(., any_of(c(report_column_names_keep, "SummedQuantity")), # unique peptides column comes from the candidates dataframe
                      starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue"), ends_with("PTM.Quantity")) %>%
        dplyr::rename_all(~str_replace_all(., "\\s+", "")) %>%
        dplyr::filter(., ModificationTitle %in% input$SpNPTMsKeep) %>%
        dplyr::arrange(., desc(SummedQuantity)) %>%
        Filter(function(x) !all(is.na(x)), .) # removes any columns that only contain NA's, mostly used for GO term columns that are empty.
      
    }
    
    
    if(input$transformSpectro == "Log2"){
      
      quant2 = quant2 %>%
        dplyr::mutate(across(.cols = matches("[PG|PTM].Quantity"), ~log2(.x), .names = "log2_{.col}"), .keep = "unused") %>%
        dplyr::mutate(across(.cols = matches("[PG|PTM].Quantity"), ~round(.x, 4))) %>%
        dplyr::arrange(., desc(SummedQuantity))
       
    }
    if(input$transformSpectro == "Both"){
      
      cols_to_keep = names(quant2)[!grepl("[PG|PTM].Quantity", names(quant2))]
      
      quant2 = quant2 %>%
        dplyr::mutate(across(.cols = matches("[PG|PTM].Quantity"), ~log2(.x), .names = "log2_{.col}")) %>%
        dplyr::mutate(across(.cols = matches("[PG|PTM].Quantity"), ~round(.x, 4))) %>%
        dplyr::select(., any_of(cols_to_keep), starts_with("log2_"), starts_with("[")) %>%
        dplyr::arrange(., desc(SummedQuantity))
    }  
    
    # change the order of the p-values or q-values based on which was filtered on
    if(input$statsFilter == "pvalue"){
      
      quant2 = quant2 %>%
        dplyr::select(., any_of(c(report_column_names_keep, "SummedQuantity", "Contaminant")),
                      starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue"), matches("[PG|PTM].Quantity")) %>%
        dplyr::relocate(., "Contaminant", .after = "FastaFiles")
        
    }
    if(input$statsFilter == "qvalue"){

      quant2 = quant2 %>%
        dplyr::select(., any_of(c(report_column_names_keep, "SummedQuantity", "Contaminant")),
                      starts_with("log2FC"), starts_with("qvalue"), starts_with("pvalue"), matches("[PG|PTM].Quantity")) %>%
        dplyr::relocate(., "Contaminant", .after = "FastaFiles")
        
    }

    # if condition file was added, rename the quant column to match
    # for some reason this code works when run on its own but not when run within Shiny??
    if(input$AddConditions == "Yes"){
      
      cond_df = spectroCond()
      
      if(input$SpectroDataType == "Protein"){
        
        
        if(input$transformSpectro == "Log2"){
          
          var_names = cond_df %>%
            dplyr::select(., any_of(c("Run Label", "Condition", "Replicate"))) %>%
            dplyr::mutate(num = seq(1:nrow(.))) %>%
            dplyr::mutate(current_names = paste0("log2_", "[", num, "]", `Run Label`, ".PG.Quantity")) %>%
            dplyr::mutate(samp_names = paste0("Log2_", Condition, "_", Replicate, "_Quantity")) %>%
            dplyr::select(., all_of(c("samp_names", "current_names"))) %>%
            tibble::deframe()
          
        }
        if(input$transformSpectro == "Both"){
          
          new_log_names = cond_df %>%
            dplyr::select(., any_of(c("Run Label", "Condition", "Replicate"))) %>%
            dplyr::mutate(num = seq(1:nrow(.))) %>%
            dplyr::mutate(current_names = paste0("log2_", "[", num, "]", `Run Label`, ".PG.Quantity")) %>%
            dplyr::mutate(samp_names = paste0("Log2_", Condition, "_", Replicate, "_Quantity")) %>%
            dplyr::select(., all_of(c("samp_names", "current_names"))) %>%
            tibble::deframe()
          
          new_nonlog_names = cond_df %>%
            dplyr::select(., any_of(c("Run Label", "Condition", "Replicate"))) %>%
            dplyr::mutate(num = seq(1:nrow(.))) %>%
            dplyr::mutate(current_names = paste0("[", num, "]", `Run Label`, ".PG.Quantity")) %>%
            dplyr::mutate(samp_names = paste0(Condition, "_", Replicate, "_Quantity")) %>%
            dplyr::select(., all_of(c("samp_names", "current_names"))) %>%
            tibble::deframe()
          
          var_names = c(new_log_names, new_nonlog_names)
          
        }
      }
      
      if(input$SpectroDataType == "PTM"){
        
        var_names = cond_df %>%
          dplyr::select(., any_of(c("Run Label", "Condition", "Replicate"))) %>%
          dplyr::mutate(num = seq(1:nrow(.))) %>%
          dplyr::mutate(current_names = paste0("log2_", "[", num, "]", `Run Label`, ".PTM.Quantity")) %>%
          dplyr::mutate(samp_names = paste0("Log2_", Condition, "_", Replicate, "_Quantity")) %>%
          dplyr::select(., all_of(c("samp_names", "current_names"))) %>%
          tibble::deframe()
        
      }
      
      quant2 = quant2 %>%
        dplyr::rename(!!!var_names)
      
    }
    
    # calcualte CVs
    
    cv = function(df){
      
      int = df %>%
        dplyr::select(., ProteinGroups, starts_with("Log2") & ends_with("Quantity")) %>%
        tidyr::pivot_longer(., cols = ends_with("Quantity"), names_to = "BioReplicate") %>%
        dplyr::mutate(Condition = gsub("Log2_|_Quantity", "", BioReplicate)) %>%
        dplyr::mutate(Condition = sub("_[^_]+$", "", Condition)) %>%
        dplyr::mutate(unlog_abun = 2^value) %>%
        dplyr::group_by(Condition, ProteinGroups) %>%
        dplyr::reframe(CV = round(sd(unlog_abun, na.rm=TRUE) / mean(unlog_abun, na.rm=TRUE) * 100, 2)) %>%
        tidyr::pivot_wider(names_from = Condition, values_from = CV, names_prefix = "CV_") %>%
        dplyr::ungroup()
      
      return(int)
    }
    
    quant2_cv = cv(quant2)
    
    # ERROR getting error here, but if i continue it works fine.
    quant2 = quant2 %>%
      dplyr::left_join(., quant2_cv, by = "ProteinGroups") 
    
    quant2 = quant2 %>%
      dplyr::relocate(grep("CV_", names(quant2_cv), value = T), .after = SummedQuantity)
    
    # removes any comparisons that weren't choosen
    stats_rmPool = stats_df %>%
      #dplyr::filter(!grepl("pool", tolower(`Comparison (group1/group2)`))) %>%
      dplyr::filter(., `Comparison (group1/group2)` %in% input$SpNcomparisons)
    
    # creates the comparison tabs by filtering on either p-value or q-value
    # removed the log2FC filter forthe separate tabs
    ind_comps = lapply(gsub("\\s+", "", unique(stats_rmPool$`Comparison (group1/group2)`)), function(x){
      
      int = quant2 %>%
        #dplyr::filter(., !!as.symbol(paste0("log2FC_",x)) > 0.6 | !!as.symbol(paste0("log2FC_", x)) < -0.6) %>%
        dplyr::filter(., !!as.symbol(paste0(input$statsFilter, "_", x)) < 0.05) %>%
        dplyr::arrange(., desc(!!as.symbol(paste0("log2FC_", x))))
      
    })
    
    summary_stats = function(df){
      
      proteins = nrow(df)
      contams = sum(df$Contaminant)
      
      single_hits = nrow(filter(df, UniquePeptides == 1))
      med_cv = apply(quant2_cv[2:length(quant2_cv)], 2, function(x) median(x, na.rm=TRUE))
      
      comparisons = gsub("log2FC_", "", grep("log2FC", names(df), value = TRUE))

      comp_stats = function(df, comp){
        
        p_filt_pos = df %>%
          dplyr::select(., contains(comp)) %>%
          dplyr::filter(., !!sym(paste0("log2FC_", comp)) > 0.6 & !!sym(paste0("pvalue_", comp)) < 0.05) %>%
          nrow()
        
        p_filt_neg = df %>%
          dplyr::select(., contains(comp)) %>%
          dplyr::filter(., !!sym(paste0("log2FC_", comp)) < -0.6 & !!sym(paste0("pvalue_", comp)) < 0.05) %>%
          nrow()
        
        q_filt_pos = df %>%
          dplyr::select(., contains(comp)) %>%
          dplyr::filter(., !!sym(paste0("log2FC_", comp)) > 0.6 & !!sym(paste0("qvalue_", comp)) < 0.05) %>%
          nrow()
        
        q_filt_neg = df %>%
          dplyr::select(., contains(comp)) %>%
          dplyr::filter(., !!sym(paste0("log2FC_", comp)) < -0.6 & !!sym(paste0("qvalue_", comp)) < 0.05) %>%
          nrow()
        
        
        tot = data.frame(p_filt_pos, p_filt_neg, q_filt_pos, q_filt_neg)
        colnames(tot) = paste0(c("pvalue_greater","pvalue_less",  "qvalue_greater", "qvalue_less"), comp)
        
        return(tot)
        
      }
       
      comp_lst = lapply(comparisons, function(x) comp_stats(df, x))
      
      final = t(data.frame("total_proteins" = proteins,
                              "total_contams" = contams,
                              "single_hits" = single_hits,
                              "medium" = t(med_cv),
                              comp_lst))
      
      final = data.frame(columns = rownames(final),
                          "summary stats" = final[,1])
      

      return(final)
    }
    
    summary = summary_stats(quant2)
    
    lst = c(list(summary), list(quant2), ind_comps)
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
      
      filt = map2(log_names, pvalue_names, function(x,y){
        int = cleaned_imputed %>%
          dplyr::filter(., !!as.symbol(y) < 0.05) %>%
          dplyr::arrange(., desc((!!as.symbol(x))))
      })
      
      lst = c(list(cleaned_unimputed, cleaned_imputed), filt)
      names(lst) = perseusSheetNames()
      
      return(lst)
    }
    
  })
  
  conv_Perseus_FP = eventReactive(list(input$convert, input$SearchEngine == "FP-Perseus|PD-Perseus"),{
    
    
    if(input$SearchEngine == "FP-Perseus"){
      
      cleaned_unimputed = APMS_FP(df = perseusUnImputed())
      cleaned_imputed = APMS_FP(df = perseusImputed())
      
      
      if(input$statsFilter == "pvalue"){
        
        log_names = names(dplyr::select(cleaned_imputed, contains("Difference")))
        pvalue_names = names(dplyr::select(cleaned_imputed, contains("p-value")))
        
        filt = map2(log_names, pvalue_names, function(x,y){
          int = cleaned_imputed %>%
            dplyr::filter(., !!as.symbol(y) < 0.05) %>%
            dplyr::arrange(., desc((!!as.symbol(x))))
        })
      }
      
      if(input$statsFilter == "qvalue"){
        
        log_names = names(dplyr::select(cleaned_imputed, contains("Difference")))
        pvalue_names = names(dplyr::select(cleaned_imputed, contains("q-value")))
        
        filt = map2(log_names, pvalue_names, function(x,y){
          int = cleaned_imputed %>%
            dplyr::filter(., !!as.symbol(y) < 0.05) %>%
            dplyr::arrange(., desc((!!as.symbol(x))))
        })
      }
      
      lst = c(list(cleaned_unimputed, cleaned_imputed), filt)
      names(lst) = perseusSheetNames()
      
      return(lst)
    }
    
  })
  
  
  conv_MIBS_SpN = eventReactive(list(input$convert, input$SearchEngine == "MIBS-SpN"),{
    
    stats_df = spectroStats()
    quant_df = spectroQuant()
    
    stats2 = stats_df %>% # remember this data is in the long format until the end when i pivot_wider
      dplyr::filter(., if_any(matches("Valid"), ~Valid == TRUE)) %>% # if the valid column exists, filter it for only true values
      dplyr::select(., comparison = starts_with("Comparison"), Group,
                    log2FC = `AVG Log2 Ratio`) %>%
      dplyr::filter(., comparison %in% input$SpNcomparisons) %>%
      #dplyr::filter(!grepl("pool", tolower(comparison))) %>% # remove comparisons that contain the word "pool", should not need now with SpNcomparisons
      tidyr::pivot_wider(., names_from = comparison, values_from = c(log2FC))
    
    
    report_column_names_keep = c("ProteinGroups", "ProteinNames", "Genes", "ProteinDescriptions","FastaFiles", "FastaHeaders",
                                 "CellularComponent", "BiologicalProcess", "MolecularFunction", "UniquePeptides")
    
    quant2 = quant_df %>%
      dplyr::select(., any_of(c("PG.ProteinGroups", "PG.ProteinNames", "PG.Genes", "PG.ProteinDescriptions", "PG.FastaFiles", "PG.NrOfStrippedSequencesIdentified (Experiment-wide)",
                                "PG.FastaHeaders", "PG.CellularComponent", "PG.BiologicalProcess", "PG.MolecularFunction")), ends_with("PG.Quantity")) %>%
      dplyr::rename(., "UniquePeptides" = `PG.NrOfStrippedSequencesIdentified (Experiment-wide)`) %>%
      dplyr::rename_with(., .cols = !ends_with("PG.Quantity"), ~gsub("^.*\\.", "", .x)) %>%      
      dplyr::select(., any_of(report_column_names_keep), ends_with("PG.Quantity")) %>%
      dplyr::mutate("SummedQuantity" = round(rowSums(across(ends_with("PG.Quantity")), na.rm=TRUE)),0) %>%
      dplyr::mutate(across(.cols = ends_with("PG.Quantity"), ~round(.x, 4))) %>%
      dplyr::left_join(., stats2, by = c("ProteinGroups" = "Group")) %>%
      dplyr::select(., any_of(c(report_column_names_keep, "SummedQuantity")), # unique peptides column comes from the candidates dataframe
                    starts_with("log2FC"), starts_with("pvalue"), starts_with("qvalue"), ends_with("PG.Quantity")) %>%
      dplyr::rename_all(~str_replace_all(., "\\s+", "")) %>%
      dplyr::mutate("Contaminant" = grepl("contaminants", FastaFiles)) %>%
      dplyr::arrange(., desc(SummedQuantity)) %>%
      Filter(function(x) !all(is.na(x)), .) %>% # removes any columns that only contain NA's, mostly used for GO term columns that are empty.
      dplyr::relocate(., "Contaminant", .after = "FastaFiles") %>%
      dplyr::mutate(across(.cols = matches("[PG|PTM].Quantity"), ~log2(.x), .names = "log2_{.col}"), .keep = "all") %>% # log2 transform data
      dplyr::mutate(across(.cols = matches("[PG|PTM].Quantity"), ~round(.x, 4))) %>% # round log2 values 
      dplyr::select(., any_of(c(report_column_names_keep, "SummedQuantity", "Contaminant")),
                    starts_with("log2FC"), starts_with("log2_"), ends_with("Quantity")) %>% 
      dplyr::arrange(., desc(SummedQuantity)) # sort by decending summed Quantity
    
      
    # if condition file was added, rename the quant column to match
    # for some reason this code works when run on its own but not when run within Shiny??

    cond_df = spectroCond()

    var_names = cond_df %>%
      dplyr::select(., any_of(c("Run Label", "Condition", "Replicate"))) %>%
      dplyr::mutate(num = seq(1:nrow(.))) %>%
      dplyr::bind_rows(.,.) %>%
      dplyr::mutate(current_names = paste0("log2_", "[", num, "]", `Run Label`, ".PG.Quantity")) %>% # rep names 2 times, for log2 and for non-transformed
      dplyr::mutate(samp_names = paste0(rep(c("Log2_", ""), each = max(num)), Condition, "_", Replicate, "_Quantity")) %>%
      dplyr::select(., all_of(c("samp_names", "current_names"))) %>%
      tibble::deframe()
          
    quant2 = quant2 %>%
      dplyr::rename(!!!var_names)
      
    # get kinase database
    # if no database is selected use the term "kinase" in the protein description column
    
    kiansedb = kinaseDatabase()
    
    if(input$KinaseSpecies == "None"){
      
      quant3 = quant2 %>%
        mutate("Kinase.Family" = grepl("kinase", ProteinDescriptions)) %>%
        dplyr::relocate(., "Kinase.Family", .after = "FastaFiles")
      
    }else{
      kinasedb = kinasedb %>%
        dplyr::select("Human.Uniprot", `Kinase.Family`)
      #dplyr::select(input$KinaseSpecies, `Kinase Family`)
      
      quant3 = quant2 %>%
        left_join(., kinasedb, by = c(ProteinGroups = names(kinasedb[1]))) %>%
        dplyr::relocate(., "Kinase.Family", .after = "FastaFiles")
      
    }
    
    
    # filtering for kinase family members
    kinaseOnly = quant3 %>%
      dplyr::filter(!is.na(Kinase.Family))
    
    # QC
    # I think i'll have to pivot_longer the data, will need non-log2 quant values, kinase.family, and proteingroups columns
    # then group_by(Kinase.family) %>% summarize()
    qc = quant3 %>%
      dplyr::select(., any_of(c("ProteinGroups", "Kinase.Family")), ends_with("_Quantity")) %>%
      tidyr::pivot_longer(col = ends_with("Quantity"), values_to = "Log2_Quant", names_to = "samples") %>%
      dplyr::mutate(quant = 2^Log2_Quant) %>%
      dplyr::mutate(kinase = if_else(!is.na(Kinase.Family), TRUE, FALSE)) %>%
      dplyr::group_by(samples) %>%
      dplyr::summarize(kinase_sum = sum(quant[kinase == TRUE], na.rm=TRUE),
                       total_sum = sum(quant, na.rm = TRUE),
                       percent_enrichment = (kinase_sum / total_sum * 100))
    

    lst = c(list(quant3, qc, kinaseOnly))
    names(lst) = spectroSheetNames()
    
    return(lst) 
  })
  
  

  finalOut = reactive({
    
    switch(input$SearchEngine,
           "MQ-Perseus" = conv_Perseus_MQ(),
           "FP-Perseus" = conv_Perseus_FP(),
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
