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
                              fileInput("byosFile", "Select the Byos preprocessed xlsx results file:",
                                        multiple = FALSE,
                                        accept = c(".xlsx")),
                              br(),
                              fileInput("byosExpectedFile", "Optional: Select a xlsx file containing expected masses:"),
                              br(),
                              textInput("expectedMasses", label = "Optional: Write in the expected masses, only separated by a comma:",
                                        value = ""),
                              br(),
                              textInput("outputFileName", label = "Export file name:", value = "")
                              ),
                            conditionalPanel(
                              h3("Spectronaut"),
                              condition = "input.SearchEngine == 'Spectronaut'",
                              fileInput("SpectroQuantFile", "Choose a xlsx stats data file:",
                                        multiple = FALSE,
                                        accept = c(".xls")), # Can i have 2 fileInput options in a single conditional Panel?
                              br(),
                              fileInput("SpectroStatsFile", "Select the Byos preprocessed xlsx results file:",
                                        multiple = FALSE,
                                        accept = c(".xls")),
                              br(),
                              textInput("outputFileName", label = "Export file name:", value = "")
                              ),
                            conditionalPanel(
                              h3("Perseus"),
                              condition = "input.PostProcessing == 'Perseus'",
                              fileInput("perseusUnimputedFile", "Select the UNIMPUTED Perseus txt file:",
                                        multiple = FALSE,
                                        accept = c("txt")),
                              br(),
                              fileInput("perseusImputedFile", "Select the IMPUTED Perseus txt file:",
                                        multiple = FALSE,
                                        accept = c(".txt")),
                              br(),
                              textInput("outputFileName", label = "Export file name:", value = "")
                              )
                            ),
                            mainPanel(DT::dataTableOutput("dataframe"))
                          )))

server = function(input, output, session){
  
  byosDf = reactive({
    
    req(input$byosFile)
    sheet_names = excel_sheets(input$byosFile) # extract all excel sheet names
    file = lapply(sheet_names, function(x){
      as.data.frame(read_excel(input$byosFile, sheet = x)) # read in each tab
    })
    return(file)
    
  })
  
  
  byosExpectedMasses = reactive({
    
    req(input$byosExpectedFile)
    file = read.xlsx(input$byosExpectedFile, sheet=1)
    return(file)
    
  })
  

  spectroQuant = reactive({

    req(input$SpectroQuantFile)
    file = fread(input$SpectroQuantFile)
    return(file)

  })

  spectroStats = reactive({

    req(input$SpectroStatsFile)
    file = fread(input$SpectroStatsFile)
    return(file)

  })


  perseusUnImputed = reactive({

    req(input$perseusUnimputedFile)

    import_pers = function(file_loc){

      firstRead = read_delim(file_loc, delim = "\t", show_col_types = FALSE, na = c("", "NaN", "NA")) # used to determine the number of rows to skip

      lastRead = read_delim(file_loc, delim = "\t", skip = (sum(gregexpr("#!", firstRead[,1], fixed = TRUE)[[1]]>0) +1),
                             col_names = names(firstRead), na = c("", "NaN", "NA"), show_col_types = FALSE)
      # skips the first several rows which contain a #!
      # also renames columns from the first read, and converts blank, NaN and NA values to N/A

    }

    file = import_pers(file_loc = input$perseusUnimputedFile)
    return(file)

  })

  perseusImputed = reactive({

    req(input$perseusImputedFile)

    import_pers = function(file_loc){

      firstRead = read_delim(file_loc, delim = "\t", show_col_types = FALSE, na = c("", "NaN", "NA")) # used to determine the number of rows to skip

      lastRead = read_delim(file_loc, delim = "\t", skip = (sum(gregexpr("#!", firstRead[,1], fixed = TRUE)[[1]]>0) +1),
                            col_names = names(firstRead), na = c("", "NaN", "NA"), show_col_types = FALSE)
      # skips the first several rows which contain a #!
      # also renames columns from the first read, and converts blank, NaN and NA values to N/A

    }

    file = import_pers(file_loc = input$perseusImputedFile)
    return(file)

  })


  byos_expected = reactive({
    
    expected = input$expectedMasses
    
  })

  
  output$dataframe = DT::renderDataTable({
    
    if(input$SearchEngine == "Byos"){
      
      byosConvert = function(byos_file, expected_masses_file, expected_masses_vector, writtenFileName){
        
        if(is.null(expected_masses_file)){
          expected = expected_masses_vector
        }else{
          expected = read_excel(expected_masses_file)[,2]
          expected = unlist(expected)
        }
        
        sheet_names = excel_sheets(byos_file) 
        
        byos_files = lapply(sheet_names, function(x){
          as.data.frame(read_excel(byos_file, sheet = x))
        })
        
        names(byos_files) = sheet_names
        
        nist = byos_files[[1]] %>%
          dplyr::select(., Name, Mass, `Expected mass`, Intensity) %>%
          mutate(Mass = as.numeric(Mass),
                 Intensity = as.numeric(Intensity)) %>%
          arrange(desc(Intensity)) %>%
          mutate(`Expected mass` = as.numeric(`Expected mass`)) %>%
          mutate("Delta mass from expected" = Mass - `Expected mass`) %>%
          mutate("Delta mass from most intense" = dplyr::first(Mass) - Mass)  %>%
          mutate("Local Rel. Int. (%)" = round(Intensity / dplyr::first(Intensity) * 100, 2), "%") %>%
          dplyr::rename(., "Measured mass" = Mass) %>%
          mutate(Intensity = formatC(Intensity, format = "e", digits = 2)) %>%
          dplyr::select(., Name, `Measured mass`, `Expected mass`, 
                        `Delta mass from expected`,
                        `Delta mass from most intense`, Intensity, `Local Rel. Int. (%)`)
        
        samps = map2(expected, byos_files[2:length(byos_files)], function(x,y){
          
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
        
        hs = createStyle(textDecoration = "Bold", wrapText = TRUE)
        
        write.xlsx(c(list(nist), samps), file = paste0(format(Sys.time(),'%Y%m%d_%H%M'), "_", writtenFileName, "_postprocessed.xlsx" ), 
                   sheetName = sheet_names, overwrite = TRUE, headerStyle = hs)
        
        
      }
      
      
      byosConvert(byos_file = byosDf,
                  expected_masses_file = byosExpectedMasses(),
                  expected_masses_vector = byos_expected(),
                  writtenFileName = input$outputFileName)
      
      
    }
    
    
  })
  
  
  
}

shinyApp(ui, server)
