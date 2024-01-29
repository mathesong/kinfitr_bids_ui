library(shiny)
library(shinythemes)
library(bslib)
library(glue)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(jsonlite)
require(shinyjs)
require(shinyFiles)
require(shiny.fluent)

shinyServer(
  function(input, output, session) {
    # Loading of BIDS directory and dynamic loading of the bids structure
    # TODO: load the directory and parse subfolders
    # TODO: find the _tacs.tsv file and load the structure
    # TODO: display amongst the available regions ONLY those in that specific file
    roots <- c('wd' = getwd())
    volumes <- shinyDirChoose(input, "bidsdir", 
                              roots=roots, 
                              allowDirCreate = FALSE)
    
    output$dirOutput <- renderPrint({
      req(input$dir)
      
      # Get the selected directory path
      selected_dir <- parseDirPath(volumes, input$dir)
      
      # List all directories within the selected directory
      if (!is.null(selected_dir$datapath)) {
        dirs <- list.dirs(path = selected_dir$datapath, 
                          full.names = TRUE, 
                          recursive = TRUE)
        return(dirs)
      } else {
        return(NULL)
      }
    })
    
    # Get the derivatives folder from the BIDS folder
    # derivatives_folder <- list.dirs(bidsdir, 
    #                                 pattern = "derivatives", 
    #                                 recursive = FALSE),
    # # User select the desired folder
    # 
    # # Get the available segmentation files
    # avail_seg <- available_region_files(derivatives_folder),
    # 
    # # User select file
    # seg_file <- "",
    # # Get region names
    # avail_rois <- tac_region_names(seg_file)
    
    # Reactive model based logic ----
    model1_parameters <- observe(
      
      if(input$button == "1TCM"){
        
        
      }
      else if(input[["button"]] == "2TCM"){
        observeEvent(input[["irreversible_trapping"]],
                     {
                       if(input[["irreversible_trapping"]]){
                         updateNumericInput(session, inputId = "k4.start", value = 0, min = 0, max = 0, step = 0)
                         updateNumericInput(session, inputId = "k4.lower", value = 0, min = 0, max = 0, step = 0)
                         updateNumericInput(session, inputId = "k4.upper", value = 0, min = 0, max = 0, step = 0)
                       } else {
                         updateNumericInput(session, inputId = "k4.start", value = 0.1, min = 0, max = 0, step = 0.001)
                         updateNumericInput(session, inputId = "k4.lower", value = 0.0001, min = 0, max = 0, step = 0.0001)
                         updateNumericInput(session, inputId = "k4.upper", value = 0.5, min = 0, max = 0, step = 0.001)
                       }
                     })
        # Parameters for the model to use for data fitting
      }
      else if(input[["button"]] == "Logan"){
        
      }
      else if(input[["button"]] == "t* finder"){
        
      }
      else if(input[["button"]] == "Fit Delay"){
        
      })
    
    
  }
)