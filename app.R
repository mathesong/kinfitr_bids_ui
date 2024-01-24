#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(bslib)
library(glue)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(jsonlite)

# New filename
generate_new_filename <- function() {

  current_date <- as.character(as.Date(Sys.Date()))
  random_seq <- stringi::stri_rand_strings(1, 4)
  paste0("config_", current_date, "_id-", random_seq, ".json")

}

out_filename <- generate_new_filename()

# Clear out old files
json_files <- tibble::tibble(
  filename = list.files("output", pattern = ".json",full.names = T),
  ) %>%
  mutate(Date = stringr::str_match(filename, "config_(.*)_id")[,2],
         Days_ago = as.numeric(Sys.Date() - as.Date(Date)))

file.remove(json_files$filename[json_files$Days_ago > 7])




# Define UI for bloodstream app ----
ui <- fluidPage(theme = shinytheme("flatly"),

  # theme = bs_theme(version = 4, bootswatch = "cosmo",
  #                  #bg = "#0b3d91",
  #                  #fg = "white"
  #                  ),

  # App title ----
  titlePanel("Create a customised kinfitr BIDS App config file"),

  # Sidebar layout for subsetting ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      h2("Select BIDS Dataset"),
      fileInput("bidsdir", 
                "Choose a BIDS directory", 
                accept = "folder", 
                multiple = F),
      h2("Data Subset"),
      p(glue("Use these options to apply this config to a subset of the data. ",
             "Values should be separated by semi-colons. ",
             "All measurements fulfilling all the conditions will ",
             "be included. Leave options blank for no subsetting is desired, ",
             "i.e. leaving sub blank implies that all subjects should ",
             "be included."),
        style = "font-size:14px;"
      ),
      br(),
      textInput(inputId = "subset_sub", label = "sub", value = ""),
      textInput(inputId = "subset_ses", label = "ses", value = ""),
      textInput(inputId = "subset_rec", label = "rec", value = ""),
      textInput(inputId = "subset_tracer", label = "TracerName", value = ""),
      
      h2("Region"),
      p(glue("Use these options to define the region to which the model should ",
             "be applied. ",
             "Combine regions by pasting the region names separated by commas. ",
             "The seg refers to which segmentation the TACs are drawn from. ",
             "The derivative grob allows users to specify a specific set of TAC derivatives",
             ),
        style = "font-size:14px;"
      ),
      textInput(inputId = "region_name", label = "Region outputname (e.g. Hippocampus)", value = ""),
      textInput(inputId = "region_regions", label = "Constituent Regions (e.g. L_HIP,R_HIP)", value = ""),
      textInput(inputId = "region_seg", label = "Description (desc) of tacs file (e.g. gtmseg)", value = ""),
      textInput(inputId = "region_deriv", label = "Derivative folder grob (e.g. petprep)", value = ""),
      br(),

    ),

    # Main panel for modelling options ----
    mainPanel(

      h2("Modelling Selections"),
      p(glue("Select the modelling approach to fit the Time Activity Curves",
             " (TACs). You can select among different methods, either ",
             "compartmental models (e.g., 1TCM, 2TCM), and graphical models",
             "(e.g., Logan) to fit your data. Data to be fitted are exepcted to",
             " be derived from a  "),
        style = "font-size:14px;"
        ),
      br(),

      # Tabset
      tabsetPanel(type = "tabs",
                  tabPanel("Model",
                           # br(),
                           h4(""),
                           p(glue(""),
                             #style = "font-size:14px;"
                           ),
                           # Model selection drop-down menu
                           selectInput("button", "Select a model:", 
                                       choices = c("1TCM", 
                                                   "2TCM",
                                                   "Logan",
                                                   "Fit Delay",
                                                   "t* finder"
                                                   )),
                           # 1TCM selection panel
                           conditionalPanel(
                             condition = "input.button == '1TCM'",
                             fluidRow(
                               column(3, offset = 0, numericInput("K1.start", "K1.start", value = 0.1,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("K1.lower", "K1.lower", value = 0.0001,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("K1.upper", "K1.upper", value = 0.5,min = 0, step=.001)),
                             ),
                             fluidRow(
                               column(3, offset = 0, numericInput("k2.start", "k2.start", value = 0.1,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k2.lower", "k2.lower", value = 0.0001,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k2.upper", "k2.upper", value = 0.5,min = 0, step=.001)),
                             ),
                             fluidRow(
                               column(3, offset = 0, numericInput("vB.start", "vB.start", value = 0.05, min = 0, step=.001)),
                               column(3, offset = 0, numericInput("vB.lower", "vB.lower", value = 0.01, min = 0, step=.001)),
                               column(3, offset = 0, numericInput("vB.upper", "vB.upper", value = 0.1, min = 0, step=.001)),
                             ),
                             checkboxInput("vB.fit", "Fit vB (otherwise use vB.start)", value = FALSE),
                             
                             
                           ),
                           # 2TCM selection panel
                           conditionalPanel(
                             condition = "input.button == '2TCM'",
                             fluidRow(
                               column(3, offset = 0, numericInput("K1.start", "K1.start", value = 0.1,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("K1.lower", "K1.lower", value = 0.0001,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("K1.upper", "K1.upper", value = 0.5,min = 0, step=.001)),
                             ),
                             fluidRow(
                               column(3, offset = 0, numericInput("k2.start", "k2.start", value = 0.1,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k2.lower", "k2.lower", value = 0.0001,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k2.upper", "k2.upper", value = 0.5,min = 0, step=.001)),
                             ),
                             fluidRow(
                               column(3, offset = 0, numericInput("k3.start", "k3.start", value = 0.1,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k3.lower", "k3.lower", value = 0.0001,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k3.upper", "k3.upper", value = 0.5,min = 0, step=.001)),
                             ),
                             fluidRow(
                               column(3, offset = 0, numericInput("k4.start", "k4.start", value = 0.1,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k4.lower", "k4.lower", value = 0.0001,min = 0, step=.0001)),
                               column(3, offset = 0, numericInput("k4.upper", "k4.upper", value = 0.5,min = 0, step=.001)),
                             ),
                             fluidRow(
                               column(3, offset = 0, numericInput("vB.start", "vB.start", value = 0.05, min = 0, step=.001)),
                               column(3, offset = 0, numericInput("vB.lower", "vB.lower", value = 0.01, min = 0, step=.001)),
                               column(3, offset = 0, numericInput("vB.upper", "vB.upper", value = 0.1, min = 0, step=.001)),
                             ),
                             checkboxInput("vB.fit", "Fit vB (otherwise use vB.start)", value = FALSE),
                             checkboxInput("irreversible_trapping", "Use irreversible trapping (k4 start, lower and upper set to 0)")
                           ),
                           # Logan selection panel
                           conditionalPanel(
                             condition = "input.button == 'Logan'",
                             numericInput("tstarIncludedFrames", "tstar", value = 10),
                             checkboxInput("vB.fit", "Fit vB", value = FALSE)
                           ),
                           
                           # t* finder
                           conditionalPanel(
                             condition = "input.button == 't* finder'",
                             textInput(
                               inputId = "low.binding", label = "Low-binding region", value = ""),
                             textInput(
                               inputId = "medium.binding", label = "Medium-binding region", value = ""),
                             textInput(
                               inputId = "high.binding", label = "High-binding region", value = ""),
                             p(glue("Please note that by inputting the regions above the Constituent Regions will be ignored."))
                           )
                  ),
                  tabPanel("Results",),
                  tabPanel("Download",)
      )
    )
  )
)

# Define server logic for config file creation ----
server <- function(input, output, session) {
  
  # Reactive model based logic ----
  model_parameters <- observe(
  
  if(input$button == "1TCM"){
    # Check for required fields
    
    
  }
  else if(input$button == "2TCM"){
    observeEvent(input$irreversible_trapping,
                 {
                   if(input$irreversible_trapping){
                     updateNumericInput(session, inputId = "k4.start", value = 0, min = 0, max = 0, step = 0)
                     updateNumericInput(session, inputId = "k4.lower", value = 0, min = 0, max = 0, step = 0)
                     updateNumericInput(session, inputId = "k4.upper", value = 0, min = 0, max = 0, step = 0)
                   } else {
                     updateNumericInput(session, inputId = "k4.start", value = 0.1, min = 0, max = 0, step = 0.001)
                     updateNumericInput(session, inputId = "k4.lower", value = 0.0001, min = 0, max = 0, step = 0.0001)
                     updateNumericInput(session, inputId = "k4.upper", value = 0.5, min = 0, max = 0, step = 0.001)
                   }
                 })
  }
  else if(input$button == "Logan"){
    
  }
  else if(input$button == "t* finder"){
    
  }
  else if(input$button == "Fit Delay"){
    
  })
  

}

# Run the application ----
shinyApp(ui = ui, server = server)
