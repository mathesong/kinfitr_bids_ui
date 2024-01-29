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


# Define UI for bloodstream app ----
shinyUI(
  fluidPage(theme = shinytheme("flatly"),
                
                # App title ----
                titlePanel("Create a customised kinfitr BIDS App config file"),
                
                # Sidebar layout for BIDS datset subsetting ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    h2("Select BIDS Dataset"),
                    shinyDirButton("bidsdir", 
                                   label = "Browse to choose a BIDS folder",
                                   title = "Choose a folder",
                                   buttonType = "primary",
                                   multiple = FALSE,
                                   class = "btn-block"),
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
                    ComboBox.shinyInput(inputId = "region_regions", label = "Constituent Regions (e.g. L_HIP,R_HIP)", value = "", multiSelectDelimiter = ",",allowFreeform = TRUE),
                    ComboBox.shinyInput(inputId = "region_seg", label = "Description (desc) of tacs file (e.g. gtmseg)", value = "",allowFreeform = FALSE),
                    ComboBox.shinyInput(inputId = "region_deriv", label = "Derivative folder grob (e.g. petprep)", value = "", allowFreeform = FALSE),
                    br(),
                    
                  ),
                  
                  # Main panel for modelling options ----
                  mainPanel(
                    
                    h2("Modelling Selections"),
                    p(glue("Select the modelling approach to fit the Time Activity Curves",
                           " (TACs). You can select among different methods, either ",
                           "compartmental models (e.g., 1TCM, 2TCM), and graphical models ",
                           "(e.g., Logan) to fit your data. Data to be fitted are exepcted to",
                           " be derived from a BIDS folder."),
                      h6("You can fit up to three models on your regional data."),
                      style = "font-size:14px;"
                    ),
                    br(),
                    
                    # Tabset
                    tabsetPanel(type = "tabs",
                                tabPanel("Model 1",
                                         # br(),
                                         h4(""),
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
                                           condition = "input[['button']] == '1TCM'",
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
                                           condition = "input[['button']] == '2TCM'",
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
                                           condition = "input[['button']] == 'Logan'",
                                           numericInput("tstarIncludedFrames", "tstar", value = 10),
                                           checkboxInput("vB.fit", "Fit vB", value = FALSE)
                                         ),
                                         
                                         # t* finder
                                         conditionalPanel(
                                           condition = "input[['button']] == 't* finder'",
                                           ComboBox.shinyInput(
                                             inputId = "low.binding", label = "Low-binding region", value = ""),
                                           ComboBox.shinyInput(
                                             inputId = "medium.binding", label = "Medium-binding region", value = ""),
                                           ComboBox.shinyInput(
                                             inputId = "high.binding", label = "High-binding region", value = "")
                                         )
                                ),
                                tabPanel("Model 2",),
                                tabPanel("Model 3",),
                                tabPanel("Download",
                                         br(),
                                         h3("Download kinfitr config files"),
                                         # Only one download button! 
                                         fluidRow(
                                           column(3, downloadButton("model1_config", "Download Model 1 config")),
                                           column(3, downloadButton("model2_config", "Download Model 2 config")),
                                           column(3, downloadButton("model3_config", "Download Model 3 config"))
                                         )
                                         )
                                )
                    )
                  )
            )
  )