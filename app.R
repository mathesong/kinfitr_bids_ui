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

      # p("Once finished defining how each part of the data should be modelled, ",
      #   "you can download ",
      #   "the resulting config file by clicking the button below."),
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
      h2("Data Subset"),
      p(glue("Use these options to apply this config to a subset of the data. ",
             "Values should be separated by semi-colons. ",
             "All measurements fulfilling all the conditions will ",
             "be included. Leave options blank for no subsetting is desired, ",
             "i.e. leaving sub blank implies that all subjects should ",
             "be included."),
        style = "font-size:14px;"
        ),
      #br(),
      textInput(inputId = "subset_sub", label = "sub", value = ""),
      textInput(inputId = "subset_ses", label = "ses", value = ""),
      textInput(inputId = "subset_rec", label = "rec", value = ""),
      textInput(inputId = "subset_tracer", label = "TracerName", value = ""),

      

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
                           p(glue("There are not so many common models for the BPR. ",
                                  "When the BPR is clearly constant or linear, use ",
                                  "the relevant option. ",
                                  "For most tracers, with a more complex function, ",
                                  "a good default option is the ",
                                  "`Fit Individually: GAM` option, ",
                                  "which will fit a smooth generalised additive model ",
                                  "to each curve independently. ",
                                  "Hierarchical models are best left for experienced users.  "),
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
                               column(3, offset = 0, numericInput("k4.lower", "k4.lower", value = 0.0001,min = 0, step=.001)),
                               column(3, offset = 0, numericInput("k4.upper", "k4.upper", value = 0.5,min = 0, step=.001)),
                             ),
                             fluidRow(
                               column(3, offset = 0, numericInput("vB.start", "vB.start", value = 0.05, min = 0, step=.001)),
                               column(3, offset = 0, numericInput("vB.lower", "vB.lower", value = 0.01, min = 0, step=.001)),
                               column(3, offset = 0, numericInput("vB.upper", "vB.upper", value = 0.1, min = 0, step=.001)),
                             ),
                             checkboxInput("vB.fit", "Fit vB (otherwise use vB.start)", value = FALSE)
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
                               inputId = "binding_regions", label = "Low-, medium- and high-binding regions", value = ""),
                           )
                  ),
                  tabPanel("Download",)
      )
    )
  )
)

# Define server logic for config file creation ----
server <- function(input, output) {

  # Reactive expression to generate the config file ----

  config_json <- reactive({

    Subsets <- list(
      sub = input$subset_sub,
      ses = input$subset_ses,
      rec = input$subset_rec,
      task = input$subset_task,
      run = input$subset_run,
      TracerName = input$subset_tracer,
      ModeOfAdministration = input$subset_modeadmin,
      InstitutionName = input$subset_institute,
      PharmaceuticalName = input$subset_pharmaceutical
    )

    ParentFraction <- list(
      Method = input$pf_model,
      set_ppf0 = input$pf_set_t0,
      starttime = as.numeric(input$pf_starttime),
      endtime  = as.numeric(input$pf_endtime),
      #nlme_re = input$pf_nlme_opt,
      gam_k = input$pf_k,
      hgam_formula = input$pf_hgam_opt
    )

    BPR <- list(
      Method = input$bpr_model,
      starttime = as.numeric(input$bpr_starttime),
      endtime  = as.numeric(input$bpr_endtime),
      gam_k = as.numeric(input$bpr_k),
      hgam_formula = input$bpr_hgam_opt
    )

    AIF <- list(
      Method = input$aif_model,
      starttime = as.numeric(input$aif_starttime),
      endtime  = as.numeric(input$aif_endtime),
      expdecay_props = as.numeric(c(input$aif_expdecay_1,
                         input$aif_expdecay_2)),
      inftime = as.numeric(str_split(input$aif_inftime, pattern = ";")[[1]]),
      spline_kb = input$aif_kb,
      spline_ka_m = input$aif_ka_m,
      spline_ka_a = input$aif_ka_a
    )

    WholeBlood <- list(
      Method = input$wb_model,
      dispcor = input$wb_dispcor,
      starttime = as.numeric(input$wb_starttime),
      endtime  = as.numeric(input$wb_endtime),
      spline_kb = input$wb_kb,
      spline_ka_m = input$wb_ka_m,
      spline_ka_a = input$wb_ka_a
    )

    config_list <- list(
      Subsets = Subsets,
      Model = list(
        ParentFraction = ParentFraction,
        BPR = BPR,
        AIF = AIF,
        WholeBlood = WholeBlood
      )
    )

    jsonlite::toJSON(config_list, pretty=T)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      generate_new_filename()
    },
    content = function(con) {
      writeLines(text = config_json(),
                 con = con)
    }
  )

  # config_jsonfile <- reactive({
  #   jsonlite::write_json( x = config_json(),
  #                         path = paste0("output/", out_filename) )
  # })

  output$json_text <- renderText( { config_json() } )

}
# Run the application
shinyApp(ui = ui, server = server)
