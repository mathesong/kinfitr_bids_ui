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

      h2("Modelling Choices"),
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
                  tabPanel("1TCM",
                           # br(),
                           h4("One-Tissue Compartment Model"),
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
                           
                           selectInput(inputId = "bpr_model",
                                       label = "BPR model",
                                       choices=c("Interpolation",
                                                 "Fit Individually: Constant",
                                                 "Fit Individually: Linear",
                                                 "Fit Individually: GAM",
                                                 "Fit Hierarchically: HGAM"),
                                       selected = "Interpolation",
                                       multiple = FALSE),
                           br(),
                           h4("Time subsetting"),
                           div(style="display:inline-block",textInput(inputId="bpr_starttime", label="from (min)", value = 0)),
                           div(style="display:inline-block",textInput(inputId="bpr_endtime", label="to (min)", value = Inf)),
                           br(),
                           h4("Additional Modelling Options"),
                           # p("GAM models may sometimes require k to be reduced, ",
                           #      "and HGAM models require additional model specifications."),
                           textInput(inputId = "bpr_k",
                                     label = "GAM dimension of the basis (k)",
                                     value = "6"),
                           p(div(HTML("<em>This value must sometimes be reduced when there are too few data points, or increased for extra wiggliness.</em>")),
                             style = "font-size:12px;"
                           ),
                           #br(),
                           #h4("Additional HGAM Modelling Options"),
                           #p(glue("If using a hierarchical modelling approach ",
                           #      "these models require some additional model ",
                           #      "specifications. Please define as appropriate.")),
                           textInput(inputId = "bpr_hgam_opt",
                                     label = "HGAM Smooth Formula",
                                     value = ""),
                           p(div(HTML("<em>Use any of the subsetting attributes, as well as measurement (pet), e.g. s(time, k=8) + s(time, pet, bs='fs', k=5)</em>")),
                             #style = "font-size:12px;"
                           )),

                  tabPanel("2TCM",

                           # br(),
                           h4("Two-Tissue Compartmental Model (2TCM)"),
                           p(glue("There are many options available for modelling the parent ",
                                  "fraction. For most tracers, a good default option is the ",
                                  "`Fit Individually: Choose the best-fitting model` option, ",
                                  "which will choose the model which fits best on average, and ",
                                  "applies that model to all of the data. ",
                                  "Hierarchical models (more to come) are best left for experienced users.  "),
                             #style = "font-size:14px;"
                             ),

                           selectInput(inputId = "pf_model",
                                       label = "Parent fraction model",
                                       choices=c("Interpolation",
                                                 "Fit Individually: Choose the best-fitting model",
                                                 "Fit Individually: Hill",
                                                 "Fit Individually: Exponential",
                                                 "Fit Individually: Power",
                                                 "Fit Individually: Sigmoid",
                                                 "Fit Individually: Inverse Gamma",
                                                 "Fit Individually: Gamma",
                                                 "Fit Individually: GAM",
                                                 "Fit Hierarchically: HGAM"),
                                                 #"Fit Hierarchically: NLME Hill",
                                                 #"Fit Hierarchically: NLME Exponential",
                                                 #"Fit Hierarchically: NLME Power",
                                                 #"Fit Hierarchically: NLME Sigmoid",
                                                 #"Fit Hierarchically: NLME Inverse Gamma",
                                                 #"Fit Hierarchically: NLME Gamma"
                                                 #),
                                       selected = "Interpolation", multiple = FALSE),
                           checkboxInput(inputId = "pf_set_t0",
                                         label = "Set to 100% at time 0",
                                         value = TRUE),
                           #br(),
                           h4("Time subsetting"),
                           div(style="display:inline-block",textInput(inputId="pf_starttime", label="from (min)", value = 0)),
                           div(style="display:inline-block",textInput(inputId="pf_endtime", label="to (min)", value = Inf)),
                           br(),
                           h4("Additional Modelling Options"),
                           # p(glue("If using a hierarchical modelling approach ",
                           #        "these models require some additional model ",
                           #        "specifications. Please select as appropriate."),
                             #style = "font-size:14px;"
                             #),
                           # textInput(inputId = "pf_nlme_opt",
                           #             label = "NLME Random Effects",
                           #             value = ""),
                           # p(div(HTML("<em>e.g. a + b + c</em>")),
                           #   #style = "font-size:12px;"
                           #   ),
                           #br(),
                           textInput(inputId = "pf_k",
                                     label = "GAM dimension of the basis (k)",
                                     value = "6"),
                           p(div(HTML("<em>This value must sometimes be reduced when there are too few data points, or increased for extra wiggliness.</em>")),
                             style = "font-size:12px;"
                           ),
                           textInput(inputId = "pf_hgam_opt",
                                     label = "HGAM Smooth Formula",
                                     value = ""),
                           p(div(HTML("<em>Use any of the subsetting attributes, as well as measurement (pet), e.g. s(time, k=8) + s(time, pet, bs='fs', k=5)</em>")),
                             #style = "font-size:12px;"
                             )
                           ),

                  tabPanel("Logan",
                           # br(),
                           h4("Logan Graphical Model"),
                           p(glue("Models for the whole blood don't tend to make much difference. ",
                                  "They are mostly useful when the blood measurements are very noisy, ",
                                  "and when brain uptake is so low that the blood makes a big impact."),
                             #style = "font-size:14px;"
                             ),

                           selectInput(inputId = "wb_model",
                                       label = "Whole Blood model",
                                       choices=c("Interpolation",
                                                 "Fit Individually: Splines"),
                                       selected = "Interpolation",
                                       multiple = FALSE),
                           # br(),
                           checkboxInput(inputId = "wb_dispcor",
                                         label = "Perform dispersion correction on autosampler samples?",
                                         value = FALSE),
                           h4("Time subsetting"),
                           div(style="display:inline-block",textInput(inputId="wb_starttime", label="from (min)", value = 0)),
                           div(style="display:inline-block",textInput(inputId="wb_endtime", label="to (min)", value = Inf)),
                           br(),
                           h4("Additional Spline Modelling Options"),
                           p(glue("Depending on the number of samples, some of the ",
                                  "k values may need to be reduced from their default ",
                                  "of 10")),
                           div(style="display:inline-block",textInput(inputId="wb_kb",   label="k before the peak", value = "")),
                           div(style="display:inline-block",textInput(inputId="wb_ka_a", label="k after the peak (auto)", value = "")),
                           div(style="display:inline-block",textInput(inputId="wb_ka_m", label="k after the peak (manual)", value = ""))),
                  tabPanel("Download",
                           # actionButton(inputId = "update",
                           #              label = "Update"),
                           downloadButton('downloadData', 'Download customised config file'),
                           verbatimTextOutput("json_text")
                           ),
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
