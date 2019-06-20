#####################################  HEADER  ################################
# SCRIPTNAME: app.R
# DESCRIPTION: Master file for BRC Water Quality Data Management Shiny App
# WRITTEN BY: Dan Crocker
# DATE OF LAST UPDATE: Spring 2019
# Credits: Code inspired by Dean Attali -
#          https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
##############################################################################.

### TO DO ####
# Login credentials?
# dashboard panel?
### Add login page (password Input- login provides the following:
  # FC- Name - Region - site and volunteer lists should get filtered automatically
  # PC - gets access to all sites and volunteers, can see hidden modules and access to import button
# Should submitted records be in tidy format? or non-tidy - for archive -
# non-tidy format will preserve nulls, whereas tidy format will not
# Process button - needs to perform error checks, reformat, then send email,
# with excel and .rds file attached
# Need an email account (non-personal) for sending emails via R (UN and PW go in config)
# Add comment button for each data entry subsection - for depth make show when not recorded is selected
# Add replicate chemical samples - one comment button for all chemical samples
# Finish modal for adding comment - observe action button submit - save comments to separate csv and rds file
# Split Staged Data Preview so that comments appear to the right
# In other data add a button for attach photo - similar to comment - Add photo name, parameter number, photo credit
#       is selectize input where Field Monitor is default - but can type in other name.
#       Photo is logged and a csvFile is generated
#
### Load Libraries ####
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = F, repos="http://cran.rstudio.com/", quiet = T, verbose = F)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("shiny","shinyjs", "shinyFiles", "shinyTime", "shinyalert","shinydashboard","rmarkdown", "knitr", "tidyselect", "lubridate",
              "plotly", "leaflet", "RColorBrewer", "devtools", "data.table", "DT", "scales", "stringr", "shinythemes", "ggthemes",
              "dplyr" , "httr", "tibble", "bsplus", "readxl", "miniUI", "rstudioapi", "rdrop2")
ipak(packages)

# loadData()

print(paste0("BRCWQDM App lauched at ", Sys.time()))

# source('all_sessions.R', local = TRUE)
### Set Directories ####
wdir <- getwd()
### Local Data Directory ####
dataDir <<- file.path(paste0(config[1],"Data/"))
app_user <<- config[2]
user_zone <<- config[13]
### CSV Files ####
stagedDataCSV <<- paste0(dataDir,"StagedData/BRC_StagedData.csv")
stagedCommentsCSV <<- paste0(dataDir,"StagedData/BRC_StagedComments.csv")
submittedDataDir <<- paste0(dataDir,"SubmittedData/")

### RDS FILES ####
rdsFiles <- paste0(dataDir,"rdsFiles/")
stagedDataRDS <- paste0(rdsFiles,"stagedData.rds")
stagedCommentsRDS <- paste0(rdsFiles,"stagedComments.rds")
submitteddDataRDS <- paste0(rdsFiles,"submittedData.rds")
submittedCommentsRDS <- paste0(rdsFiles,"submittedComments.rds")

### RDS DATABASE FILES ####
### These live on GH and can be updated periodically
remote_data_dir <- paste0(getwd(),"/data/")

sitesRDS <- paste0(remote_data_dir,"sites_db.rds")
peopleRDS <- paste0(remote_data_dir,"people_db.rds")
parametersRDS <- paste0(remote_data_dir,"parameters_db.rds")
assignmentsRDS <- paste0(remote_data_dir,"assignments_db.rds")

### From edit module ####
useShinyalert()

source(paste0(wdir, "/funs/dropB.R"))
### Download rds files cached on dropbox to local data folder and load these and any staged RDS files
LOAD_DB_RDS()
### Change to the last record date (rds file)
last_update <- Sys.Date()

### AS IS fields require no manipulation and can go directly to outputs
### Date and Time and any other QC'd values need to come from reactive elements
# wdir
table_fields <<- readr::read_csv(paste0(wdir,"/data/table_fields.csv"))
data_fields <<- table_fields[1:30,]
comment_fields <<- table_fields[31:35,]

### DATA FIELDS ####
fieldsASIS <<- data_fields$shiny_input[data_fields$as_is == "yes"]
### Data Column names csv ####
col_names <<- data_fields$shiny_input
### Comment Column Names csv ####
comm_col_names <<- comment_fields$shiny_input

### Select Option Choices ####
### All selection dependent lists need to go in server
### These lists are static
sites <- sites_db$BRC_CODE
names(sites) <- paste0(sites_db$SITE_NAME, " - ", sites_db$BRC_CODE)
sites <<- sites[order(names(sites))]
samplers <<- sort(unique(assignments_db$NAME[assignments_db$YEAR == year(Sys.Date())]))

wea_choices <<- c("","Storm (heavy rain)", "Rain (steady rain)",
                 "Showers (intermittent rain)", "Overcast","Clear/Sunny", "Other","Not Recorded")
wat_appear_choices <<- c("","Clear", "Milky", "Foamy", "Oily Sheen",
                        "Dark Brown", "Greenish", "Orange", "Tea Color", "Other", "Not Recorded")
wat_trash_choices <<- c("","None", "Light", "Medium", "Heavy", "Not Recorded")
wat_odor_choices <<-c("","None", "Sewage", "Fishy", "Chlorine",
                     "Rotten Eggs", "Other", "Not Recorded")
wat_NAV_choices <<- c("","None", "Light", "Medium", "Heavy", "Not Recorded")
wat_clarity_choices <<- c("","Clear","Slightly Hazy","Cloudy","Opaque", "Not Recorded")
wat_erosion_choices <<- c("","Undercut bank", "Slumping", "Erosional gullies in bank",
                         "Bridge or building undermining", "No erosion", "Not Recorded")
depth_choices <<- c("","Gage (Staff Plate-feet)", "Ruler (inches)", "Not Recorded", "No Datum")

### SOURCE EXTERNAL SCRIPTS ####
source(paste0(wdir, "/funs/csv2df.R"))
source(paste0(wdir, "/funs/editableDT_modFuns.R"))
source(paste0(wdir, "/mods/mod_editDT.R"))

### CSS ####
appCSS <-   ".mandatory_star { color: red; }
              #error { color: red; }"

fieldsMandatory <- c("site", "sampler" )

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
actionCount <- reactiveVal(0)
### End Global Scope ####
### NOTES FOR UI AND SERVER ####
    # User enters records in form
    # When record complete --> Save Record --> saves csv --> displays in DT (Add to new tab)
    # When user saves new record - overwrite csv file in staging --> updates DT
    # The save button is not active unless data is edited in DT
    # When all records have been added user presses enter records
    # The submit button executes a process function which does all calculations
    # A preview table shows up in a new tab, which cannot be edited
    # The final csv file is moved from staged to processed folder (can be edited and reprocessed if needed)


### UI ####
ui <-tagList(
  ### Creates padding at top for navBar space due to "fixed-top" position
  tags$style(type='text/css',
             'body {padding-top: 70px;}',
             'h2 {
               font-family: "Arial Black";
               font-weight: 500;
               line-height: 1.1;
               color: #0C4B91;
             }'
    ),

    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),

  ### Create the Top Navigation Bar as well as define aesthetics
  navbarPage("Blackstone River Coalition Water Quality Data Management", position = "fixed-top",
             inverse = FALSE, collapsible = TRUE,
             theme = shinytheme("flatly"), windowTitle = "BRCWQM",
             footer = tagList(hr(),
               column(4,strong(paste("Data last updated:", last_update)),br()),
               column(8,tags$div(tags$em("Created by Dan Crocker"), align = "right"), br())
  ),
  ### DATA ENTRY TAB  ####
  tabPanel("DATA ENTRY",
        fluidRow(
           column(2, imageOutput("brc_logo1", height = 80), align = "left"),
           column(8, h2("Enter Monitoring Data and Lab Results Here", align = "center")),
           column(2, imageOutput("zap_logo1", height = 80), align = "right")
         ),
          h4("After each sampling event is added click the 'Enter Record' button. Any
              records that have been entered will be staged and available in future sessions,
              or they may be processed, reviewed, and submitted on the other tabs.
              Data may be corrected in the 'Staged Data' tab or in the staged 'csv'file ", align = "center"),
    # "Blackstone River Coalition Water Quality Management"),
    # downloadButton("downloadBtn", "Save records"),
    div(id = "form",
      bs_accordion(id = "sample_inputs") %>%
        bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
        bs_append(title = "SAMPLE EVENT INFO", content =
                wellPanel(fluidRow(
                  column(width = 12,
                    fluidRow(
                      column(width = 4,
                        selectInput("site", labelMandatory("Choose Sample Location:"), c("",sites), selected = "")
                      ),
                      column(width = 3,
                             selectInput("sampler", labelMandatory("Choose Sampler(s):"), multiple = T, choices = c("",samplers), selected = ""),
                             verbatimTextOutput("samplers_rx")
                             ),
                      column(width = 2,
                             dateInput("date", labelMandatory("Sample Date:"))
                      ),
                      column(width = 3,
                             timeInput("time", labelMandatory("Sample Starting Time (24-hr format):"), seconds = FALSE)
                      )
                    )
                  ) #End col
                )) # End Well Panel and FR
        ) %>%
        bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
        bs_append(title = "PHYSICAL PARAMETERS", content =
                wellPanel(fluidRow(
                  column(width = 12,
                    fluidRow(
                      column(width = 3,
                          selectInput("wea48", "Weather Last 48 Hours (P01):",
                                         choices = wea_choices, multiple = T, selected = ""),
                          textAreaInput("comm_P01", labelMandatory("Comments for Weather Last 48 Hours (P01):"), placeholder = "Describe 'other'"),
                          selectInput("wea_now", "Weather at time of sample (P02):",
                                      choices = wea_choices, multiple = T , selected = ""),
                          textAreaInput("comm_P02", labelMandatory("Comments for Weather at time of Sample (P02):"), placeholder = "Describe 'other'"),
                          numericInput("temp_air","Ending Air Temperature (C) (P03):",
                                       value = NULL, min = -20, max = 40, step = 0.5),
                          numericInput("temp_wat", "Ending Water Temperature (C) (P04):",
                                       value = NULL, min = 0, max = 30, step = 0.5)
                        ),
                        column(width = 3,
                          selectInput("wat_trash", "Presence of Trash (P06.A):", choices = wat_trash_choices, selected = ""),
                          textAreaInput("trash_descr", "Desription of Trash (P06.B):"),
                          selectInput("erosion", "Stream bank/infrastructure erosion (P07.A):", choices = wat_erosion_choices, multiple = T, selected = ""),
                          textAreaInput("comm_P07", labelMandatory("Comments for Erosion (P07.A):"), placeholder = "Describe 'other'"),
                          textAreaInput("erosion_change", "Changes to erosion from last month (P07.B):")
                        ),
                        column(width = 3,
                          selectInput("wat_appear", "Water Appearance (P05):", choices = wat_appear_choices, multiple = T, selected = ""),
                          textAreaInput("comm_P05", labelMandatory("Comments for Water Appearance (P05):"), placeholder = "Describe 'other'"),
                          selectInput("wat_odor", "Water Odor (P08):", choices = wat_odor_choices, multiple = T, selected = ""),
                          textAreaInput("comm_P08", labelMandatory("Comments for Water Odor (P08):"), placeholder = "Describe 'other'"),
                          selectInput("wat_nav", "Nuisance Aquatic Vegetation (NAV) (P09.A):", choices = wat_NAV_choices, multiple = F, selected = ""),
                          textAreaInput("wat_nav_descr", "NAV Comments (P09.B):")

                        ),
                        column(width = 3,
                          selectInput("wat_clarity", "Water Clarity (P10)", choices = wat_clarity_choices, selected = ""),
                          numericInput("lab_turb","Lab Turbidity (NTU) (P11.A):", value = NULL, min = 0, max = 2500, step = 0.5),
                          numericInput("lab_turb_rep","Lab Turbidity Replicate (NTU) (P11.B):", value = NULL, min = 0, max = 2500, step = 0.5)
                        )
                    )
                  ) #End col
                )) # End Well Panel and FR
        ) %>%
        bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
        bs_append(title = "DEPTH PARAMETERS", content =
                wellPanel(fluidRow(
                  column(width = 12,
                    fluidRow(
                      column(width = 4,
                        selectInput("depth_type", "Type of depth measurement (D01):", choices = depth_choices)),
                      column(width = 4,
                        uiOutput("depth")
                      )
                    )
                  ) #End col
                )) # End Well Panel and FR
        ) %>%
        bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
        bs_append(title = "CHEMICAL PARAMETERS", content =
                    wellPanel(fluidRow(
                      column(width = 12,
                        fluidRow(
                          column(width = 3,
                            numericInput("do","Dissolved Oxyen (mg/L) (C01):", value = NULL, min = 0, max = 25),
                            numericInput("o2","Oxygen Saturation (%) (C02):", value = NULL, min = 0, max = 100, step = 1)
                            ),
                          column(width = 3,
                             numericInput("no3","Nitrate (mg/L)(C03.A):", value = NULL, min = 0, max = 20),
                             numericInput("no3_rep","Nitrate Replicate (mg/L)(C03.B):", value = NULL, min = 0, max = 20)
                          ),
                          column(width = 3,
                             numericInput("po4","Orthophosphate (mg/L)(C04.A):", value = NULL, min = 0, max = 2),
                             numericInput("po4_rep","Orthophosphate Replicate (mg/L)(C04.B):", value = NULL, min = 0, max = 2)

                          ),
                          column(width = 3,
                             numericInput("conduct", "Specific Conductivity (uS/cm)(C05.A):", value = NULL, min = 0, max = 10000, step = 1),
                             numericInput("conduct_rep", "Specific Conductivity Replicate (uS/cm)(C05.B):", value = NULL, min = 0, max = 10000, step = 1)
                          ))
                      )
                    )) # End Well Panel and FR
        ) %>%
        bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
        bs_append(title = "OTHER SAMPLE INFORMATION", content =
                    wellPanel(fluidRow(
                      column(width = 4,
                             actionButton("add_comment", "Add comment")
                      ),
                      column(width = 4,
                             checkboxInput("photos","Photos associated with sampling event?")
                      ),
                      column(width = 4,
                             actionButton("add_photo", "Add Photo")
                      )
                    )
                ) # End Well Panel
        ),
        tags$head(tags$script(src = "message-handler.js")),
        actionButton("enter", "Enter Record", class = "btn-primary"),
        shinyjs::hidden(
          span(id = "enter_msg", "Adding record entry ..."),
          div(id = "error",
                div(br(), tags$b("Error: "), span(id = "error_msg"))
            )
          )
        ), # End div
        shinyjs::hidden(
          div(
            id = "thankyou_msg",
            h3("Thanks, your record was entered successfully!"),
            actionLink("enter_another", "Enter another record")
          )# End div
        )
    ), # End TabPanel
  ### STAGED DATA TAB ####
  tabPanel("STAGED DATA",
           fluidRow(
           column(2, imageOutput("brc_logo2", height = 80), align = "left"),
           column(8, h2("Data Ready to Process & Submit", align = "center")),
           column(2, imageOutput("zap_logo2", height = 80), align = "right")
         ),
         tabsetPanel(
           tabPanel("STAGED DATA", type = "pills",
                  fluidRow(
                    column(6,
                           ### This is to adjust the width of pop up "showmodal()" for DT modify table
                           tags$head(tags$style(HTML('

                                              .modal-lg {
                                              width: 1200px;
                                              }
                                              '))),
                           helpText("Note: Remember to save any edits/deletions!"),
                           # br(),
                           ### tags$head() is to customize the download button
                           tags$head(tags$style(".butt{background-color:#222f5b;} .butt{color: #e6ebef;}")),
                           useShinyalert(),
                           actionButton(inputId = "SaveStagedData",label = "Save", width = "245px", class="butt"),
                           editableDTUI("stagedDataDT")
                    ),
                    column(6,
                           verbatimTextOutput("rec_comments")
                    )
                  )
                ), # END DATA tp
                tabPanel("STAGED COMMENTS", type = "pills",
                  fluidRow(
                    column(6,
                           ### This is to adjust the width of pop up "showmodal()" for DT modify table
                           tags$head(tags$style(HTML('

                                              .modal-lg {
                                              width: 1200px;
                                              }
                                              '))),
                           helpText("Note: Remember to save any edits/deletions!"),
                           # br(),
                           ### tags$head() is to customize the download button
                           tags$head(tags$style(".butt{background-color:#222f5b;} .butt{color: #e6ebef;}")),
                           useShinyalert(),
                           actionButton(inputId = "SaveStagedComments",label = "Save", width = "245px", class="butt"),
                           # downloadButton("Trich_csv", "Download in CSV", class="butt"),
                           # Set up shinyalert
                           editableDTUI("stagedCommentsDT")
                    )
                  )
                ), # End tp
                tabPanel("PROCESS & SUBMIT", type = "pills",
                  fluidRow(
                    column(12,
                      h4("Make sure all data has been entered and checked over for accuracy. Click the 'PROCESS' button \n
                            when you are ready to proceed. During processing the data will be checked for errors and anomalies.\n
                            If no problems are found then you may submit the data to the Program Coordinator for final QC and database import.")
                    # actionButton(inputId = "process",label = "PROCESS", width = "200px", class="butt"),
                    # actionButton(inputId = "submit",label = "SUBMIT", width = "200px", class="butt"),
                    )
                  ),
                  fluidRow(
                    column(6,
                    wellPanel(
                        strong(h4("Process staged records:")),
                        br(),
                        uiOutput("process.UI"),
                        br(),
                        h4(textOutput("text.process.status"))
                      )
                    ),
                    column(6,
                      wellPanel(
                        strong(h4("Submit processed staged records to program coordinator:")),
                        br(),
                        uiOutput("submit.UI"),
                        br(),
                        uiOutput("text.submit.status")
                      )
                    )
                  ),
                  fluidRow(
                    column(12,
                           tabsetPanel(
                             tabPanel("Processed Data",
                                      dataTableOutput("table.process.data")
                             ),
                             tabPanel("Processed Comments",
                                      dataTableOutput("table.process.comments")
                             ) # End Tab Panel
                           ) # End Tabset Panel
                    ) # End Col
                  ) # End Fluid row
                ) # End TabPanel
         )
  ),# End Tab Panel
  ### SUBMITTED RECORDS TAB ####
    tabPanel("SUBMITTED DATA",
        fluidRow(
           column(2, imageOutput("brc_logo3", height = 80), align = "left"),
           column(8,  h2("These Records Have Been Submitted", align = "center")),
           column(2, imageOutput("zap_logo3", height = 80), align = "right")
         )
        # fluidRow(
        #   column(12,
        #     # DT_OUT_UI("mod_submittedData")
        #   )
        # )
    ),
  ### MORE TAB ####
  navbarMenu("More",
    tabPanel("ANALYSIS",
        fluidRow(
           column(2, imageOutput("brc_logo4", height = 80), align = "left"),
           column(8,  h2("Data Analysis", align = "center")),
           column(2, imageOutput("zap_logo4", height = 80), align = "right")
         )
    ),
    ### REPORTS TAB ####
    tabPanel("REPORTS",
        fluidRow(
           column(2, imageOutput("brc_logo5", height = 80), align = "left"),
           column(8,  h2("Reports", align = "center")),
           column(2, imageOutput("zap_logo5", height = 80), align = "right")
         )
    ),
    ### INSTRUCTIONS TAB ####
    tabPanel("INSTRUCTIONS",
      fluidRow(column(12,
                  h2("Instructions and Data Processing Workflow", align = "center"),
                  h4("blah.. blah blah...", align = "left"))) ### Perhaps this should be a markdown doc
    )
  ) # End navbar page
)
) # End UI - taglist

####################################################.
###                   SERVER                    ####
####################################################.

server <- function(input, output, session) {
  app_user <- config[2]
  rxdata <- reactiveValues()
  loadData <- function() {
    if(file.exists(stagedDataCSV) == TRUE){
      data <- read.table(stagedDataCSV, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
      df <- data_csv2df(data, data_fields) ### saves RDS file as data.frame
      saveRDS(df, stagedDataRDS)
      rxdata$stagedData <- readRDS(stagedDataRDS)
    } else {
      df <- NULL
      rxdata$stagedData <- NULL
    }
    return(df)
  }

  loadComments <- function() {
    if(file.exists(stagedCommentsCSV) == TRUE){
      data <- read.table(stagedCommentsCSV, stringsAsFactors = FALSE, sep = " " ,header = T)
      df <- comm_csv2df(data, comment_fields) ### saves RDS file as data.frame
      saveRDS(df, stagedCommentsRDS)
      rxdata$stagedComments <- readRDS(stagedCommentsRDS)
    } else {
      df <- NULL
      rxdata$stagedComments <- NULL
    }
    return(df)
  }

  # readRDS(stagedCommentsRDS)
  loadAll <- function(){
    loadData()
    loadComments()
  }
  loadAll()

  ### interactive dataset

  # rxdata$stagedData <- readRDS(stagedDataRDS)
  # rxdata$stagedComments <- readRDS(stagedCommentsRDS)

  # rxdata$submittedData <- try(readRDS(submittedDataRDS))
  # rxdata$submittedComments <- try(readRDS(submittedCommentsRDS))

  DataEditCols <- data_fields$dt_cols[data_fields$editable == "yes"]
  CommentEditCols <- comment_fields$dt_cols[comment_fields$editable == "yes"]

  staged_df <- callModule(editableDT, "stagedDataDT",
                   data = reactive(rxdata$stagedData),
                   rxdata = "stagedData",
                   inputwidth = reactive(170),
                   edit_cols = DataEditCols)

  staged_comments <- callModule(editableDT, "stagedCommentsDT",
                          data = reactive(rxdata$stagedComments),
                          rxdata = "stagedComments",
                          inputwidth = reactive(170),
                          edit_cols = CommentEditCols)

  # submitted_df <- callModule(editableDT, "submittedDataDT",
  #                         data = reactive(rxdata$submittedData),
  #                         rxdata = "submittedData",
  #                         inputwidth = reactive(170),
  #                         edit_cols = DataEditCols)
  #
  # submitted_comments <- callModule(editableDT, "submittedCommentsDT",
  #                               data = reactive(rxdata$submittedComments),
  #                               rxdata = "submittedComments",
  #                               inputwidth = reactive(170),
  #                               edit_cols = CommentEditCols)

### OTHER/NOT RECORDED ACTIONS ####
  shinyjs::hide("comm_P01")
  shinyjs::hide("comm_P02")
  shinyjs::hide("comm_P05")
  shinyjs::hide("comm_P07")
  shinyjs::hide("comm_P08")
### SHOW HIDDEN COMMENTS ####
observeEvent(input$wea48, {
  if("Other" %in% input$wea48){
      shinyjs::show("comm_P01")
      fieldsMandatory <- c(fieldsMandatory,"comm_P01")
  } else {
      shinyjs::hide("comm_P01")
      fieldsMandatory <- fieldsMandatory[fieldsMandatory != "comm_P01"]
  }
})
observeEvent(input$wea48, {
  if("Not Recorded" %in% input$wea48){
      updateSelectInput(session, "wea48",
      selected = "Not Recorded"
    )
  }
})###
observeEvent(input$wea_now, {
  if("Other" %in% input$wea_now){
      shinyjs::show("comm_P02")
      fieldsMandatory <- c(fieldsMandatory,"comm_P02")
  } else {
      shinyjs::hide("comm_P02")
      fieldsMandatory <- fieldsMandatory[fieldsMandatory != "comm_P02"]
  }
})
observeEvent(input$wea_now, {
  if("Not Recorded" %in% input$wea_now){
      updateSelectInput(session, "wea_now",
      selected = "Not Recorded"
    )
  }
})
###
observeEvent(input$wat_appear, {
  if("Other" %in% input$wat_appear){
      shinyjs::show("comm_P05")
      fieldsMandatory <- c(fieldsMandatory,"comm_P05")
  } else {
      shinyjs::hide("comm_P05")
      fieldsMandatory <- fieldsMandatory[fieldsMandatory != "comm_P05"]
  }
})
observeEvent(input$wat_appear, {
  if("Not Recorded" %in% input$wat_appear){
      updateSelectInput(session, "wat_appear",
      selected = "Not Recorded"
    )
  }
})
###
observeEvent(input$erosion, {
  if("Other" %in% input$erosion){
      shinyjs::show("comm_P07")
      fieldsMandatory <- c(fieldsMandatory,"comm_P07")
  } else {
      shinyjs::hide("comm_P07")
      fieldsMandatory <- fieldsMandatory[fieldsMandatory != "comm_P07"]
  }
})
observeEvent(input$erosion, {
  if("Not Recorded" %in% input$erosion){
      updateSelectInput(session, "erosion",
      selected = "Not Recorded"
    )
  }
})
###
observeEvent(input$wat_odor, {
  if("Other" %in% input$wat_odor){
      shinyjs::show("comm_P08")
      fieldsMandatory <- c(fieldsMandatory,"comm_P08")
  } else {
      shinyjs::hide("comm_P08")
    fieldsMandatory <- fieldsMandatory[fieldsMandatory != "comm_P08"]
  }
})
observeEvent(input$wat_odor, {
  if("Not Recorded" %in% input$wat_odor){
      updateSelectInput(session, "wat_odor",
      selected = "Not Recorded"
    )
  }
})
###
observeEvent(input$wat_clarity, {
  if("Not Recorded" %in% input$wat_clarity){
      updateSelectInput(session, "wat_clarity",
      selected = "Not Recorded"
    )
  }
})

### CHECK MANDATORY FIELDS ####
observe({
  mandatoryFilled <- vapply(fieldsMandatory,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != ""
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)

  # enable/disable the enter button
  shinyjs::toggleState(id = "enter", condition = mandatoryFilled)
})
### STAGED DATA CREATION ####
# stagedData <- eventReactive(input$enter,{
#     loadData()
# })

### REACTIVE DATA ENTRY VALS ####
### SAMPLEDATE-TIME CALC ####
SampleDT <- reactive({paste0(strftime(input$date, "%Y-%m-%d"), " ", strftime(input$time, "%H:%M"))})

### CONVERT MULIPLE SELECT INPUTS TO 1 STRING ####
samplersRX <- reactive({
  req(input$sampler)
  paste(input$sampler, collapse = ";") %>% trimws()
  })
wea48RX <- reactive({
  req(input$wea48)
  paste(input$wea48, collapse = ";") %>% trimws()
  })
wea_nowRX <- reactive({
  req(input$wea_now)
  paste(input$wea_now, collapse = ";") %>% trimws()
  })
wat_appearRX <- reactive({
  req(input$wat_appear)
  paste(input$wat_appear, collapse = ";") %>% trimws()
  })
erosionRX <- reactive({
  req(input$erosion)
  paste(input$erosion, collapse = ";") %>% trimws()
  })
wat_odorRX <- reactive({
  req(input$wat_odor)
  paste(input$wat_odor, collapse = ";") %>% trimws()
  })


output$depth <- renderUI({
  req(input$depth_type %in% c("Ruler (inches)", "Gage (Staff Plate-feet)"))
  switch(input$depth_type,
         "Gage (Staff Plate-feet)" = numericInput("depth_meas","Water Depth (feet) (D02):",
                                     value = NULL, min = 0, max = , step = 0.01),
         "Ruler (inches)" = numericInput("depth_meas","Ruler Water Depth (Decimal Inches) (D02):",
                                          value = NULL, min = 0, max = 24, step = 0.1)
  )
})

### FORM DATA ####
formData <- reactive({
  data <- sapply(fieldsASIS, function(x) input[[x]])
  data <- c(data, "SampleDateTime" = SampleDT(), "sampler" = samplersRX(),
            wea48 = wea48RX(), wea_now = wea_nowRX(), wat_appear = wat_appearRX(),
            erosion =  erosionRX(), wat_odor = wat_odorRX(), "Entered_By" = app_user)
  data <- data[col_names] %>% as.character()
  data <- t(data)
  data
})

### OBSERVE ENTER RECORD ####
observeEvent(input$enter, {
  # loadData()
  shinyjs::reset("form")
  shinyjs::hide("form")
  shinyjs::show("thankyou_msg")
})
### SAVE DATA TO CSV ####
saveData <- function(data, csvFile, rdsFile) {
  # csvFile <- stagedDataCSV
 # Strip out any commas
  # data <- str_replace_all(data, pattern = ",", replacement = "..")
  # Set the csvFile using data data, so each data has its own csv.
  # If the name exists then append the records from that csv using rbind and save the fle
  # csvFile <- paste0("BRC_StagedData.csv") #come up with better name - like "BRC_StagedData_Apr2019_R1.csv"
    if(file.exists(csvFile)){
      write.table(x = data, file = csvFile,
              row.names = FALSE, quote = TRUE, na = "", append = TRUE,
              col.names = FALSE, qmethod = "d")
    } else {
      write.table(x = data, file = csvFile,
              row.names = FALSE, col.names = col_names, na = "", quote = TRUE,
              qmethod = "d", append = FALSE,)
    }
  # dt <- read.table(csvFile, stringsAsFactors = TRUE, header = T, sep = " ")
  # saveRDS(data.table(dt), rdsFile)
  loadAll()
  rxdata$stagedData <- readRDS(stagedDataRDS)
  }

observeEvent(input$enter_another, {
  shinyjs::show("form")
  shinyjs::hide("thankyou_msg")
})

### ENTER RECORD EVENT ####
observeEvent(input$enter, {
  shinyjs::disable("enter")
  shinyjs::show("enter_msg")
  shinyjs::hide("error")

  tryCatch({
    saveData(formData(),  csvFile = stagedDataCSV, rdsFile = stagedDataRDS)
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  },
  error = function(err) {
    shinyjs::html("error_msg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::enable("enter")
    shinyjs::hide("enter_msg")
  })
})
# action to take when enter button is pressed
# observeEvent(input$enter, {
#   saveData(formData())
# })
# tableData <- if(input$enter == 0){
#   tableData <- loadData()
#   tableData <- data.frame(tableData, stringsAsFactors = F)
#   tableData
# } else {
#   tableData <- stagedData()
#   tableData <- data.frame(tableData, stringsAsFactors = F)
#   tableData
# }


### DT OUTPUT ####
# output$stagingTable <- renderDT({
#   # req(tableData()),
#   dt <- datatable(tableData(), editable = F, rownames = FALSE,
#                   colnames = dt_names, selection = "single",
#   options = list(searching = FALSE, lengthChange = FALSE))
#   # server = FALSE #%>%
#     # formatDate("SampleDateTime", method = "toLocaleString")
#    }

###

### COMMENT MODAL ####
comment_par_choices <<- c("General Comment", data_fields$dt_cols[data_fields$take_comments =="yes"])

   observeEvent(input$add_comment, {
     # req(input$site, SampleDT())
     if(input$site == ""| is.null(SampleDT())){
       shinyalert("Oops!", "A site and Date must be selected to add a comment!.", type = "error")
     } else {
      showModal(modalDialog(
        h3("ADD COMMENT..."),
        "Add comment related to specific parameter or a general comment that applies to this sampling event.
           You will be listed as the commenter, unless the box is checked to indicate that the comment is from the Field Monitor",
        selectInput("comment_par", "Parameter Reference:", comment_par_choices),
        textAreaInput("comment_text", label = NULL, placeholder = "Add comment here...", width = "100%"),
        checkboxInput("FM_comment", "This is a comment from Field Monitor", value = F),
        actionButton("submit_comment", "Submit Comment"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel")
      )
      ))
     }
   })
  ### save to RDS and CSV ####
  observeEvent(input$SaveStagedData,{
    csv <- staged_df()
    names(csv) <- data_fields$shiny_input
    #### overwrite table as well ... write.table
    write.table(x = csv, file = stagedDataCSV,
                row.names = FALSE, col.names = TRUE, quote = TRUE,
                qmethod = "d", append = FALSE)
    # saveRDS(object = staged_df(), stagedDataRDS)
    loadData()
    shinyalert(title = "Saved!", type = "success")
  })

   ### save to RDS and CSV ####
   observeEvent(input$SaveStagedComments,{
     csv <- staged_comments()
     names(csv) <- comment_fields$shiny_input
     #### overwrite table as well ... write.table
     write.table(x = csv, file = stagedCommentsCSV,
                 row.names = FALSE, col.names = TRUE, quote = TRUE,
                 qmethod = "d", append = FALSE)
     # saveRDS(object = staged_(), stagedDataRDS)
     loadComments()
     shinyalert(title = "Saved!", type = "success")
   })

  saveComment <- function(data,csvFile,rdsFile) {
         if(file.exists(file.path(csvFile))){
           write.table(x = data, file = csvFile,
                       row.names = FALSE, quote = TRUE, append = TRUE,
                       col.names = FALSE, qmethod = "d")
         } else {
           write.table(x = data, file = csvFile,
                       row.names = FALSE, col.names = comm_col_names, quote = TRUE,
                       qmethod = "d", append = FALSE)
         }
         dt <- read.table(csvFile, stringsAsFactors = TRUE, header = T, sep = " ")
         loadAll()
         rxdata$stagedComments <- readRDS(rdsFile)
  }

formatComment <- function(){
  if(input$FM_comment == TRUE){
    commenter <- input$sampler
  } else {
    commenter <- app_user
  }
  comment <- tibble(SITE = input$site, DATE = input$date, PARAMETER = input$comment_par,
                    COMMENTER = commenter, COMMENT_TEXT = input$comment_text)
  # glimpse(comment)
  return(comment)
}

### Submit Comment ####
  # Need to append comment to another csv file of staged comments
  # Also hold records in rds file
  # Need ability to edit comments?
observeEvent(input$submit_comment,{
  saveComment(data = formatComment(), csvFile = stagedCommentsCSV, rdsFile = stagedCommentsRDS)
      session$sendCustomMessage(
        message = 'Comment Submitted!')
  removeModal()
})


# observeEvent(input$save_dt, {
#   write.table(x = tableData(), file = paste0(stagingDir,"/BRC_StagedData.csv"),
#               row.names = FALSE, col.names = col_names, quote = TRUE,
#               qmethod = "d", append = FALSE)
#     session$sendCustomMessage(type = 'testmessage',
#       message = 'Thank you for clicking')
#     loadData()
#     tableData()
#   })


### SUBMIT DATA SERVER LOGIC ####

### Process DATA

# Process Action Button
output$process.UI <- renderUI({
  req(staged_df())
  actionButton(inputId = "process",
               label = paste0("Process Staged Records"),
               width = '500px')
})

# Run the function to process the data and return 2 dataframes and path as list
dfs <- eventReactive(input$process,{
  source("funs/process.R", local = T) # Hopefully this will overwrite functions as source changes...needs more testing
  PROCESS()
})

# Extract each dataframe
processedData <- reactive({
  dfs()[[1]]
})
processedComments  <- reactive({
  dfs()[[2]]
})

### Table Outputs

# Processed WQ Table - Only make table if processing is successful
output$table.process.data <- renderDataTable({
  req(try(processedData()))
  processedData() %>% datatable(editable = FALSE, rownames = FALSE,
                              colnames = data_fields$dt_cols, selection = "single",
                              options = list(searching = TRUE, lengthChange = TRUE))

})

# Processed Flag Table - Only make table if processing is successful
output$table.process.comments <- renderDataTable({
  req(try(processedComments()))
  processedComments() %>% datatable(editable = FALSE, rownames = FALSE,
                                    selection = "single",
                                    options = list(searching = TRUE, lengthChange = TRUE))
})

# Text for Process Data Error or Successful
# process.status <- reactive({
#   if(processedData()){
#     " "
#   } else if(inherits(try(df.wq()), "try-error")){
#     geterrmessage()
#   } else {
#     paste0('The records were successfully processed')
#   }
# })
observeEvent(input$process, {
  show('submit')
  # show('table.process.wq')
  # show('table.process.flag')
})

# Import Action Button - Will only be shown when a file is processed successfully
output$submit.UI <- renderUI({
  req(dfs())
  req(try(staged_df()))
  actionButton(inputId = "submit",
               label = paste("Submit Staged Records"),
               width = '500px')
})

# Import Data - Run import_data function
observeEvent(input$submit, {
  source("funs/dropB.R", local = T)
  out <- tryCatch(SUBMIT_CSV(zone = user_zone),
                  error=function(cond) {
                    message(paste("There was an error, records not submitted...\n", cond))
                    return(NA)
                  },
                  warning=function(cond) {
                    message(paste("Submittal process completed with warnings...\n", cond))
                    return(NULL)
                  },
                  finally={
                    message(paste("Submittal Process Complete"))
                  }
  )
  ImportFailed <- any(class(out) == "error")

  if (ImportFailed == TRUE){
    print(paste0("Submittal Failed at ", Sys.time() ,". There was an error: "))
    print(out)
  } else {
    print(paste0("Data Submittal Successful at ", Sys.time()))
    NewCount <- actionCount() + 1
    actionCount(NewCount)
    print(paste0("Action Count was ", actionCount()))
    # ImportEmail()
  }
})

### Function to send ImportEmail
ImportEmail <- function() {
  out <- tryCatch(
    message("Trying to send email"),
    sendmail(from = paste0("<",useremail,">"),
             to = distro1(),
             subject = paste0("New monitoring data have been submitted to the BRC Coordinator"),
             msg = paste0(app_user," has submitted ", nrow(submittedData()), " new record(s) for review and import to the BRCWQDM database."),
             control=list(smtpServer=MS))
    ,
    error=function(cond) {
      message(paste("User cannot connect to SMTP Server, cannot send email", cond))
      return(NA)
    },
    warning=function(cond) {
      message(paste("Send mail function caused a warning, but was completed successfully", cond))
      return(NULL)
    },
    finally={
      message(paste("Submittal email was sent successfully"))
    }
  )
  return(out)
}

# Hide import button and tables when import button is pressed (So one cannot double import same file)
observeEvent(input$submit, {
  hide('submit')
  #hide('table.process.wq')
  #hide('table.process.flag')
})

# Add text everytime successful import
observeEvent(input$submit, {
  insertUI(
    selector = "#submit",
    where = "afterEnd",
    ui = h4(paste("Successful submittal of", nrow(processedData()), "record(s) to the BRC Coordinator"))
  )
})

  observeEvent(input$save_changes,{
    removeModal() # Save does not work here?
  })

### IMAGES ####

  # DCR IMAGE
  output$brc_logo1 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo1 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)
  output$brc_logo2 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo2 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)
  output$brc_logo3 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo3 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)
  output$brc_logo4 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo4 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)
  output$brc_logo5 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo5 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)

### SESSION END ####
# Code to stop app when browser session window closes
session$onSessionEnded(function() {
      print(paste0("BRCWQDM session ended at ", Sys.time()))
      stopApp()
    })

} # END SERVER FUNCTION ####

shinyApp(ui = ui, server = server)


