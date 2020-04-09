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
### Add login page (password Input- login provides the following:
  # FC- Name - Region - site and volunteer lists should get filtered automatically
  # PC - gets access to all sites and volunteers, can see hidden modules and access to import button
# Should submitted records be in tidy format? or non-tidy - for archive -
# non-tidy format will preserve nulls, whereas tidy format will not
# In other data add a button for attach photo - similar to comment - Add photo name, parameter number, photo credit
#       is selectize input where Field Monitor is default - but can type in other name.
#       Photo is logged and a csvFile is generated
# Add save button to submitted data page (for edits)
# Add process and import UI to server (use WIT at template)
# Update select input after import on submitted data tab - so file selector updates
# Add comment modal button for submitted comments
# When datatable modules are empty- disable buttons or remove entirely and display a helpful message

print(paste0("BRCWQDM App lauched at ", Sys.time()))

### Load Libraries ####
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/", quiet = T, verbose = F)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("shiny","shinyjs", "shinyFiles", "shinyTime", "shinyalert","shinydashboard","rmarkdown", "knitr", "tidyselect", "lubridate",
              "plotly", "leaflet", "RColorBrewer", "data.table", "DT", "scales", "stringr", "shinythemes", "ggthemes", "tidyr",
              "dplyr", "magrittr", "httr", "tibble", "bsplus", "readxl", "rdrop2", "RSQLite", "readr", "purrr", "htmlwidgets", "ggplot2",
              "pool", "curl", "glue")

# install.packages("https://github.com/jeroen/curl/archive/master.tar.gz", repos = NULL)
# update.packages("curl", repos="http://cran.rstudio.com/", quiet = T, verbose = F)

# source('all_sessions.R', local = TRUE)
 # "devtools"
# "miniUI"
# "rstudioapi"
# "shinyFiles"
# "rgdal"

### Set Directories ####
wdir <<- getwd()
### LOCAL PROJECT DIRECTORY ####
### Settings dependent on launch mode (docker or normal) ####
if(launch_mode == "docker") {
  suppressPackageStartupMessages(
    sapply(packages, require, character.only = TRUE)
   )
  LocalDir <<- "/usr/local/src/LocalProjectDir/"
} else { # app is not run in docker container
  suppressPackageStartupMessages(
    ipak(packages)
  )
  LocalDir <<- config[1]
}

  dataDir <<- paste0(LocalDir,"Data/")
  app_user <<- config[2]
  user_zone <<- config[13]

### CSV Files ####
stagedDataCSV <<- paste0(dataDir,"StagedData/BRC_StagedData.csv")
stagedCommentsCSV <<- paste0(dataDir,"StagedData/BRC_StagedComments.csv")
submittedDataDir <<- paste0(dataDir,"SubmittedData")

### RDS FILES ####
rdsFiles <<- paste0(dataDir,"rdsFiles/")
stagedDataRDS <<- paste0(rdsFiles,"stagedData.rds")
stagedCommentsRDS <<- paste0(rdsFiles,"stagedComments.rds")
submittedDataRDS <<- paste0(rdsFiles,"submittedData.rds")
submittedCommentsRDS <<- paste0(rdsFiles,"submittedComments.rds")

### Database Data RDS - updated each time data is imported or using admin tools
data_n_RDS <- paste0(rdsFiles,"data_num_db.rds")
data_t_RDS <- paste0(rdsFiles,"data_text_db.rds")
data_c_RDS <- paste0(rdsFiles,"data_comment_db.rds")
trans_log_RDS <-  paste0(rdsFiles,"trans_log_db.rds")
### RDS DATABASE FILES ####

### Periodic updates - supporting tables
sitesRDS <- paste0(rdsFiles,"sites_db.rds")
peopleRDS <- paste0(rdsFiles,"people_db.rds")
parametersRDS <- paste0(rdsFiles,"parameters_db.rds")
assignmentsRDS <- paste0(rdsFiles,"assignments_db.rds")

### From edit module ####
useShinyalert()

source(paste0(wdir, "/funs/dropB.R"))
### Download rds files cached on dropbox to local data folder and load these and any staged RDS files
# LOAD_DB_RDS()

### Download database rds files from dropbox ####
GET_DATABASE_DATA()

### Change to the last record date (rds file)
last_update <- data_num_db$DATE_TIME %>% max()

### AS IS fields require no manipulation and can go directly to outputs
### Date and Time and any other QC'd values need to come from reactive elements

remote_data_dir <- paste0(getwd(),"/data/")

table_fields <<- readr::read_csv(paste0(wdir,"/data/table_fields.csv"), col_types = cols(
  shiny_input = col_character(),
  dt_cols = col_character(),
  col_type = col_character(),
  col_input = col_character(),
  input_mult = col_character(),
  editable = col_character(),
  pcode = col_character(),
  take_comments = col_character(),
  choices = col_character(),
  input_width = col_double(),
  as_is = col_character(),
  input_section = col_character()
))

data_fields <<- table_fields[1:32,]
comment_fields <<- table_fields[33:37,]

### DATA FIELDS ####
fieldsASIS <<- data_fields$shiny_input[data_fields$as_is == "yes"]
### Data Column names csv ####
col_names <<- data_fields$shiny_input
### Comment Column Names csv ####
comm_col_names <<- comment_fields$shiny_input

### Select Option Choices ####
### All selection dependent lists need to go in server
### These lists are static
sites_db <- sites_db %>%
  arrange(WATERBODY_NAME)
sites <<- sites_db$BRC_CODE
names(sites) <- paste0(sites_db$WATERBODY_NAME, " - ", sites_db$SITE_NAME, " (", sites_db$BRC_CODE, ")")
sites <<- sites

samplers <<-  assignments_db %>%
  filter(ROLE == "Field", YEAR ==  year(Sys.Date())) %>%
   add_case(NAME = "BRC SamplerX") %>%
  .$NAME %>%
  sort()

wea_choices <<- c("Storm (heavy rain)", "Rain (steady rain)",
                 "Showers (intermittent rain)", "Overcast","Clear/Sunny", "Other","Not Recorded")
wat_appear_choices <<- c("Clear", "Milky", "Foamy", "Oily Sheen",
                        "Dark Brown", "Greenish", "Orange", "Tea Color", "Other", "Not Recorded")
wat_trash_choices <<- c("None", "Light", "Medium", "Heavy", "Not Recorded")
wat_odor_choices <<-c("None", "Sewage", "Fishy", "Chlorine",
                     "Rotten Eggs", "Other", "Not Recorded")
wat_NAV_choices <<- c("None", "Light", "Medium", "Heavy", "Not Recorded")
wat_clarity_choices <<- c("Clear","Slight","Medium","Heavy", "Not Recorded")
wat_erosion_choices <<- c("Undercut bank", "Slumping", "Erosional gullies in bank",
                         "Bridge or building undermining", "No erosion", "Not Recorded")
depth_choices <<- c("Gage (Staff Plate-feet)", "Ruler (inches)", "Not Recorded", "No Datum")

### SOURCE EXTERNAL SCRIPTS ####
source(paste0(wdir, "/funs/csv2df.R"))
source(paste0(wdir, "/funs/editableDT_modFuns.R"))
source(paste0(wdir, "/mods/mod_editDT.R"))
source(paste0(wdir, "/funs/sendEmail.R"))
source(paste0(wdir, "/mods/mod_add_comment.R"))
source(paste0(wdir, "/mods/mod_map.R"))
source(paste0(wdir, "/funs/data_update.R"))

  rxdata <<- reactiveValues()
  loadData <<- function() {
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

  loadComments <<- function() {
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


  loadAll <<- function(){
    loadData()
    loadComments()
  }

  loadAll()


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
SubmitActionCount <- reactiveVal(0)
ImportActionCount <-  reactiveVal(0)
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
               column(4,strong(paste("Most recent date of data: ", last_update)),br()),
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
                      column(width = 6,
                        selectInput("site", labelMandatory("Choose Sample Location:"), c("",sites), selected = "")
                      ),
                      column(width = 6,
                             selectInput("sampler", labelMandatory("Choose Sampler(s):"), multiple = T, choices = c("",samplers), selected = "")
                      )
                    ),
                    fluidRow(
                      column(width = 4,
                        dateInput("date", labelMandatory("Sample Date:"))
                      ),
                      column(width = 3,
                        timeInput("time", labelMandatory("Sample Starting Time (24-hr format):"), seconds = FALSE)
                      ),
                      column(width = 2,
                        numericInput("lab_num", "Lab Record#:", value = NULL, min = 1, max =50, step = 1)
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
                          radioButtons("wea48", "Weather Last 48 Hours (P01):", choices = wea_choices, selected = "Not Recorded"),
                          textAreaInput("comm_P01", labelMandatory("Comments for Weather Last 48 Hours (P01):"), placeholder = "Describe 'other'"),
                          radioButtons("wea_now", "Weather at time of sample (P02):", choices = wea_choices, selected = "Not Recorded"),
                          textAreaInput("comm_P02", labelMandatory("Comments for Weather at time of Sample (P02):"), placeholder = "Describe 'other'"),
                          numericInput("temp_air","Ending Air Temperature (C) (P03):",
                                       value = NULL, min = -20, max = 40, step = 0.5),
                          numericInput("temp_wat", "Ending Water Temperature (C) (P04):",
                                       value = NULL, min = 0, max = 30, step = 0.5),
                          ADD_COMMENT_UI("add_comment_physical")
                        ),
                        column(width = 3,
                          checkboxGroupInput("wat_appear", "Water Appearance (P05):", choices = wat_appear_choices),
                          textAreaInput("comm_P05", labelMandatory("Comments for Water Appearance (P05):"), placeholder = "Describe 'other'"),
                          radioButtons("wat_trash", "Presence of Trash (P06):", choices = wat_trash_choices, selected = "Not Recorded")
                        ),
                        column(width = 3,
                          checkboxGroupInput("erosion", "Stream bank/infrastructure erosion (P07):", choices = wat_erosion_choices),
                          textAreaInput("comm_P07", labelMandatory("Comments for Erosion (P07):"), placeholder = "Describe 'other'"),
                          checkboxGroupInput("wat_odor", "Water Odor (P08):", choices = wat_odor_choices),
                          textAreaInput("comm_P08", labelMandatory("Comments for Water Odor (P08):"), placeholder = "Describe 'other'")
                        ),
                        column(width = 3,
                          radioButtons("wat_nav", "Nuisance Aquatic Vegetation (NAV) (P09):", choices = wat_NAV_choices, selected = "Not Recorded"),
                          radioButtons("wat_clarity", "Water Clarity (Visual Turbidity) (P10)", choices = wat_clarity_choices, selected = "Not Recorded"),
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
                        radioButtons("depth_type", "Type of depth measurement (D01):",
                                     choiceNames = depth_choices, choiceValues = depth_choices, selected = "No Datum")
                      ),
                      column(width = 4,
                        uiOutput("depth")
                      ),
                      column(width = 4,
                              ADD_COMMENT_UI("add_comment_depth")
                      ) # End Col
                  )
                  ))#End FR
                ) # End Well Panel
        ) %>%
        bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
        bs_append(title = "CHEMICAL PARAMETERS", content =
                    wellPanel(fluidRow(
                      column(width = 12,
                        fluidRow(
                          column(width = 3,
                            numericInput("do","Dissolved Oxyen (mg/L) (C01):", value = NULL, min = 0, max = 25),
                            numericInput("o2","Oxygen Saturation (%) (C02):", value = NULL, min = 0, max = 100, step = 1),
                            ADD_COMMENT_UI("add_comment_chemical")
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
        bs_append(title = "BIOLOGICAL PARAMETERS", content =
                    wellPanel(fluidRow(
                      column(width = 12,
                        fluidRow(
                          column(width = 4,
                            numericInput("e_coli", "E. coli (MPN/100mL) (B01):", value = NULL, min = 0, max = 25),
                            numericInput("e_coli_rep", "E. coli - Replicate (MPN/100mL) (B02):", value = NULL, min = 0, max = 100, step = 1),
                            ADD_COMMENT_UI("add_comment_biological")
                            ),
                          column(width = 4,
                             numericInput("e_coli_lab_blank", "E. coli - Lab Blank (MPN/100mL) (B03):", value = NULL, min = 0, max = 20),
                             numericInput("e_coli_field_blank", "E. coli - Field Blank (MPN/100mL) (B04):", value = NULL, min = 0, max = 20)
                          ))
                      )
                    )) # End Well Panel and FR
        ) %>%
        bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
        bs_append(title = "OTHER SAMPLE INFORMATION", content =
                    wellPanel(fluidRow(
                      column(width = 4,
                             ADD_COMMENT_UI("add_comment_other")
                      ),
                      column(width = 4,
                             checkboxInput("photos","Photos associated with sampling event?")

                      ),
                      column(width = 4,
                             actionButton("add_photo", "Add Photo"),
                             h4("This feature coming soon")
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
                    column(12,
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
                    column(12,
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
                tabPanel("PROCESS & SUBMIT", value = 'process_submit', type = "pills",
                  fluidRow(
                    column(12,
                      h4("Make sure all data has been entered and checked over for accuracy. Click the 'PROCESS' button \n
                            when you are ready to proceed. During processing the data will be checked for errors and anomalies.\n
                            If no problems are found then you may submit the data to the Program Manager for final QC and database import.")
                    # actionButton(inputId = "process",label = "PROCESS", width = "200px", class="butt"),
                    # actionButton(inputId = "submit",label = "SUBMIT", width = "200px", class="butt"),
                    )
                  ),
                  fluidRow(
                    column(6,
                    wellPanel(
                        strong(h4("Process staged records:")),
                        br(),
                        uiOutput("process1.UI"),
                        br(),
                        h4(textOutput("text.process1.status"))
                      )
                    ),
                    column(6,
                      wellPanel(
                        strong(h4("Submit processed staged records to the BRC Program Coordinator:")),
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
                                      dataTableOutput("table.process1.data")
                             ),
                             tabPanel("Processed Comments",
                                      dataTableOutput("table.process1.comments")
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
        ),
        fluidRow(
          column(12,
                 tags$head(tags$style(HTML('
                                              .modal-lg {
                                              width: 1200px;
                                              }
                                              '))),
                 uiOutput("selectFile_ui"),
                 br(),

                 uiOutput("submitted_data.UI")
        ) #End Col
        ) # End FR
    ), # End Tab Panel
  ### MORE TAB ####
  navbarMenu("More",
    ### DATABASE TAB ####
    tabPanel("DATABASE",
        fluidRow(
           column(2, imageOutput("brc_logo4", height = 80), align = "left"),
           column(8,  h2("BRC Water Quality Database", align = "center")),
           column(2, imageOutput("zap_logo4", height = 80), align = "right")
         ),
        fluidRow(
          column(12,
                 wellPanel(
                   strong(h4("BRCWQDM DATABASE TABLES")),
                   br()
                 ),
                 tabsetPanel(
                   tabPanel("Numeric Data",
                            fluidRow(downloadButton("download_data_num", "Download table as csv"), align = "center"),
                            DTOutput("data_num_db")
                   ),
                   tabPanel("Text Data",
                            fluidRow(downloadButton("download_data_text", "Download table as csv"), align = "center"),
                          DTOutput("data_text_db")
                   ),
                   tabPanel("Comments",
                            fluidRow(downloadButton("download_data_comments", "Download table as csv"), align = "center"),
                            DTOutput("data_comment_db")
                   ),
                         tabPanel("Transaction Log",
                            fluidRow(downloadButton("download_trans_log", "Download table as csv"), align = "center"),
                           DTOutput("data_trans_log_db")
                   )
                 )  # End Tab Panel
          ) # End Col
        ) # End Fluid row
    ), # End Tab Panel
    ### MAP TAB ####
    tabPanel("MAP",
        fluidRow(
           column(2, imageOutput("brc_logo5", height = 80), align = "left"),
           column(8,  h2("BRC Water Quality Sampling Sites", align = "center")),
           column(2, imageOutput("zap_logo5", height = 80), align = "right")
         ),
        fluidRow(
          column(12,
                 BRCMAP_UI("brc_map")
          )
        )
    ),
    ### REPORTS TAB ####
    tabPanel("REPORTS",
        fluidRow(
           column(2, imageOutput("brc_logo6", height = 80), align = "left"),
           column(8,  h2("Reports", align = "center")),
           column(2, imageOutput("zap_logo6", height = 80), align = "right")
         ),
         fluidRow(
           column(12,
                  h4("Click this button to test email functionality - check email and log file for result", align = "center"),
                      actionButton(inputId = "email_test",label = "Send a testing Email", width = "245px", class="butt")
           )
         )
    ),
    ### INSTRUCTIONS TAB ####
    tabPanel("INSTRUCTIONS",
      fluidRow(
           column(2, imageOutput("brc_logo7", height = 80), align = "left"),
           column(8,  h2("BRC Water Quality Sampling Sites", align = "center")),
           column(2, imageOutput("zap_logo7", height = 80), align = "right")
         ),
      fluidRow(column(12,

                  h2("Instructions and Data Processing Workflow", align = "center"),
                    htmlOutput("instructions")
      )
      )
    ),
    ### ADMIN TOOLS ####
    tabPanel("ADMIN TOOLS",
        fluidRow(
           column(2, imageOutput("brc_logo8", height = 80), align = "left"),
           column(8,  h2("Admin Tools", align = "center")),
           column(2, imageOutput("zap_logo8", height = 80), align = "right")
         ),
    uiOutput("admin_tools.UI")
    )
  ) # End navbar page
  )
) # End UI - taglist

####################################################.
###                   SERVER                    ####
####################################################.

server <- function(input, output, session) {

### Generate User list ####
user_list <<- assignments_db %>%
    filter(YEAR == max(assignments_db$YEAR), ROLE %in% c("Field Coordinator","Program Coordinator", "App Developer")) %>%
  .$NAME

### Verify User ####
if(app_user %in% user_list){
  print(paste0("App user '", app_user, "' verified!"))
} else {
  stop("App user '", app_user,"' cannot be verified - please contact the program coordinator and ensure your configuration file user name matches your role in the BRCWQDM database. ")
}

### Set User Role ####
user_role <<- filter(assignments_db, YEAR == max(assignments_db$YEAR),
                     NAME == app_user,
                     ROLE %in% c("App Developer", "Program Coordinator", "Field Coordinator")) %>% .$ROLE
# user_role <<- "Field Coordinator"

if(user_role %in% c("App Developer", "Program Coordinator")){
      shinyjs::show("ADMIN TOOLS")
} else {
      shinyjs::hide("ADMIN TOOLS")
}

### Download submitted data from dropbox ####
GET_SUBMITTED_DATA()

fileChoices <- function(){
      # if(submittedFileNum > 0){
        dataFiles <- list.files(submittedDataDir, pattern = "*Data*", full.names = TRUE)
        names(dataFiles) <- list.files(submittedDataDir, pattern = "*Data*", full.names = FALSE) %>%
          str_replace_all("_SubmittedData_"," ") %>% str_replace_all(".csv","")
        dataFiles
      # } else{
      #   NULL
      # }
    }

rxdata$fileChoices <- fileChoices()

selected_site <- reactive({
  input$site
  })
selected_date <- reactive({
  input$date
  })
selected_sampler <- reactive({
  input$sampler
  })

if(user_role == "Program Coordinator"){
      selectFile_lab <<-  "Choose submitted data to process and import:"
      } else {
      selectFile_lab <<- "Choose previously submitted data to view:"
}

# SelectFile UI
output$selectFile_ui <- renderUI({
      selectInput("selectFile", label = selectFile_lab,
                  choices = c("", rxdata$fileChoices), selected = "" , multiple = FALSE, width = "400px")
    })

    # Update Select Input when a file saved imported (actually when the import button is pressed (successful or not))
    observeEvent(input$SaveSubmittedData, {
      updateSelectInput(session = session,
                        inputId = "selectFile",
                        label = selectFile_lab,
                        choices = c("", rxdata$fileChoices),
                        selected = "")
    })

    # Update Select Input when a file is imported (actually when the import button is pressed (successful or not))

    observeEvent(input$import,{
      rxdata$fileChoices <- fileChoices()
    })

    observeEvent( input$submit,{
      rxdata$fileChoices <- fileChoices()
    })

    observeEvent(input$import, {
     updateSelectInput(session = session,
                       inputId = "selectFile",
                       label = selectFile_lab,
                       choices = c("", rxdata$fileChoices),
                       selected = "")
    })

     observeEvent(input$submit, {
      updateSelectInput(session = session,
                        inputId = "selectFile",
                        label = selectFile_lab,
                        choices = c("",rxdata$fileChoices),
                        selected = "")
    })

  DataEditCols <- data_fields$dt_cols[data_fields$editable == "yes"]
  CommentEditCols <- comment_fields$dt_cols[comment_fields$editable == "yes"]

  staged_df <- callModule(editableDT, "stagedDataDT",
                   data = reactive(rxdata$stagedData),
                   data_name = "stagedData",
                   inputwidth = reactive(170),
                   edit_cols = DataEditCols)

  staged_comments <- callModule(editableDT, "stagedCommentsDT",
                          data = reactive(rxdata$stagedComments),
                          # rxdata = "stagedComments",
                          data_name = "stagedComments",
                          inputwidth = reactive(170),
                          edit_cols = CommentEditCols)

  submitted_df <- callModule(editableDT, "submittedDataDT",
                          data = reactive(rxdata$submittedData),
                          # rxdata = "submittedData",
                          data_name = "submittedData",
                          inputwidth = reactive(170),
                          edit_cols = DataEditCols)

  submitted_comments <- callModule(editableDT, "submittedCommentsDT",
                                data = reactive(rxdata$submittedComments),
                                # rxdata = "submittedComments",
                                data_name = "submittedComments",
                                inputwidth = reactive(170),
                                edit_cols = CommentEditCols)

  observeEvent(input$selectFile, {
    req(input$selectFile != "")
    data_csv <<- input$selectFile
    comment_csv <<- str_replace(data_csv,"_SubmittedData_","_SubmittedComments_")

    data <- read.table(data_csv, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
    df <- data_csv2df(data, data_fields) ### saves RDS file as data.frame
    saveRDS(df, submittedDataRDS)
    rxdata$submittedData <- readRDS(submittedDataRDS)
  if (file.exists(comment_csv)) {
    data <- read.table(comment_csv, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
    df <- comm_csv2df(data, comment_fields) ### saves RDS file as data.frame
    saveRDS(df, submittedCommentsRDS)
    rxdata$submittedComments <- readRDS(submittedCommentsRDS)
  } else {
    print("There were no submitted comments associated with the submitted data file")
  }
  })


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
      updateCheckboxGroupInput(session, "wea48",
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
     updateCheckboxGroupInput(session, "wea_now",
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
      updateCheckboxGroupInput(session, "wat_appear",
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
     updateCheckboxGroupInput(session, "erosion",
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
     updateCheckboxGroupInput(session, "wat_odor",
      selected = "Not Recorded"
    )
  }
})
###
observeEvent(input$wat_clarity, {
  if("Not Recorded" %in% input$wat_clarity){
      updateCheckboxGroupInput(session, "wat_clarity",
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
  paste0(input$sampler, collapse = "; ") %>% trimws()
  })
# wea48RX <- reactive({
#   req(input$wea48)
#   paste(input$wea48, collapse = "; ") %>% trimws()
#   })
# wea_nowRX <- reactive({
#   req(input$wea_now)
#   paste(input$wea_now, collapse = "; ") %>% trimws()
#   })
wat_appearRX <- reactive({
  req(input$wat_appear)
  paste0(input$wat_appear, collapse = "; ") %>% trimws()
  })
erosionRX <- reactive({
  req(input$erosion)
  paste0(input$erosion, collapse = "; ") %>% trimws()
  })
wat_odorRX <- reactive({
  req(input$wat_odor)
  paste0(input$wat_odor, collapse = "; ") %>% trimws()
  })

depth_typeRX <- reactive({
  req(input$depth_type)
  input$depth_type
  })

output$depth <- renderUI({
  req(depth_typeRX() %in% c("Ruler (inches)", "Gage (Staff Plate-feet)"))
  switch(depth_typeRX(),
         "Gage (Staff Plate-feet)" = numericInput("depth_meas","Water Depth (feet) (D02):",
                                     value = NULL, min = 0, max = , step = 0.01),
         "Ruler (inches)" = numericInput("depth_meas","Ruler Water Depth (Decimal Inches) (D02):",
                                          value = NULL, min = 0, max = 24, step = 0.1)
  )
})

### FORM DATA ####
formData <- reactive({
  data <- sapply(fieldsASIS, function(x) input[[x]])
  data <- c(data, "SampleDateTime" = SampleDT(), "sampler" = samplersRX(), wat_appear = wat_appearRX(),
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
              qmethod = "d", append = FALSE)
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

update_inputs <- function(){
   updateRadioButtons(session, "wea48", "Weather Last 48 Hours (P01):", choices = wea_choices, selected = "Not Recorded")
   updateRadioButtons(session, "wea_now", "Weather at time of sample (P02):", choices = wea_choices, selected = "Not Recorded")
   updateRadioButtons(session, "wat_nav", "Nuisance Aquatic Vegetation (NAV) (P09):", choices = wat_NAV_choices, selected = "Not Recorded")
   updateRadioButtons(session, "wat_clarity", "Water Clarity (Visual Turbidity) (P10)", choices = wat_clarity_choices, selected = "Not Recorded")
   updateRadioButtons(session, "wat_trash", "Presence of Trash (P06):", choices = wat_trash_choices, selected = "Not Recorded")
   # updateDateInput(session, "date", labelMandatory("Sample Date:"), value = today())
   # updateTimeInput(session, "time", labelMandatory("Sample Starting Time (24-hr format):"), value = strptime("00:00", "%H:%M"))
}

observe({
update_inputs()
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

### COMMENT CHOICES ####
comment_par_choices <<- c("General Comment", data_fields$dt_cols[data_fields$take_comments =="yes"])

  ### save Staged Data to RDS and CSV ####
  observeEvent(input$SaveStagedData,{
    req(staged_df())
    csv <- staged_df()
    names(csv) <- data_fields$shiny_input
    #### overwrite table as well ... write.table
    write.table(x = csv, file = stagedDataCSV,
                row.names = FALSE, col.names = TRUE, quote = TRUE,
                qmethod = "d", append = FALSE)
     loadData()
     ### Read the updated table back and refresh the DT
     data <- read.table(stagedDataCSV, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
     df <- data_csv2df(data, data_fields) ### saves RDS file as data.frame
     saveRDS(df, stagedDataRDS)
     rxdata$stagedData <- readRDS(stagedDataRDS)
     shinyalert(title = "Saved!", type = "success")
  })

   observeEvent(input$SaveSubmittedData,{
     req(submitted_df())
     csv <- submitted_df() ### This is the current state of DT
     names(csv) <- data_fields$shiny_input # Change col names back to csv format
     ### Set the file name
     file <- input$selectFile
     #### overwrite table as well ... write.table
     write.table(x = csv, file = file,
                 row.names = FALSE, col.names = TRUE, quote = TRUE,
                 qmethod = "d", append = FALSE)
    ### Read the updated table back and refresh the DT
     data <- read.table(file, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
     df <- data_csv2df(data, data_fields) ### saves RDS file as data.frame
     saveRDS(df, submittedDataRDS)
     rxdata$submittedData <- readRDS(submittedDataRDS)
     shinyalert(title = "Saved!", type = "success")
   })

   ### Save Staged Comments to RDS and CSV ####
   observeEvent(input$SaveStagedComments,{
     req(staged_comments())
     csv <- staged_comments()
     names(csv) <- comment_fields$shiny_input
     #### overwrite table as well ... write.table
     write.table(x = csv, file = stagedCommentsCSV,
                 row.names = FALSE, col.names = TRUE, quote = TRUE,
                 qmethod = "d", append = FALSE)
     # saveRDS(object = staged_(), stagedDataRDS)
     ### Read the updated table back and refresh the DT
     data <- read.table(stagedCommentsCSV, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
     df <- comm_csv2df(data, comment_fields) ### saves RDS file as data.frame
     saveRDS(df, stagedCommentsRDS)
     rxdata$stagedComments <- readRDS(stagedCommentsRDS)
     shinyalert(title = "Saved!", type = "success")
   })

   ### Save Submitted Comments to RDS and CSV ####
   observeEvent(input$SaveSubmittedComments,{
     req(submitted_comments())
     csv <- submitted_comments()
     names(csv) <- comment_fields$shiny_input
     file <- str_replace(input$selectFile, "_SubmittedData_","_SubmittedComments_")
     #### overwrite table as well ... write.table
     write.table(x = csv, file = file , row.names = FALSE, col.names = TRUE, quote = TRUE,
                 qmethod = "d", append = FALSE)
     ### Read the updated table back and refresh the DT
     data <- read.table(file, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
     df <- comm_csv2df(data, comment_fields) ### saves RDS file as data.frame
     saveRDS(df, submittedCommentsRDS)
     rxdata$submittedComments <- readRDS(submittedCommentsRDS)
     shinyalert(title = "Saved!", type = "success")
   })

  saveComment <- function(data, csvFile, rdsFile) {
         if(file.exists(csvFile)){
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
  shinyalert(title = "Comment Entered!", type = "success")
  removeModal()
})


### SUBMITTED DATA UI ####

output$submitted_data.UI <- renderUI({
if (user_role %in% c("Program Coordinator", "App Developer")){
  tabsetPanel(
    tabPanel("SUBMITTED DATA", type = "pills",
             fluidRow(
               column(12,
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
                      actionButton(inputId = "SaveSubmittedData",label = "Save", width = "245px", class="butt"),
                      editableDTUI("submittedDataDT")
               ),
               column(6,
                      verbatimTextOutput("rec_comments2")
               )
             )
    ), # END DATA tp
    tabPanel("SUBMITTED COMMENTS", type = "pills",
             fluidRow(
               column(12,
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
                      actionButton(inputId = "SaveSubmittedComments",label = "Save", width = "245px", class="butt"),
                      # Set up shinyalert
                      editableDTUI("submittedCommentsDT")
               )
             )
    ), # End tp
    tabPanel("PROCESS & IMPORT", type = "pills",
             fluidRow(
               column(12,
                      h4("Make sure all data has been checked over for accuracy. Click the 'PROCESS' button \n
                                        when you are ready to proceed. During processing the data will be checked for errors and anomalies.\n
                                        If no problems are found then you may import the records to the database.")
               )
             ),
             fluidRow(
               column(6,
                      wellPanel(
                        strong(h4("Process submitted records:")),
                        br(),
                        uiOutput("process2.UI"),
                        br(),
                        h4(textOutput("text.process2.status"))
                      )
               ),
               column(6,
                      wellPanel(
                        strong(h4("Import processed submitted records to the database:")),
                        br(),
                        uiOutput("import.UI"),
                        br(),
                        uiOutput("text.import.status")
                      )
               )
             ),
             fluidRow(
               column(12,
                      tabsetPanel(
                        tabPanel("Processed Numeric Data",
                                 dataTableOutput("table.process2.data_n")
                        ),
                        tabPanel("Processed Text Data",
                                 dataTableOutput("table.process2.data_t")
                        ),
                        tabPanel("Processed Comments",
                                 dataTableOutput("table.process2.data_c")
                        ),
                        tabPanel("Processed Transaction Log",
                                 dataTableOutput("table.process2.trans_log")
                        )  # End Tab Panel
                      ) # End Tabset Panel
               ) # End Col
             ) # End Fluid row
    ) # End TabPanel
  ) # End TabSet Panel
} else {
   tabsetPanel(
    tabPanel("SUBMITTED DATA", type = "pills",
             fluidRow(
               column(12,
                      dataTableOutput("submitted_data_dt")
               )
             )
    ), # END DATA tp
    tabPanel("SUBMITTED COMMENTS", type = "pills",
             fluidRow(
               column(12,
                dataTableOutput("submitted_comments_dt")
               )
             )
    )
  ) # End TabSet Panel
}

})


output$submitted_data_dt <- renderDataTable(
  rxdata$submittedData %>% datatable(editable = FALSE, rownames = FALSE,
                              colnames = data_fields$dt_cols, selection = "single",
                              options = list(searching = TRUE, lengthChange = TRUE))
)

output$submitted_comments_dt <- renderDataTable(
  rxdata$submittedComments %>% datatable(editable = FALSE, rownames = FALSE,
                                    selection = "single",
                                    options = list(searching = TRUE, lengthChange = TRUE))
)


########################################################################.
###                           PROCESS/SUBMIT DATA                   ####
########################################################################.

# Process Action Button
output$process1.UI <- renderUI({
  req(staged_df())
  actionButton(inputId = "process1",
               label = paste0("Process Staged Records"),
               width = '500px')
})

# Run the function to process the data and return 2 dataframes and path as list
dfs <- eventReactive(input$process1,{
  source("funs/processSubmit.R", local = T) # Hopefully this will overwrite functions as source changes...needs more testing
  PROCESS1()
})


# Extract each dataframe
processedData1 <- reactive({
  dfs()$data
})
processedComments1  <- reactive({
  dfs()$comments
})

### Table Outputs

# Processed WQ Table - Only make table if processing is successful
output$table.process1.data <- renderDataTable({
  req(try(processedData1()))
  processedData1() %>% datatable(editable = FALSE, rownames = FALSE,
                              colnames = data_fields$dt_cols, selection = "single",
                              options = list(searching = TRUE, lengthChange = TRUE))

})

# Processed Flag Table - Only make table if processing is successful
output$table.process1.comments <- renderDataTable({
  req(try(!is.null(processedComments1())))
  processedComments1() %>% datatable(editable = FALSE, rownames = FALSE,
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
observeEvent(input$process1, {
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

########################################################################.
###                         SUBMIT DATA                             ####
########################################################################.
observeEvent(input$submit, {
  showModal(busyModal(msg = "Submitting data ..."))
  source("funs/dropB.R", local = T)
  out <- tryCatch(SUBMIT_CSV(zone = user_zone),
                  error = function(cond) {
                      submit_err <<- paste("Submittal Failed - There was an error at ", Sys.time() ,
                      "...\n ", cond)
                      print(submit_err)
                    return(1)
                  },
                  warning = function(cond) {
                    submit_err <<- paste("Submittal process completed with warnings...\n", cond)
                    print(submit_err)
                    return(2)
                  },
                  finally = {
                    message(paste("Submittal Process Complete"))
                  }
  )
  # submitFailed <- is.na(out)

  if (out == 1){
     removeModal()
      print(submit_err)
      submit_msg <<- paste0(submit_err, "\n... Check log file and review submitted data files and existing database records.")
       showModal(modalDialog(
              title = "Submittal Failed...",
              h4(submit_msg)
            ))
       } else {
    removeModal()
    submit_msg <<- paste("Successful submittal of", nrow(processedData1()), "record(s) to the BRC Program Coordinator")
    print(paste0("Data Submittal Successful at ", Sys.time()))
    showModal(modalDialog(
              title = "Submittal Successful...",
              h4(submit_msg)
            ))
    NewCount <- SubmitActionCount() + 1
    SubmitActionCount(NewCount)
    print(paste0("Submit Action Count was ", SubmitActionCount()))
    submitEmail()
    loadData()
    loadComments()
    rxdata$fileChoices <- fileChoices()
  }
})


### Generalize this to work with both submit and import
### Function to send ImportEmail
SubmitEmail <- function() {
  out <- tryCatch(
   submitEmail(),
      error=function(cond) {
        mail_msg <<- paste("There was an error with the Submit email function, cannot send email", cond)
        print(mail_msg)
        return(NA)
      },
      warning=function(cond) {
        mail_msg <<- paste("Submit email function caused a warning, but was completed successfully", cond)
        print(mail_msg)
        return(NULL)
      },
      finally={
       message("Submit email completed")
      }
  )
  return(out)
}

# Hide submit button and tables when import button is pressed (So one cannot double import same file)
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
    ui = h4(submit_msg)
  )
  # freezeReactiveValue(input, selectFile)
})

  observeEvent(input$save_changes,{
    removeModal() # Save does not work here?
  })

########################################################################.
###                   PROCESS/IMPORT SUBMITTED DATA                 ####
########################################################################.

# Process Action Button
  output$process2.UI <- renderUI({
    req(input$selectFile)
    actionButton(inputId = "process2",
                 label = "Process records from the selected file",
                 width = '500px')
  })

  # Run the function to process the data and return 2 dataframes and path as list
  dfs_to_import <- eventReactive(input$process2,{
    source("funs/processImport.R", local = T) # Hopefully this will overwrite functions as source changes...needs more testing
    PROCESS2(data_file = input$selectFile)
  })

  # Extract each dataframe
  # n is numerical
  df_data_n <- reactive({
    dfs_to_import()$data_n
  })
  # t is text
  df_data_t  <- reactive({
   dfs_to_import()$data_t
  })
  # c is comments
  df_data_c  <- reactive({
    dfs_to_import()$data_c
  })
 # transaction log
  df_data_trans_log  <- reactive({
    dfs_to_import()$trans_log
  })

  # Last File to be Processed
  file.processed2 <- eventReactive(input$process2, {
    input$selectFile
  })

    ### Table Outputs

  # Processed Data Num Table - Only make table if processing is successful
  output$table.process2.data_n <- renderDataTable({
    req(try(df_data_n()))
    df_data_n()
  })

  # Processed Data Text Table - Only make table if processing is successful
  output$table.process2.data_t <- renderDataTable({
    req(try(df_data_t()))
    df_data_t()
  })

    # Processed Data Comments Table - Only make table if processing is successful
  output$table.process2.data_c <- renderDataTable({
    req(try(df_data_c()))
    df_data_c()
  })
    # Processed Transaction Log Table - Only make table if processing is successful
  output$table.process2.trans_log <- renderDataTable({
    req(try(df_data_trans_log()))
    df_data_trans_log()
  })

### DATABASE PAGE ####

  if (file.exists(data_n_RDS)){
    rxdata$data_n_db <- readRDS(data_n_RDS)
  } else {
    rxdata$data_n_db <- NULL
  }

  if (file.exists(data_t_RDS)){
    rxdata$data_t_db <- readRDS(data_t_RDS)
  } else {
    rxdata$data_t_db <- NULL
  }

  if (file.exists(data_c_RDS)){
    rxdata$data_c_db <- readRDS(data_c_RDS)
  } else {
    rxdata$data_c_db <- NULL
  }

  if (file.exists(trans_log_RDS)){
    rxdata$data_trans_log_db <- readRDS(trans_log_RDS)
  } else {
    rxdata$data_trans_log_db <- NULL
  }

  output$data_num_db <- renderDataTable({
    req(!is.null(rxdata$data_n_db))
    datatable(rxdata$data_n_db,filter = "top")
  })

  output$data_text_db <- renderDataTable({
    req(!is.null(rxdata$data_t_db))
    datatable(rxdata$data_t_db,filter = "top")
  })

  output$data_comment_db <- renderDataTable({
    req(!is.null(rxdata$data_c_db))
    datatable(rxdata$data_c_db,filter = "top")
  })

  output$data_trans_log_db <- renderDataTable({
    req(!is.null(rxdata$data_trans_log_db))
    datatable(rxdata$data_trans_log_db,filter = "top")
  })

  # Downloadable csv of numerical data
  output$download_data_num <- downloadHandler(
    filename = function() {
      paste("BRC_NumericalData", ".csv", sep = "")
    },
    content = function(file) {
        df_csv <- rxdata$data_n_db
        # df_csv$SampleDateTime <- format(df_csv$SampleDateTime, usetz=TRUE)
      write_csv(df_csv, file)
    }
  )
    # Downloadable csv of text data
  output$download_data_text <- downloadHandler(
    filename = function() {
      paste("BRC_TextData", ".csv", sep = "")
    },
    content = function(file) {
        df_csv <- rxdata$data_t_db
        # df_csv$SampleDateTime <- format(df_csv$SampleDateTime, usetz=TRUE)
      write_csv(df_csv, file)
    }
  )
    # Downloadable csv of comment data
  output$download_data_comments <- downloadHandler(
    filename = function() {
      paste("BRC_CommentData", ".csv", sep = "")
    },
    content = function(file) {
        df_csv <- rxdata$data_c_db
        # df_csv$SampleDateTime <- format(df_csv$SampleDateTime, usetz=TRUE)
      write_csv(df_csv, file)
    }
  )

      # Downloadable csv of transaction log
  output$download_trans_log <- downloadHandler(
    filename = function() {
      paste("BRC_TransactionData", ".csv", sep = "")
    },
    content = function(file) {
        df_csv <- rxdata$data_trans_log_db
        # df_csv$SampleDateTime <- format(df_csv$SampleDateTime, usetz=TRUE)
      write_csv(df_csv, file)
    }
  )

### ADMIN TOOLS UI ####
# Add buttons to enter volunteers/add roles?
#
### Other Tools:
    # csv download for tables
    #
output$admin_tools.UI <- renderUI({
if (user_role %in% c("Program Coordinator", "App Developer")){
  fluidRow(
    column(6,
           verbatimTextOutput("testing_mode_text"),
           checkboxInput("testing_mode", label = "Turn on testing mode", value = FALSE),
           verbatimTextOutput("update_db_text"),
           actionButton(inputId = "update_db_rds",label = "UPDATE DATABASE TABLES", width = "245px", class="butt")
    ))
}
})

observeEvent(input$testing_mode, {
  if(input$testing_mode == TRUE){
     rxdata$t_mode <<- TRUE
  } else {
     rxdata$t_mode <<- FALSE
  }
})

output$testing_mode_text <- renderText("Testing mode turns off all app email alerts")
output$update_db_text <- renderText("Use this button after making updates or edits in the BRCWQ Database.")

  observeEvent(input$update_db_rds, {
    source("funs/BRCDB2RDS.R", local = T)
    msg <- MAKE_DB_RDS()
      showModal(modalDialog(
          title = "Database file update ...",
          h4(msg)
      ))
  })

  # Text for Process Data Error or Successful
  process.status2 <- reactive({
    if(input$selectFile != file.processed2()){
      " "
    }else if(inherits(try(dfs_to_import()), "try-error")){
      geterrmessage()
    }else{
      paste0('The file was successfully processed!')
    }
  })

  # Text Output
  output$text.process2.status <- renderText({process.status2()})

  # Show import button and tables when process button is pressed
  # Use of req() later will limit these to only show when process did not create an error)
  # observeEvent(input$process2, {
  #   # req(process.status2() == 'The file was successfully processed!')
  #   show('import')
  #   # show('table.process.wq')
  #   # show('table.process.flag')
  # })

  ### Import Data

  # Import Action Button - Will only be shown when a file is processed successfully
  output$import.UI <- renderUI({
    req(process.status2() == 'The file was successfully processed!')
    actionButton(inputId = "import",
                 label = paste("Import", str_replace(input$selectFile, paste0(submittedDataDir,"/"), ""), "Data"),
                 width = '500px')
  })
########################################################################.
###                             IMPORT DATA                         ####
########################################################################.
  observeEvent(input$import, {
    showModal(busyModal(msg = "Importing data ..."))
    source("funs/processImport.R", local = T)
    out <- tryCatch(IMPORT_DATA(dfs_to_import()),
                    error=function(cond) {
                      import_err <<- paste("Import Failed - There was an error at ", Sys.time(),
                      "...\n ", cond)
                      print(import_err)
                      return(1)
                    },
                    warning=function(cond) {
                      import_err <<- paste("Import process completed with warnings...\n", cond)
                      print(import_err)
                      return(2)
                    },
                    finally={
                      message(paste("Import Process Complete"))
                    }
    )

    if (out == 1){
      removeModal()
      import_msg <<- paste0("Import Failed!  There was a error ... Check log file and review submitted data files and existing database records.")
      showModal(modalDialog(
          title = "Import Failed...",
          h4(import_msg)
        ))
      } else {
      removeModal()
      import_msg <<- paste0("Successful import of '", str_replace(input$selectFile, paste0(submittedDataDir,"/"), ""), "' to the BRCWQDM Database!")
      print(paste0("Data Import Successful at ", Sys.time()))
      print(paste("Data file '", input$selectFile, "' imported."))
      showModal(modalDialog(
          title = "Import Successful...",
          h4(import_msg)
        ))
      NewCount <- ImportActionCount() + 1
      ImportActionCount(NewCount)
      print(paste0("Import Action Count was ", ImportActionCount()))
      ARCHIVE_SUBMITTED_DATA(data_file = input$selectFile)
      print(paste("Data file '", input$selectFile, "' archived."))
      BACKUP_DATABASE()
      ImportEmail()
      loadData()
      loadComments()
      rxdata$data_n_db <- readRDS(data_n_RDS)
      rxdata$data_t_db <- readRDS(data_t_RDS)
      rxdata$data_c_db <- readRDS(data_c_RDS)
      rxdata$fileChoices <- fileChoices()
    }
  })

  ### Function to send ImportEmail
  ImportEmail <- function() {
    out <- tryCatch(
      importEmail(),
      error=function(cond) {
        mail_msg <<- paste("There was an error with the Import email function, cannot send email", cond)
        print(mail_msg)
        return(NA)
      },
      warning=function(cond) {
        mail_msg <<- paste("Import email function caused a warning, but was completed successfully", cond)
        print(mail_msg)
        return(NULL)
      },
      finally={
       message("Import email completed")
      }
    )
    return(out)
  }

  # Hide import button and tables when import button is pressed (So one cannot double import same file)
  observeEvent(input$import, {
    hide('import')
    #hide('table.process.wq')
    #hide('table.process.flag')
  })

  # Create a delayed reactive to trigger input file change update after import
  ### THIS GETS TRIGGERED TOO QUICKLY - NEEDS TO TRIGGER AFTER INPUT$IMPORT IS CLICKED, NOT ON A TIMER
  # import.delay <- reactive({
  #   # Delay reactive invalidation (in milliseconds)
  #   invalidateLater(10000, session)
  #   input$import
  # })

  # Add text everytime successful import
  # observeEvent(input$import, {
  #   insertUI(
  #     selector = "#import",
  #     where = "afterEnd",
  #     ui = h4(import_msg)
  #   )
  # })
### REPORTS ####

  observeEvent(input$email_test,{
    try(testEmail())
  })

### INSTRUCTIONS ####

  output$instructions <- renderUI({
    includeHTML("www/instructions.html")
  })

 ### BUSY MODAL ####
 busyModal <- function(msg){
    modalDialog(
      size = "s",
      fluidPage(
        useShinyjs(),
        includeCSS("www/animate.min.css"),
        includeCSS("www/animate.css"),
        h2(class = "animated infinite pulse", msg)
        )
    )
  }

### MODULE CALLS ####
callModule(ADD_COMMENT, "add_comment_physical",
           input_section = "physical",
           site = selected_site,
           comment_date = selected_date,
           sampler = selected_sampler,
           formatted_sampler = samplersRX)

callModule(ADD_COMMENT, "add_comment_depth",
           input_section = "depth",
           site = selected_site,
           comment_date = selected_date,
           sampler = selected_sampler,
           formatted_sampler = samplersRX)

callModule(ADD_COMMENT, "add_comment_chemical",
           input_section = "chemical",
           site = selected_site,
           comment_date = selected_date,
           sampler = selected_sampler,
           formatted_sampler = samplersRX)

callModule(ADD_COMMENT, "add_comment_biological",
           input_section = c("biological"),
           site = selected_site,
           comment_date = selected_date,
           sampler = selected_sampler,
           formatted_sampler = samplersRX)

callModule(ADD_COMMENT, "add_comment_other",
           input_section = c("chemical","physical","depth","biological"),
           site = selected_site,
           comment_date = selected_date,
           sampler = selected_sampler,
           formatted_sampler = samplersRX)

callModule(BRCMAP, "brc_map", sitelist = sites_db)

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
  output$brc_logo6 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo6 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)
  output$brc_logo7 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo7 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)
  output$brc_logo8 <- renderImage({list(src = "www/BRC_logo_River.jpg", width= "160", height= "80")}, deleteFile = FALSE)
  output$zap_logo8 <- renderImage({list(src = "www/zap_logo.gif", width= "76", height= "59")}, deleteFile = FALSE)

### SESSION END ####
# Code to stop app when browser session window closes
session$onSessionEnded(function() {
      if(user_role != "App Developer"){
         UPLOAD_LOG()
      }

      print(paste0("BRCWQDM session ended at ", Sys.time()))
      stopApp()
    })

} # END SERVER FUNCTION ####

shinyApp(ui = ui, server = server)

