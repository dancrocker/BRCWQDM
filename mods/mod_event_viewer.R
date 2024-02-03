################################### HEADER ###################################
#  TITLE: mod_BRC_event_viewer
#  DESCRIPTION: Module plugin to view database data pulled together by SEID
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: April 2020
#  GIT REPO: BRCWQDM
#  R version 3.6.3 (2020-02-29)  x86_64
##############################################################################.

library("dplyr")
library("stringr")

########################################################################.
###                             MODULE UI                           ####
########################################################################.

### Well Panel 1 ####
### Filter events ####
# text input (numeric) for SEID
# Select input for Location
# Select input for available dates (as a list)- F = YYYY-MM-DD - this will allow some filtering if lots of dates
# A button to see event data

EVENTS_UI <- function(id) {

ns <- NS(id)

fluidPage(
  titlePanel("BRC Data Viewer"),

  sidebarLayout(
    sidebarPanel(
      em("Select Event using Location and Date"),
      selectInput(ns("site"), "Choose Sample Location:", c("", sites), selected = ""),
      uiOutput(ns("date_ui")),
      br(),
      em("Select Event using Sample Event #"),
      fluidRow(
        column(6,
               numericInput(inputId = ns("seid"), label = "Sample Event #", value = 1, min = 1,
                            max = max(trans_log_db$SEID), step = 1, width = 200)
        ),
        column(6,
               checkboxInput(ns("use_id"), label = "Use Sample Event ID:", value = FALSE)
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Event Summary",
                 h3(textOutput(ns("event_header"))),
                 br(),
                 fluidRow(
                   column(6,
                          em("PHYSICAL DATA RESULTS:"),
                          tableOutput(ns("event_phys_num")),
                          tableOutput(ns("event_phys_text"))
                   ),
                   column(6,
                          em("CHEMICAL DATA RESULTS"),
                          tableOutput(ns("event_chem_num")),
                          br(),
                          em("BIOLOGICAL DATA RESULTS:"),
                          tableOutput(ns("event_bio_num"))
                   )
                 ),
                 fluidRow(
                   column(12,
                          em("EVENT COMMENTS"),
                          tableOutput(ns("event_comments"))
                   )
                 )
        ),
        tabPanel("Event Photos",
                 column(12,
                        PHOTOS_UI(ns("event_photos"))
                 )
        )
        # tabPanel("Plot Data", plotOutput("plot")),
        # tabPanel("Tabular Data", tableOutput("table"))
      )
    )
  )
)

} # End UI

########################################################################.
###                         MODULE SERVER                          ####
########################################################################.

EVENTS <- function(input, output, session, photo_list, globalSession, mode = reactive(2)) {

ns <- session$ns

# Pass rxdata from modules:
data_n_db <- readRDS(data_n_RDS) %>% filter(!is.na(SEID))
data_t_db <- readRDS(data_t_RDS)
data_c_db <- readRDS(data_c_RDS)
data_trans_log_db <- readRDS(trans_log_RDS)
params_db <-  readRDS(parametersRDS)
photo_recs <- reactive(photo_list())
### Placeholder for photos
### The photo list needs to combine the gsheets records with the database records
# photos_db <- readRDS(photos_list)# These is the database photos
# photos)_gs <- isolate(rxdata$photos)

# Select SEID Directly (text input)
# or Select Location (select input), which then updates a select input of sample dates
# Go button to pull records
# Server Logic:
# When input Go is clicked:
# If SEID input is blank is not null, set SEID, if null, go to Location and date selection
   # Default to first location and date selected so that no error can occur, add error handling if no valid selection provided

# A. Use a known SEID:

seid_selected <- reactive({
  # req(input$seid | input$date)
  if(input$use_id) {
     input$seid
  } else {
    data_n_db$SEID[data_n_db$SITE_BRC_CODE == input$site & str_trunc(data_n_db$DATE_TIME, side = "right", width = 10, ellipsis = "") == input$choose_date][1] %>% as.numeric()
  }
})

# Use location and date to fetch seid
# location <- "B-02-01-055"
# sample_day <- "2019-09-14"

# seid <- input$seid
# Get SEID from location and date

### Heading info
# event_id <- seid_selected()
event_site_code <- reactive({
  data_n_db$SITE_BRC_CODE[data_n_db$SEID == seid_selected()][1]
  })
event_date_time <- reactive({
  data_n_db$DATE_TIME[data_n_db$SEID == seid_selected()][1]
  })
event_sampler <- reactive({
  data_t_db()$RESULT[data_t_db$PARAMETER == "Sampler" & data_n_db$SEID == seid_selected()][1]
  })

# Cull data down to
event_data_n <- reactive({
  data_n_db %>%
    filter(SEID == seid_selected()) %>%
    select(c(1,5:7))
})
event_data_t <- reactive({
  data_t_db %>%
    filter(SEID == seid_selected()) %>%
    select(c(1,5:6))
})
event_data_c <- reactive({
  data_c_db %>%
    filter(SEID == seid_selected()) %>%
    select(-SEID)
})

event_data_trans_log <- reactive({
  trans_log_db %>%
    filter(SEID == seid_selected()) %>%
    select(-SEID)
})
### Reduce further for table display - group by parameter type ? Include import date or imported by?
# numeric_data <- data_n[,c(1,5:7)] %>% arrange(PARAMETER)
# text_data <- data_t[,c(1,5:6)] %>% arrange(PARAMETER)
# comment_data <- data_c %>% select(-SEID)
# photos (pending implementation)

# Group into categories:

# Physical Cat = PHYSICAL
num_phys <- reactive({
  event_data_n()[event_data_n()$PARAMETER %in% params_db$PARAMETER_NAME[params_db$CATEGORY == "Physical"],]
})
text_phys <- reactive({
  event_data_t()[event_data_t()$PARAMETER %in% params_db$PARAMETER_NAME[params_db$CATEGORY == "Physical"],]
})
# Chemical CAT = CHEMICAL
num_chem <- reactive({
  event_data_n()[event_data_n()$PARAMETER %in% params_db$PARAMETER_NAME[params_db$CATEGORY == "Chemical"],]
})
# Biological CAT = BIOLOGICAL
num_bio <- reactive({
  event_data_n()[event_data_n()$PARAMETER %in% params_db$PARAMETER_NAME[params_db$CATEGORY == "Biological"],]
})

# Photos CAT = BOOLEAN (pending) or just photos
event_photos <- reactive({
  photo_recs() %>%
  filter(SITE_CODE == event_site_code(), DATE == as.character(as_date(ymd_hm(event_date_time()))))
})
### REACTIVE OUTPUTS ####
### Date UI ####

date_choices <- reactive({
  req(input$site != "")
  data_n_db %>% filter(SITE_BRC_CODE == input$site) %>%
    pull(DATE_TIME) %>%
    ymd_hm() %>%
    as_date() %>%
    paste0() %>%
    unique() %>%
    sort()
})

output$date_ui <- renderUI({
  req(date_choices())
  selectInput(ns("choose_date"), "Sample Date:", choices = date_choices(), multiple = FALSE)
})

output$event_header <- renderText({
  glue("DATA FOR SITE {event_site_code()} AT {event_date_time()} (SAMPLE EVENT #: {seid_selected()})")
})

output$event_phys_num = renderTable({
    num_phys()
})
output$event_phys_text = renderTable(
    text_phys(), colnames = FALSE
    )
output$event_chem_num = renderTable({
    num_chem()
})
output$event_bio_num = renderTable(
    num_bio(), colnames = TRUE
)
output$event_comments = renderTable(
     event_data_c(), colnames = TRUE
)

callModule(PHOTOS, "event_photos", photo_list = reactive(event_photos()))
########################################################################.
###                       Plot Data at this Location                ####
########################################################################.

# Need a select input to choose the parameter
# This will make a scatterplot showing all data for this location/parameter
# Make it an interactive dyegraph so user can hover on points etc
# Try to highlight the point for this SEID
# Execute in a modal box launched from a button?

} # End Server



