### TRAINING LOG MODULE ####

TLOG_UI <- function(id) {
  ns <- NS(id)

  ########################################################################.
  ###                       MODULE UI                                ####
  ########################################################################.
  fluidPage(
    fluidRow(actionButton(ns("add_tlog"), "Add Training Log Entry", icon = icon("glyphicon-list-alt", lib="glyphicon")),
             br(),
             DTOutput(ns("training_log"))
    )
  )
}

########################################################################.
###                       MODULE SERVER                             ####
########################################################################.

# Module workflow:
#1) Action button to add log entry and the log table are the only 2 elements
#2) The button opens a modal dialog that lets the user add the log record
#3) The record is saved to google sheets
#4) The rxdata$tlog element is refreshed with the new entry (running append tlog)

TLOG <- function(input, output, session, people, samplers, train_log, globalSession, mode = reactive(2)) {
### Training log table from google sheets
rx_training_log <- reactive(train_log())

output$training_log <- DT::renderDataTable({
  req(isTruthy(rx_training_log()))
  datatable(rx_training_log(), filter = "top", selection = 'single', rownames = F) %>%
    formatDate(columns = "ACTIVITY_DATE", method = 'toLocaleDateString')
})

samplers2 <- reactive(samplers())
### Function args are brought in from outside of module scope... par only from data
trainee_choices <-  reactive({
  c(people, samplers2()) %>% unique() %>% sort()
})

observeEvent(input$add_tlog, {
  ns <- session$ns
  showModal(modalDialog(
    h3("ADD TRAINING LOG RECORD ..."),
    "Add a training log record. These records cannot be edited once submitted. If changes are needed contact
    the BRC Program coordinator or the app developer.",
    br(),
    hr(),
    br(),
    selectInput(ns("trainee"), "Choose a trainee:", choices = trainee_choices()),
    selectInput(ns("training_type"), "Type of Training:", choices = c("Sampling training",
                                                                      "Sampling refresher",
                                                                      "Lab training",
                                                                      "Lab refresher",
                                                                      "Sampling Training Audit",
                                                                      "Lab Training Audit", "Other")),
    dateInput(ns("training_date"), "Date of training/audit:", startview = "year"),
    textAreaInput(ns("training_notes"), label = "Training Notes:"),
    br(),
    actionButton(ns("upload_log"), label = "Save Log", icon = icon(name = "cloud-upload", lib = "font-awesome")),
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel")
    )
  ))
})

observeEvent(input$upload_log, {
  print("Uploading training record to google sheets...")
  training_rec <- tibble(NAME = input$trainee,
                         TRAINING_TYPE = input$training_type,
                         ACTIVITY_DATE = input$training_date,
                         NOTES = input$training_notes,
                         ADDED_BY = app_user)

  # Append the photo record to the Google sheets doc
  GS_APPEND_TRAINING_LOG(sheet = config[16], data = training_rec)
  shinyalert(title = "Training log record saved successfully!", type = "success")
  removeModal()
})
} # End server