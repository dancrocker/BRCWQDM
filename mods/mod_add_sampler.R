################################### HEADER ###################################
#  TITLE: mod_add_sampler.R
#  DESCRIPTION: Module to add samplers to google sheet
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: March 30, 2021
#  GIT REPO: BRCWQDM
#  R version 3.6.3 (2020-02-29)  x86_64
##############################################################################.

#1) This module is only available on the data entry page
#2) People/volunteers can also be added directly to the google sheet - this is a good option
#3) This data must be consistently formatted QC'd and added to the database eventually

# ### SAMPLER MODULE ####
# # What needs to happen:
# 1. Click add sampler button:
# 2. Modal popup opens
# 3. User enters info about new samplers
#     Full Name (First and Last)* required
#     Site Assignment (drop down pick list) - optional
#     Primary or fill-in
#     Year = this year - automatically added to the record
#     Role = Field - automatically added to the record
#     Then all the fields from the people table?
#     Timestamp - for record keeping
#     Added_By - for record keeping


ADD_SAMPLER_UI <- function(id){
    ns <- NS(id)

########################################################################.
###                       MODULE UI                                ####
########################################################################.
 actionButton(ns("add_sampler_mod"), "Add Sampler", icon = icon(name = "user-edit", lib="font-awesome"))
}

########################################################################.
###                       MODULE SERVER                             ####
########################################################################.
ADD_SAMPLER <- function(input, output, session, sitelist) {
### Function args are brought in from outside of module scope... par only from data
site_assignment_choices <-  c("Floater", sitelist)

observeEvent(input$add_sampler_mod, {
  ns <- session$ns
  showModal(modalDialog(
    h3("ADD SAMPLER..."),
    "Add someone to the list of samplers. Please fill in as much information as you have at this time.
    Any unknown information should be requested from the sampler and can be added directly to the google sheet at a later time.",
    br(),
    textInput(ns("full_name"), "Sampler Full Name:"),
    selectInput(ns("site"), "Site Assignment (optional)", choices = site_assignment_choices),
    radioButtons(ns("sampler_type"), "Sampler Type:", c("Primary",  "Secondary", "Fill-in"), inline = TRUE),
    textInput(ns("address_1"), label = "Address Line 1:", width = "100%", placeholder = "Street Number, Street Name"),
    textInput(ns("address_2"), label = "Address Line 2:", width = "100%", placeholder = "Apt#, Unit, etc."),
    textInput(ns("city"), "City"),
    selectInput(ns("state"), label = "State", choices = c("MA", "RI")),
    textInput(ns("zip"), "Zip Code"),
    textInput(ns("phone_cell"), "Cell Phone", placeholder = "XXX-XXX-XXXX"),
    textInput(ns("phone_home"), "Home Phone", placeholder = "XXX-XXX-XXXX"),
    textInput(ns("email"), "Email"),
    textInput(ns("emerg_cont_name"), "Emergency Contact Name"),
    textInput(ns("emerg_cont_phone"), "Emergency Contact Phone/cell", placeholder ="XXX-XXX-XXXX"),
    textInput(ns("emerg_cont_relation"), "Emergency Contact Relationship"),
    textAreaInput(ns("sampler_notes"), label = NULL, placeholder = "Other important info...", width = "400px"),
    br(),
    actionButton(ns("save_sampler"), label = "Save Sampler", icon = icon(name = "cloud-upload", lib = "font-awesome")),
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel")
    )
    ))
})

observeEvent(input$save_sampler,{
  if(!isTruthy(input$full_name)) {
    shinyalert("Oops!", "You need to add a name for the sampler!", type = "error")
  } else {
    sampler_rec <- tibble(FULL_NAME = input$full_name,
                          SITE_ASSIGNMENT = input$site,
                          SAMPLER_TYPE = input$sampler_type,
                          YEAR = year(Sys.Date()),
                          ADDRESS_1 = input$address_1,
                          ADDRESS_2 = input$address_2,
                          CITY= input$city,
                          STATE  = input$state,
                          ZIP_CODE  = input$zip,
                          PHONE_CELL = input$phone_cell,
                          PHONE_HOME = input$phone_home,
                          EMAIL = input$email,
                          EMERG_CONT_NAME = input$emerg_cont_name,
                          EMERG_CONT_PHONE = input$emerg_cont_phone,
                          EMERG_CONT_RELATIONSHIP = input$emerg_cont_relation,
                          NOTES = input$sampler_notes,
                          TIMESTAMP = Sys.time(),
                          ADDED_BY = app_user)

# Append the sampler record to the Google sheets doc
    GS_APPEND_SAMPLER(sheet = config[15], data = sampler_rec)
    shinyalert(title = "Sampler Saved!", type = "success")
    removeModal()
  }
})

}

### To Do
# 1. Write script that will pull entries from gsheets and add to people and assignments tables in sqlite db
# 2.