### PHOTO MODULE ####
library("fs")

ADD_PHOTO_UI <- function(id){
    ns <- NS(id)

########################################################################.
###                       MODULE UI                                ####
########################################################################.
 actionButton(ns("add_photo_mod"), "Add Photo", icon = icon("camera", lib="glyphicon"))
}

########################################################################.
###                       MODULE SERVER                             ####
########################################################################.
# places to add photos:
#1) Data Entry by Field coordinator - Based on Site, Date - Parameter must be selected
#2) Staged Data by Field coordinator - Based on Site, Date - Parameter must be selected
#3) Submitted Data by Program coordinator - Based on Site, Date - Parameter must be selected
#4) After Data is in DB: This has SEID and Sample ID assigned - Parameter is fixed, or have radio button to override as general comment

# Module workflow:
# Add photo button is activated - modal opens
# Site and Date are populated based on inputs selected or rows selected
# Parameter choice is a dropdown - except for data already in DB - Parameter is fixed or General (override radio button)
# Error handle - stop modal and warn if no Site/Date on data entry page, cancel at bottom
# User Selects parameter, enters photographer name, caption is optional
# User uses a file browse widget to locate the file
# Photo gets assigned filename and a copy is made to localprojectdir
# Photo is pushed to dropbox photo repository
# Photo data record is appended to google sheets table

ADD_PHOTO <- function(input, output, session, site, photo_date, mod_loc, par = NULL) {
### Function args are brought in from outside of module scope... par only from data
par_choices <-  c("General", table_fields$dt_cols[table_fields$take_comments =="yes"])

### Module locations: mod_loc:
#  1. "data entry"
#  2. "staged data"
#  3. "submitted data"
#  4. "imported data"

roots <- c(wd = dataDir)
shinyFileChoose(input, 'photo_path', roots = roots, filetypes = c('png', 'jpg', 'img'))

observeEvent(input$add_photo_mod, {
  ns <- session$ns

  # if(mod_loc != "data entry") {
    if(length(site()) > 0) {
      if(any(site() == "", is.null(photo_date()))){
        shinyalert("Oops!", "A site and date must be defined in the record to add a photo!.", type = "error")
      } else {
        showModal(modalDialog(
          h3("ADD PHOTO..."),
          "Add a photo related to a specific parameter or a general photo that applies to this sampling event.
        The photo file will be uploaded to cloud storage and available through this app.",
          br(),
          strong(glue("Site: {names(which(sites == site()))}")),
          br(),
          selectInput(ns("photo_par"), "Parameter Reference:", choices = par_choices),
          textInput(ns("photographer"), label = "Photographer:", width = "100%"),
          textAreaInput(ns("photo_text"), label = NULL, placeholder = "Add descriptive caption or tags here:", width = "400px"),
          tagList(
            strong("Add photo file to ~LocalProjectDir/data folder before browsing..."),
            shinyFilesButton(
              id = ns("photo_path"),
              label = "Click to browse and select photo to upload (only png, jpg, or img)",
              title = "",
              multiple = FALSE,
              buttonType = "default",
              class = NULL),
            br(),
            strong(glue("Selected File:")),
            verbatimTextOutput(ns("photo_path_text"))
          ),
          br(),
          actionButton(ns("upload_photo"), label = "Upload Photo", icon = icon(name = "camera", lib = "font-awesome")),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel")
          )
        ))
      }
    } else {
      shinyalert("Oops!", "Click a record in the table to add a photo!", type = "error")
    }
  # }
})

  photopath <- reactive({
     parseFilePaths(roots, input$photo_path) %>% select(datapath) %>% pull(1)
  })

  photodir <- reactive({
         parseDirPath(roots, input$photo_path)
  })

  photoname <- reactive({
         parseDirPath(roots, input$photo_path) %>% select(name) %>% pull(1)
  })

  output$photo_path_text <- renderText({
    req(input$photo_path)
    photopath()
  })

  if (is.null(par)) {
    par <- reactive ({input$photo_par})
  } else {
    par <- reactive({par})
  }

observeEvent(input$upload_photo,{
  if(!isTruthy(photodir())) {
    shinyalert("Oops!", "You need to select a photo before you can upload it!", type = "error")
  } else {
    uploadPhoto(file = photopath())
    shinyalert(title = "Photo Uploaded!", type = "success")
    removeModal()
  }
})

 # file <- paste0(dataDir, "photos/A-07-08-010_2020-07-23_WATD_1595547321.jpg")
 # file
uploadPhoto <- function(file) {
  print(paste0("Photo File is: ", file))
  if(file.exists(file)){
    # Upload the photo file to Dropbox
    if(par() == "General") {
      new_name <- glue("{site()}_{photo_date()}_GEN_{format(now(),'%s')}.{stringr::str_sub(file, start = -3, end = -1)}")
    } else {
      new_name <- glue("{site()}_{photo_date()}_{parameters_db$ABBRV[parameters_db$PARAMETER_NAME == par()]}_{format(now(),'%s')}.{stringr::str_sub(file, start = -3, end = -1)}")
    }
    fullname <- glue("{dataDir}/photos/{new_name}")
    print(paste0("Photo renamed to: ", new_name))
    photo_rec <- tibble(SITE_CODE = site(),
                      DATE = photo_date(),
                      PARAMETER = par(),
                      PHOTAGRAPHER = str_trunc(input$photographer, width = 61, side = "right"),
                      FILENAME = new_name,
                      CAPTION = str_trunc(input$photo_text, width = 61, side = "right"),
                      ADDED_BY = app_user)
    # Add the photo to dropbox
    UPLOAD_PHOTO(file = file, name = fullname)
    # Append the photo record to the Google sheets doc
    GS_APPEND_PHOTO(sheet = config[14], data = photo_rec)
  } else {
    stop("The file does not exist! Please choose another file.")
  }
}

}