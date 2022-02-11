################################### HEADER ###################################
#  TITLE: mod_photo_viewer
#  DESCRIPTION: A module to view photos in shiny
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED:
#  GIT REPO: BRCWQDM
#  R version 3.6.3 (2020-02-29)  x86_64
########################################################################.
    # UI
    # Photo Browser -
    # Table to show filtered selection - button to retrieve photo
    # Keep photos in photo folder - if already available locally, then don't re-download
    # Warn users that photos were downloaded to computer and must be removed manually if desired.
    # Photos may be for sample events that have not yet been imported to database.

########################################################################.
###                       MODULE UI                                ####
########################################################################.

PHOTOS_UI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(
      id = ns('form'),
      fluidPage(
        wellPanel(
          fluidRow(
            column(8,
                   strong("Photos - Select a photo in the table below and click the 'View Photo' button. Use the column filters to help find a photo of interest"),
                   br(),
                   em("Please note: All photos viewed will be downloaded if not already in photos directory (~/LocalProjectDir/Data/photos), Photos will remain in photos directory and must be deleted manually if no longer wanted")
            ),
            column(4,
                   actionButton(ns("get_photo"), label = "View Photo", icon = icon("camera", lib = "glyphicon")),
                   textOutput(ns("selected_photo"))
            )
          )
        ),# End Well Panel
        br(),
        DT::dataTableOutput(ns("photos"))
      ) # End Fluid Page
    ) # Close Div
  ) # End tagList
}

########################################################################.
###                       Server Function                           ####
########################################################################.

PHOTOS <- function(input, output, session, photo_list, globalSession, mode = reactive(2)) {
ns <- session$ns
photo_recs <- reactive(photo_list())
local_photo_dir <- paste0(dataDir,"photos")

photo_dt <- reactive({
    d <- photo_recs() %>%
      mutate(SITE_CODE = paste0(sites_db$WATERBODY_NAME[match(SITE_CODE, sites_db$BRC_CODE)], " (", SITE_CODE, ")")) %>%
      dplyr::rename("SITE" = "SITE_CODE")
    d$DATE <- force_tz(d$DATE, tzone = "America/New_York")
    d
})

output$photos <- DT::renderDataTable({
 req(isTruthy(photo_dt()))
 datatable(photo_dt(), filter = "top", selection = 'single', rownames = F) %>%
    formatDate(columns = "DATE", method = 'toLocaleDateString')
})

browser_photo_loc <- reactive({
  photo_dt()[input$photos_rows_selected, 1]
})

browser_photo_date <- reactive({
  photo_dt()[input$photos_rows_selected, 2]
})

browser_photo_parameter <- reactive({
  photo_dt()[input$photos_rows_selected, 3]
})

browser_photographer <- reactive({
  photo_dt()[input$photos_rows_selected, 4]
})

rec <- reactive({
  photo_dt()[input$photos_rows_selected, 5]
})

browser_caption <- reactive({
  photo_dt()[input$photos_rows_selected, 6]
})

photo <- reactive({
  req(rec())
  paste0(local_photo_dir, "/", rec())
})

observeEvent(input$get_photo, {
     ns <- session$ns
     if(!length(rec())) {
       shinyalert("Oops!", "Please select a record in order to view a photo!", type = "error")
     } else {
       local_imgs <- list.files(local_photo_dir, pattern=".png|.jpg|.img", full.names = FALSE)
       if(!photo() %in% local_imgs) {
         try(GET_PHOTO(photo = rec()) # Add more error handling here - shouldn't trigger error, unless files removed from dropbox
         )
       }
       if (file.exists(photo())) {
         showModal(
           modalDialog(
             fluidPage(
               column(7,
                      h3(rec()),
                      imageOutput(ns("photo_output"))
               ),
               column(5,
                      h4(glue("Photo Date: {browser_photo_date()}")),
                      br(),
                      h4(glue("Photo Location: {browser_photo_loc()}")),
                      br(),
                      h4(glue("Parameter: {browser_photo_parameter()}")),
                      br(),
                      h4(glue("Photographer: {browser_photographer()}")),
                      br(),
                      h4(glue("Caption/tags: {browser_caption()}"))
               )
             ), # end Fluid page

             # actionButton(ns("close1"), "Close (keep photo)"), ### possible future enhancement - allow two close methods - 1 will delete photo file
             # actionButton(ns("close2"), "Close (delete photo)"),
             title = "BRC Photo Viewer", size = "l",
             easyClose = TRUE,
             fade = TRUE
           )
         )
       } else {
         showModal(
           modalDialog(
             h3(paste0("The photo with file name: '", rec(), "' was not found in the photos folder. Check with database admin to verify photo is available from dropbox." )),
             title = Error, easyClose = T, size = "s")
           )
       }
     }
}, ignoreInit = TRUE)

output$selected_photo <- renderText({
  req(rec())
  paste0("Photo File: ", rec())
})

# A better idea might be to have the table with all photos and a button to open the image for the selected row. Photo could
# open up in a pop-up window. Should we delete the photo when session ends?

output$photo_output <- renderImage({
  # Return a list
  list(src = photo(),
       width = "600px",
       alt = rec())
}, deleteFile = FALSE)


}