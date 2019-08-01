### COMMENT MODULE ####


ADD_COMMENT_UI <- function(id){
    ns <- NS(id)

########################################################################.
###                       MODULE UI                                ####
########################################################################.
 actionButton(ns("add_comment_mod"), "Add Comment")
}


########################################################################.
###                       MODULE SERVER                             ####
########################################################################.
ADD_COMMENT <- function(input, output, session, input_section, site, comment_date, sampler) {
  ### input_section refers to the data inputs: physical, chemical, depth
  comm_choices <-  c("General Comment", table_fields$dt_cols[table_fields$take_comments =="yes" & table_fields$input_section == input_section])

  observeEvent(input$add_comment_mod, {
    ns <- session$ns
    # req(input$site, SampleDT())
    if(site == ""| is.null(comment_date) | is.null(sampler())){
      shinyalert("Oops!", "A site and Date must be selected to add a comment!.", type = "error")
    } else {
      showModal(modalDialog(
        h3("ADD COMMENT..."),
        "Add comment related to specific parameter or a general comment that applies to this sampling event.
           You will be listed as the commenter, unless the box is checked to indicate that the comment is from the Field Monitor",
        selectInput(ns("comment_par"), "Parameter Reference:", choices = comm_choices),
        textAreaInput(ns("comment_text"), label = NULL, placeholder = "Add comment here...", width = "100%"),
        checkboxInput(ns("FM_comment"), "This is a comment from Field Monitor", value = F),
        actionButton(ns("save_comment"), "Save Comment"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    }
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
    commenter <- sampler()
  } else {
    commenter <- app_user
  }
  comment <- tibble(SITE = site, DATE = comment_date, PARAMETER = input$comment_par,
                    COMMENTER = commenter, COMMENT_TEXT = input$comment_text)
  # glimpse(comment)
  return(comment)
}

observeEvent(input$save_comment,{
  saveComment(data = formatComment(), csvFile = stagedCommentsCSV, rdsFile = stagedCommentsRDS)
  shinyalert(title = "Comment Entered!", type = "success")
  removeModal()
})

}