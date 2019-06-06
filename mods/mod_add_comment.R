### COMMENT MODULE ####


ADD_COMMENT_UI <- function(id){
  
  ### COMMENT MODAL ####
  
  observeEvent(input$add_comment, {
    # req(input$site, SampleDT())
    if(input$site == ""| is.null(SampleDT())){
      shinyalert("Oops!", "A site and Date must be selected to add a comment!.", type = "error")
    } else {
      showModal(modalDialog(
        h3("ADD COMMENT..."),
        "Add comment related to specific parameter or a general comment that applies to this sampling event.
           You will be listed as the commenter, unless the box is checked to indicate that the comment is from the Field Monitor",
        selectInput("comment_par", "Parameter Reference:", choices = c("General Comment", table_fields$dt_cols[table_fields$take_comments =="yes"])),
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
  
  
  
  
  
  
  
}