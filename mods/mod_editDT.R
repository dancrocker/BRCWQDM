### EDIT MODULE ####

#### Important notes:

# This module setup will work for reactive df
### TO DO:
# Test module with form data and make sure form additions and the edit features modify table
# Somehow remove or restrict the columns that show up so that Entered By
# Either change action on edit modal so that update = Save or put a separate Save button above DT
# Trigger update of csv file and RDS file on update - move
# Write csv to RDS conversion for comments df

### MODULE UI ####

editableDTUI <- function(id) {
  ns=NS(id)
  tagList(
  fluidPage(
    shinyjs::useShinyjs(),
    includeScript("www/message-handler.js"),
    fluidRow(
      uiOutput(ns("buttons")),
      # radioButtons3(ns("selection"),"Data Selection",choices=c("single","multiple"),
      #               inline=TRUE,labelwidth=130,align="center"),
      # radioButtons3(ns("resultAs"),"Resultant Data as",choices=c("tibble","data.frame"),inline=TRUE,labelwidth=150,align="center"),
      p(""),
      DT::DTOutput(ns("origTable")),
      conditionalPanel(condition="true==false",
                       numericInput(ns("width2"),"width2",value=100),
                       textInput(ns("result"),"result",value=""),
                       numericInput(ns("no"),"no",value=1),
                       numericInput(ns("page"),"page",value=1),
                       checkboxInput(ns("resetPage"),"resetPage",value=FALSE),
                       tags$head(tags$script(src = "message-handler.js")),
                       tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#origTable'+id).find('table').DataTable().table().node());
        })"))

      ),
      verbatimTextOutput(ns("record"))
    )
  )
)
}

### EDIT TABLE FUNCTION ####

editableDT <- function(input, output, session,
                       data=reactive(NULL),
                       globalSession,
                       data_name,
                       inputwidth=reactive(100),
                       edit_cols,
                       mode=reactive(2)) {


  deleted<-deleted1<-edited<-edited1<-added<-added1<-updated1<-updated<-restored<-restored1<-c()

  defaultlen=20

  # observe({
  #   updateTextInput(session,"result",value=dataname())
  # })

  observe({
    updateNumericInput(session,"width2",value=inputwidth())
  })


  df <- reactive({
      if(is.null(input$result)){
        df <- data()
      } else if(input$result!="") {
        df <- eval(parse(text=input$result))
      }
    else df <- data()
    ### BOOKMARK ####
    df
  })


#   observe({
#     enable_comment <- nrow(df()) > 0
#       print(enable_comment)
#     # enable/disable the comment button
#       if(nrow(df()) > 0){
#     # shinyjs::toggleState(id = "add_comment_mod", condition = enable_comment)
#       enable("add_comment_mod")
#     }
# })


  output$buttons <-renderUI({
    ns <- session$ns
    amode=mode()
    tagList(
      actionButton(ns("delRow"),"Delete Row",icon=icon("remove",lib="glyphicon")),
      # actionButton(ns("addRow"),"Add New",icon=icon("plus",lib="glyphicon")),
      # actionButton(ns("insertRow"),"Insert Row",icon=icon("hand-up",lib="glyphicon")),
      actionButton(ns("editData"),"Edit Data", icon=icon("wrench",lib="glyphicon")),
     if(data_name %in% c("stagedData", "submittedData")){
       # if(nrow(df() >0)){
      # disabled(actionButton(ns("add_comment_mod"), "Add Comment", icon=icon("pencil",lib="glyphicon")))
       # } else {
      actionButton(ns("add_comment_mod"), "Add Comment", icon=icon("pencil",lib="glyphicon"))
       # }
     }
      # if(amode==1) actionButton(ns("newCol"),"New Col",icon=icon("plus-sign",lib="glyphicon")),
      # if(amode==1) actionButton(ns("removeCol"),"Remove Col",icon=icon("trash",lib="glyphicon")),
      # if(amode==1) actionButton(ns("dplyr"),"Manipulate",icon=icon("scissors",lib="glyphicon")),
      # if(amode==2) actionButton(ns("reset"),"Reset",icon=icon("remove-sign",lib="glyphicon")),
      # if(amode==2) actionButton(ns("restore"),"Restore",icon=icon("heart",lib="glyphicon"))
    )
  })

  observe({
        toggleState(id = "delRow", condition = nrow(df()[input$origTable]) > 0)
      })


  output$origTable <- DT::renderDT({
    # if(dataname()!=""){
    #   validate(
    #     need(any(class(try(eval(parse(text=input$result)))) %in% c("tbl_df","tibble","data.frame")),
    #          "Please enter the valid data name")
    #   )
    # }
    updateCheckboxInput(session,"resetPage",value=TRUE)
    datatable(
      df(),
      selection = "single",
      editable=FALSE,
      caption = NULL,
      options = list(
        lengthMenu = list(c(5, 25, 50, 100), c('5', '25', '50', '100')),
        pageLength = 10)
    )
  })



  # commenter_choices <- app_user

  # print the selected indices
  output$record = renderPrint({
    # t(df()[input$origTable_rows_selected,])
    s <- df()[input$origTable_rows_selected,]
    if (length(s)) {
      cat('DATA FOR THE SELECTED ROW:\n\n')
      write.table(t(df()[input$origTable_rows_selected,]),
                  row.names = paste0(names(df()), ": "), quote = FALSE, col.names = FALSE)
    }
  })


  ########################################################################.
  ###                     ADD A COMMENT                              ####
  ########################################################################.
  #  A button that triggers a modal dialog - for adding a comment
  # A DT row must be selected... The selected row will fill in the site and the Date
  # The comment button can only be on the data tabs - not the comment tabs
observeEvent(input$add_comment_mod, {
    ns <- session$ns
     # This comment button is for adding comments to staged data
        # User must select a row from the table to add a comment
    ids <- input$origTable_rows_selected
    # s <- df()[input$origTable_rows_selected,]
    # print(s)
    # print(class(s))
     if(length(ids) > 0){
      showModal(modalDialog(
        h3("ADD COMMENT..."),
        "Add comment related to specific parameter or a general comment that applies to this sampling event.
           You will be listed as the commenter, unless the box is checked to indicate that the comment is from the Field Monitor",
        selectInput(ns("comment_par"), "Parameter Reference:", comment_par_choices),
        textAreaInput(ns("comment_text"), label = NULL, placeholder = "Add comment here...", width = "100%"),
        checkboxInput(ns("FM_comment"), "This is a comment from Field Monitor", value = F),
        actionButton(ns("save_comment"), "Save Comment"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel")
      )
      ))
    } else {
      shinyalert("Oops!", "Click a record in the table to add a comment!", type = "error")
    }
   })

    saveComment_mod <- function(data, csvFile, rdsFile) {
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

formatComment_mod <- function(){
  if(input$FM_comment == TRUE){
    commenter <- switch(data_name,
              "stagedData" = df()[input$origTable_rows_selected, 4],
              "stagedComments" = df()[input$origTable_rows_selected, 5],
              "submittedData" = df()[input$origTable_rows_selected, 4],
              "submittedComments" = df()[input$origTable_rows_selected, 5]
    )
  } else {
    commenter <- app_user
  }
  site_comm <- switch(data_name,
              "stagedData" = df()[input$origTable_rows_selected, 3],
              "stagedComments" = df()[input$origTable_rows_selected, 1],
              "submittedData" = df()[input$origTable_rows_selected, 3],
              "submittedComments" = df()[input$origTable_rows_selected, 1]
    )

  date_comm <- df()[input$origTable_rows_selected, 2] %>%
    str_trunc(width = 10, side = "right", ellipsis = "")

  comment <- tibble(SITE = site_comm, DATE = date_comm, PARAMETER = input$comment_par,
                    COMMENTER = commenter, COMMENT_TEXT = input$comment_text)
  # glimpse(comment)
  return(comment)
}

observeEvent(input$save_comment,{
  ns <- session$ns
  csv <- switch(data_name,
              "stagedData" = stagedCommentsCSV,
              "stagedComments" = stagedCommentsCSV,
              "submittedData" = comment_csv,
              "submittedComments" = comment_csv
  )
  rds <- switch(data_name,
              "stagedData" = stagedCommentsRDS,
              "stagedComments" = stagedCommentsRDS ,
              "submittedData" = submittedCommentsRDS,
              "submittedComments" = submittedCommentsRDS
  )

  saveComment_mod(data = formatComment_mod(), csvFile = csv, rdsFile = rds)
  shinyalert(title = "Comment Saved!", type = "success")
  # session$sendCustomMessage("handler1", message = 'Comment Saved!')
  removeModal()
})

  proxy = dataTableProxy('origTable')

  observeEvent(input$resetPage,{
    if(input$resetPage){
      proxy %>% selectPage(input$page)
      updateCheckboxInput(session,"resetPage", value=FALSE)
    }
  })


  observeEvent(input$origTable_cell_edit, {

    info = input$origTable_cell_edit
    # str(info)

    i = info$row
    j = info$col
    v = info$value
    x <- df()
    x[i, j] <- DT::coerceValue(v, x[i, j])
    replaceData(proxy, x, resetPaging = FALSE)  # important

    if(input$result=="updated"){
      updated1<<-x
      updateTextInput(session,"result",value="updated1")
    } else{
      updated<<-x
      updateTextInput(session,"result",value="updated")
    }
    updateNumericInput(session,"page",value=(i-1)%/%10+1)


  })
### DELETE ROW ####
  observeEvent(input$delRow,{
    ns <- session$ns
    ids <- input$origTable_rows_selected
    showModal(
      if(length(ids)>0){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete this record?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("ok"), "Yes")
          ), easyClose = TRUE)
      } else {
        modalDialog(
          title = "Delete Row",
          "Please Select Row(s) To Delete. Press 'Esc' or Press 'OK' button",
          easyClose = TRUE,
          footer=modalButton("OK")
        )
      }
    ) # End Modal
  })
### CONFIRM DELETE ####
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    ids <- input$origTable_rows_selected
    x<-as.data.frame(df())
    restored<<-x
    x <- x[-ids,]

    if(input$result=="deleted"){
      deleted1<<-x
      updateTextInput(session,"result",value="deleted1")
    } else{
      deleted<<-x
      updateTextInput(session,"result",value="deleted")
    }
    updateNumericInput(session,"page",value=(ids[1]-1)%/%10+1)
    removeModal()
  })

### UPDATE ROW ####
  observeEvent(input$update,{
    ids <- input$no
    x<-df()
    restored<<-x

    myname=colnames(x)
    status=ifelse(tibble::has_rownames(x),1,0)
    x<-as.data.frame(x)
    rownames(x)[ids]=input$rowname

    for(i in 1:ncol(x)){
      #x[ids,i]=input[[myname[i]]]
      if("character" %in% class(x[ids,i])){
        # levels(x[ids,i]) <- unique(c(levels(x[ids,i]), newlevel()))
        try(x[ids,i] <- paste0(input[[myname[i]]], collapse = "; ") %>% trimws())
      } else {
        try(x[ids,i] <- input[[myname[i]]])
      }
      # if("numeric" %in% class(x[ids,i])){
      #   x[ids,i]=as.numeric(input[[myname[i]]])
      # }
      if("POSIXct" %in% class(x[ids,i])){
        tz=""
        if(!is.null(attr(x[ids,i],"tzone"))) tz=attr(x[ids,i],"tzone")
        x[ids,i]=as.POSIXct(input[[myname[i]]],tz=tz,origin="1970-01-01")
      }
    }
    if(input$result=="updated"){
      updated1<<-x
      updateTextInput(session,"result",value="updated1")
    } else{
      updated<<-x
      updateTextInput(session,"result",value="updated")
    }
    #updateCheckboxInput(session,"showEdit",value=FALSE)
  })

  observeEvent(input$Close,{
    updateCheckboxInput(session,"showEdit",value=FALSE)
    updateSelectInput(session = session,
                      inputId = "selectFile",
                      label = "Choose submitted data to process and import:",
                      choices = fileChoices(),
                      selected = input$selectFile)
    session$sendCustomMessage('unbind-DT', 'origTable')
  })
### TOGGLE ROW EDIT ####
  observeEvent(input$no,{
    mydf2=df()

    if(!is.null(mydf2)){
      myclass=lapply(mydf2,class)

      updateTextInput(session,"rowname",value=rownames(mydf2)[input$no])
      updateNumericInput(session,"width",value=input$width)
      mydf=as.data.frame(mydf2[input$no,])

      for(i in 1:ncol(mydf)){
        myname=colnames(mydf)[i]
        type <- table_fields$col_type[table_fields$dt_cols == myname]
        choices <- table_fields$choices[table_fields$dt_cols == myname]
        mult <- switch(table_fields$input_mult[table_fields$dt_cols == myname],
                       "yes" = TRUE,
                       "no" = FALSE)
        width <- table_fields$input_width[table_fields$dt_cols == myname]
        commenter_choices <- c(mydf2[input$no ,4], app_user) %>% unique()

        if(type == "factor"){
          if(mult == TRUE){
            updateSelectInput(session,myname,
                              choices=unique(c(mydf[1,i],get(choices))), selected=mydf[1,i])
          } else {
            updateSelectInput(session,myname,
                              choices=get(choices), selected=mydf[1,i])
          }

        # if("factor" %in% myclass[[i]]){
        #   updateSelectInput(session,myname,
        #                     choices=unique(c(levels(mydf[[i]]),get(choices))), selected=mydf[1,i])
      # for(i in 1:ncol(mydf)){
      #   myname=colnames(mydf)[i]
      #   if("factor" %in% myclass[[i]]){
          # updateSelectInput(session,myname,
      #                       choices=levels(mydf[[i]]),selected=mydf[1,i])
        } else if("Date" %in% myclass[[i]]){
          updateDateInput(session,myname,value=mydf[1,i])
        } else if("logical" %in% myclass[[i]]){
          if(is.na(mydf[1,i])) myvalue=FALSE
          else myvalue=mydf[1,i]
          updateCheckboxInput(session,myname,value=myvalue)
        } else { # c("numeric","integer","charater")

          mywidth=(((max(nchar(mydf2[[i]]),defaultlen,na.rm=TRUE)*8) %/% input$width2)+1)*input$width2
          if(mywidth<=500){
            updateTextInput(session,myname,value=mydf[1,i])
          } else{
            updateTextAreaInput(session,myname,value=mydf[1,i])
          }
        }
      }
    }

  })

  observeEvent(input$home,{
    updateNumericInput(session,"no",value=1)
  })

  observeEvent(input$end,{
    updateNumericInput(session,"no",value=nrow(df()))
  })

  observeEvent(input$left,{
    value=ifelse(input$no>1,input$no-1,1)
    updateNumericInput(session,"no",value=value)
  })

  observeEvent(input$right,{
    value=ifelse(input$no<nrow(df()),input$no+1,nrow(df()))
    updateNumericInput(session,"no",value=value)
  })

  observeEvent(input$rowno,{
    maxno=nrow(df())
    print(maxno)
    print(input$rowno)
    if(is.na(input$rowno)) updateNumericInput(session,"rowno",value=maxno)
    if(input$rowno>maxno) {
      updateNumericInput(session,"rowno",value=maxno)
      updateNumericInput(session,"no",value=maxno)
    } else{
      updateNumericInput(session,"no",value=input$rowno)
    }

  })
### EDIT MODAL UI ####
  output$edit_modal=renderUI({
    ns <- session$ns
    ids <- input$no
    if(length(ids)==1){

      mydf2=df()
      mylist=list()
      myclass=lapply(mydf2,class)
      mylist[[1]]=actionButton(ns("home"),"",icon=icon("backward",lib="glyphicon"))
      mylist[[2]]=actionButton(ns("left"),"",icon=icon("chevron-left",lib="glyphicon"))
      mylist[[3]]=numericInput3(ns("rowno"),"Row#",value=input$no,min=1,
                                max=nrow(mydf2),step=1,width=60+10*log10(nrow(mydf2)))
      mylist[[4]]=actionButton(ns("right"),"",icon=icon("chevron-right",lib="glyphicon"))
      mylist[[5]]=actionButton(ns("end"),"",icon=icon("forward",lib="glyphicon"))
      # mylist[[6]]=actionButton(ns("new"),"",icon=icon("plus",lib="glyphicon"))
      mylist[[6]]=textInput3(ns("rowname"),"rowname",value=rownames(mydf2)[input$no],width=150)
      # mylist[[8]]=numericInput3(ns("width"),"input width",value=input$width2,min=100,max=500,step=50,width=80)
      mylist[[7]]=hr()
      addno=7
      mydf=as.data.frame(mydf2[input$no, edit_cols])

      for(i in 1:ncol(mydf)){
        myname=colnames(mydf)[i]
        type <- table_fields$col_type[table_fields$dt_cols == myname]
        choices <- table_fields$choices[table_fields$dt_cols == myname]
        mult <- switch(table_fields$input_mult[table_fields$dt_cols == myname],
                       "yes" = TRUE,
                       "no" = FALSE)
        width <- table_fields$input_width[table_fields$dt_cols == myname]
        commenter_choices <- c(mydf2[input$no ,4], app_user) %>% unique()

### Note: connot update levels of factors during edit - use table-fields to pick which input style for each variable
        # Try changing sampler to character type so that the choices can be updated during edit mode.

        if(type == "factor"){
          if(mult == TRUE){
            mylist[[i+addno]]=selectInput3(ns(myname),myname,
                                           choices=unique(c(mydf[[i]],get(choices))),selected=mydf[1,i], multiple = mult, width=width)
          } else {
            mylist[[i+addno]]=selectInput3(ns(myname),myname,
                                           choices=get(choices),selected=mydf[1,i], multiple = mult, width=width)

          }
          # } else
          # if("factor" %in% myclass[[i]]){
          #  mylist[[i+addno]]=selectInput3(ns(myname),myname,
          #                                choices=unique(c(levels(mydf[[i]]),get(choices))),selected=mydf[1,i], multiple = mult, width=width)
                                         # choices=levels(mydf[[i]]),selected=mydf[1,i],width=input$width2)
        } else if("Date" %in% myclass[[i]]){
          mylist[[i+addno]]=dateInput3(ns(myname),myname,value=mydf[1,i],width=input$width2)
        } else if("logical" %in% myclass[[i]]){
          if(is.na(mydf[1,i])) myvalue=FALSE
          else myvalue=mydf[1,i]
          mylist[[i+addno]]=checkboxInput3(ns(myname),myname,value=myvalue,width=input$width2)
        } else { # c("numeric","integer","charater")
          #cat("max(nchar(mydf2[[i]]))=",max(nchar(mydf2[[i]])))
          #cat("\n",mydf2[[i]][which.max(nchar(mydf2[[i]]))],"\n")
          mywidth=(((max(nchar(mydf2[[i]]),defaultlen,na.rm=TRUE)*8) %/% input$width2)+1)*input$width2
          #cat("mywidth=",mywidth,"\n")
          if(mywidth<=500){
            mylist[[i+addno]]=textInput3(ns(myname),myname,value=mydf[1,i],width=mywidth)
          } else{
            mylist[[i+addno]]=textAreaInput(ns(myname),myname,value=mydf[1,i],width="500px")
          }
        }
      }
      do.call(tagList,mylist)
    } else{
      h4("You can edit data after select one row in datatable.")
    }
  })

  observeEvent(input$width,{
    updateNumericInput(session,"width2",value=input$width)
  })

  observeEvent(input$editData,{
    ns <- session$ns
    ids <- input$origTable_rows_selected
     if(length(ids)>0){
      if(length(ids)==1){
        updateNumericInput(session,"no",value=ids)
      } else {
        #updateCheckboxInput(session,"showEdit",value=TRUE)
       updateNumericInput(session,"no",value=1)
      }

      editData2()
      updateNumericInput(session,"page",value=(ids-1)%/%10+1)
    } else {
        showModal(
          modalDialog(
            title = "Edit data...",
            "There is no data selected to edit! Press 'Esc' or Press 'OK' button",
            easyClose = TRUE,
            footer=modalButton("OK")
          )
        ) #End Modal
    }
  })

  editData2=reactive({
    input$editData
    # input$addRow
    ns <- session$ns

    showModal(modalDialog(
      title="Edit Data",
      footer=tagList(
        # actionButton(ns("remove"),"Delete",icon=icon("remove",lib="glyphicon")),
        actionButton(ns("update"),"Update",icon=icon("ok",lib="glyphicon")),
        modalButton("Close",icon=icon("eject",lib="glyphicon"))),
      easyClose=TRUE,
      uiOutput(ns("edit_modal"))
    ))
  })
  return(df)
}


