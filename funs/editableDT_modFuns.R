
textInput3<-function (inputId, label, value = "",width=100,bg=NULL,...)
{
        style=paste0("width: ",width,"px;")
        if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
  div(style="display:inline-block;",
      if(label!="") tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", class="form-control",value = value,
                 style=style,...))
}

# selectInput3 <- function(inputId, label, choices,width,...){
#   mywidth=paste(width,"px",sep="")
#   div(style="display:inline-block;",
#       tags$label(label, style=style,`for` = inputId,class="control-label"),
#       selectInput(inputId,label,choices,width=mywidth,...))
# }

selectInput3 <- function(...,width=100){
  mywidth=paste(width,"px",sep="")
  div(style="display:inline-block;",
      selectInput(...,width=mywidth))
}

label3<-function(label,width=100,bg=NULL,...){
    style=paste0("width: ",width,"px;")
    if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
    div(style="display:inline-block;",
        tags$label(label, style=style,...))
}

numericInput3<-function (inputId, label, value, min=NA,max=NA,step=NA,width=100,...)
{
    div(style="display:inline-block;",
        tags$label(label, `for` = inputId,class="form-control"),
        tags$input(id = inputId, type = "number", class="form-control",
                   value = value, min=min,max=max,step=step,style=paste("width: ",width,"px;",sep=""),...)
    )
}

checkboxInput3<-function(inputId,label,value=FALSE,width=100){
  if(value)
    div(style="display:inline-block;",

        tags$input(id = inputId, type = "checkbox",checked = "checked"),
        tags$label(label, `for` = inputId,
                   style=paste("width: ",width-15,"px;",sep=""))
    )
  else
    div(style="display:inline-block;",
        tags$input(id = inputId, type = "checkbox"),
        tags$label(label, `for` = inputId, style=paste("width: ",width-15,"px;",sep=""))
    )
}

radioButtons3<-function(inputId,label,choices,bg=NULL,labelwidth=100,inline=FALSE,align="right",...){
     style=paste0("width: ",labelwidth,"px;")
     if(inline) style=paste0(style,"text-align:",align,";")
     if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
     if(inline){
          div(style="display:inline-block;",
              tags$label(label, style=style,`for` = inputId,class="form-control"),
              div(style="display:inline-block;",
                  radioButtons(inputId,NULL,choices,inline=inline,...)
              )
          )
     } else{

          div(style="display:inline-block;",
              radioButtons(inputId,label,choices,...)

          )

     }
}

dateInput3<-function(inputId,label,width=100,...){
     div(style="display:inline-block;",
         dateInput(inputId,label,width=paste0(width,"px"),...)
     )
}


selectizeInput3=function (..., width = 100)
{
     mywidth = paste(width, "px", sep = "")
     div(style = "display:inline-block;", selectizeInput(..., width = mywidth))
}
