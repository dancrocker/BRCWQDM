################################### HEADER ###################################
#  TITLE: mod_BRC_data_explorer
#  DESCRIPTION: Module plugin to view database data pulled together by SEID
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: April 2020
#  GIT REPO: BRCWQDM
#  R version 3.6.3 (2020-02-29)  x86_64
##############################################################################.

# library("dplyr")
# library("stringr")
# library("rpivotTable")
# library("glue")
# library("htmltools")
# library("zoo")
# library("xts")
# library("dygraphs")

### Data ####
parameters_db <- readRDS(parametersRDS)
par_data_num <- parameters_db[!parameters_db$UNITS %in% c("boolean", "text"), c(2,3)] %>% arrange(PARAMETER_NAME)
pars <- par_data_num$PARAMETER_NAME
par_names <- glue("{par_data_num$PARAMETER_NAME} ({par_data_num$UNITS})")
names(pars) <- par_names
par_choices <- pars

########################################################################.
###                           UI                                    ####
########################################################################.
# Filters: Site, Parameter

# Make Dygraphs for each parameter selected, up to 4, default date range is last year of data
# Custom date range to to trim data
#tabulara data preview
# Export csv button
#

EXPLORER_UI <- function(id) {
ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),

fluidPage(
  titlePanel("BRC DATA EXPLORER"),
  tabsetPanel(
    tabPanel("Filter and Plot",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(ns("site"), "Choose Sample Location(s):", choices = c("", sites), selected = "",
                                options = list(maxItems = 3, placeholder = 'Select up to 3 sites')),
                 selectizeInput(ns("pars"), "Choose Parameter(s):", choices = c("", par_choices), selected = "",
                                options = list(maxItems = 4, placeholder = 'Select up to 4 parameters')),
                 uiOutput(ns("dates.UI")),
                 hr(),
                 br(),
                 actionButton(ns("make_plot"),
                              label = paste0('Generate Plot'),
                              width = '90%')
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Plots",
                               uiOutput(ns("dygraph_plot"))
                     ),
                     tabPanel("Tabular",
                            fluidRow(downloadButton(ns("download_data3"), "Download table as csv"), align = "center"),
                            DTOutput(ns("data3"))
                     )
                   )
                 ) # End Main Panel
             ),
    ),
    tabPanel("Pivot Table",
             fluidRow(
               column(3,
                      h3("DATA FILTERS")
               ),
               column(9,
                      htmlOutput(ns("pivot"))
               )
             )
    )#,
    # tabPanel("Search Comments/Text",
    # )
  ) # End Tabset Panel
) # End Fluid Page
  ) # End Taglist
} # End UI
########################################################################.
###                         MODULE SERVER                            ####
########################################################################.

EXPLORER <- function(input, output, session, data, globalSession, mode = reactive(2)) {

ns <- session$ns

shinyjs::disable("make_plot")
# Pass rxdata from modules:
d <- data %>%
  filter(RESULT != -999999)
d$DATE_TIME <- ymd_hm(d$DATE_TIME, tz = "America/New_York")

d1 <- reactive({
  req(input$site)
  d %>% filter(SITE_BRC_CODE %in% input$site)
})

d2 <- reactive({
  req(d1())
  d1() %>% filter(PARAMETER %in% input$pars)
})

  ### Dates UI ####
  output$dates.UI <- renderUI({
    ns <- session$ns
    req(input$site != "")
    req(input$pars != "")
    req(d2())
    # req(isTruthy(d2()))
    tagList(
      uiOutput <- dateRangeInput(ns("dates"), label = "Choose date range:",
                                 start = min(d2()$DATE_TIME, na.rm = T),
                                 end = max(d2()$DATE_TIME, na.rm = T),
                                 min = min(d2()$DATE_TIME, na.rm = T),
                                 max = max(d2()$DATE_TIME, na.rm = T),
                                 startview = "year",
                                 separator = "to")
    )
  })

d3 <- reactive({
  req(d2())
  data3 <- d2()
  data3 %>% filter(between(DATE_TIME, input$dates[1], input$dates[2]))
  data3
})

  observe({
    req(input$site)
    req(input$pars)
    if ("" %in% input$site) {
       shinyjs::disable("make_plot")
    } else {
      if ("" %in% input$pars) {
        shinyjs::disable("make_plot")
      } else {
        shinyjs::enable("make_plot")
      }
    }
  })

  plot1 <- eventReactive(input$make_plot, {
    plot1 <- dg_plot(data = d3())
    plot1 <- htmltools::tagList(plot1)
    return(plot1)
  })

  #### PLOT1 OUTPUT ####
  output$dygraph_plot <- renderUI({
       ns <- session$ns
       tagList(
         plot1()
       )
  })

########################################################################.
###                          PREVIEW PLOT                          ####
########################################################################.

dg_plot <- function(data) {

  ###  FILTER DATA  ####
  pars <- data$PARAMETER %>% unique()
  sites <- data$SITE_BRC_CODE %>% unique()
  min_dt <- min(data$DATE_TIME, na.rm = TRUE)
  max_dt <- max(data$DATE_TIME, na.rm = TRUE)

  dg <- data %>%
    select(3:7) %>%
    filter(SITE_BRC_CODE %in% sites, PARAMETER %in% pars)

  names(dg) <- c("Site", "Date-Time", "Parameter", "Results", "Units")

  plotobj <- list()

  for(i in seq_along(pars)) {
    par <- pars[i]
    dg_i <- dg %>% filter(Parameter == par)
    unit <- dg_i %>% slice(1) %>% pull(Units)
    ylab <- glue("{par} ({unit})")
    dg_i <- dg_i %>% select(-c(3,5)) %>% spread(Site, Results)
    dg_i <- xts(dg_i, order.by = dg_i$`Date-Time`)
    dg_i <- dg_i[,-1]
    plotobj[[i]] <- dygraph(dg_i, group = "main", main = par) %>%
      dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5, fillGraph = FALSE, pointSize = 5, drawPoints = TRUE,
                connectSeparatedPoints = TRUE, colors = c("blue", "green" ,"red")) %>%
      dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 1, hideOnMouseOut = FALSE)  %>%
      dyAxis(name = "y", label = ylab) %>%
      dyRangeSelector(dateWindow = c(min_dt, max_dt), strokeColor = '') %>%
      dyCrosshair(direction = "vertical") %>%
      dyLegend(width = "100%", show = "always", showZeroValues = TRUE, labelsSeparateLines = TRUE)
  }
  # render the dygraphs objects using htmltools
  return(plotobj)
}

# data <- data_num_db
# i <- 1
# rm(data,i)

########################################################################.
###                      TABULAR DATA                              ####
########################################################################.
  output$data3 <- renderDataTable({
    req(!is.null(d3()))
    datatable(d3(), rownames = F, filter = "top") %>%
    formatDate(columns = "DATE_TIME", method = 'toLocaleString')
  },
  options = list(autoWidth = TRUE,
                 # scrollX = T,
                 columnDefs = (list(list(width = '100px', targets = c(0, 1)),
                                    list(width = '200px', targets = c(7))
                 )
                 )
  ))

  # Downloadable csv of numerical data
  output$download_data3 <- downloadHandler(
    filename = function() {
      paste0("BRC_NumericalData_",today() ,".csv")
    },
    content = function(file) {
      write_csv(d3(), file)
    }
  )
########################################################################.
###                          PIVOT TABLE                          ####
########################################################################.
  pivot_data <- reactive({
    data %>%
      filter(RESULT != -999999) %>%
      mutate("Date" = as_date(DATE_TIME)) %>%
      select(c(3,5:7,9))
  })
  output$mypivot = renderRpivotTable({
    rpivotTable(data = pivot_data(),
                rows = "PARAMETER",
                col = "SITE_BRC_CODE",
                aggregatorName = "Average",
                vals = "RESULT",
                rendererName = "Row Heatmap",
                onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }")
    )
  })

  #### PIVOT OUTPUT ####
  output$pivot <- renderUI({
         rpivotTableOutput(ns("mypivot"))
  })
} ### END SERVER

