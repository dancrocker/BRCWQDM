################################### HEADER ###################################
#  TITLE: mod_map.R
#  DESCRIPTION: A module for a simple map for the BRCWQDM App
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: July 29, 2019
#  GIT REPO:
#  R version 3.4.4 (2018-03-15)  x86_64
##############################################################################.

# Notes:
#   1.
#
# To-Do List:
#   1. Add watershed delineations
#   2. Make use of stations for nutrient data for no overlap
#   3.
library(leaflet)
##############################################################################################################################
# User Interface
##############################################################################################################################
BRCMAP_UI <- function(id) {

ns <- NS(id)

  # tagList(
  #   fluidRow(
  #     tags$style(type = "text/css", "#home-map {height: calc(100vh - 50px) !important;}"),
  #     leafletOutput(ns("map"))
  #   )
  # )
    tagList(
    selectInput(ns("map_type"), "Map Style:",
                               choices=c(providers$Stamen.TonerLite,
                                         providers$CartoDB.Positron,
                                         providers$Esri.NatGeoWorldMap,
                                         providers$Esri.WorldImagery),
                               selected = providers$Stamen.TonerLite), # See Note 4

    leafletOutput(ns("map"), height = 500)
 ) # end taglist
    # tags$style(type = "text/css", "#BRCMAP-map {height: calc(100vh - 50px) !important;}"),

  # fluidRow(
  #   column(6,
  #          strong(textOutput(ns("text_db")), align = "left")
  #   ),
  #   column(6,
  #          strong(textOutput(ns("text_cred")), align = "right")
  #   )
  # )
# )

}

########################################################################.
###                       Server Function                            ####
########################################################################.

BRCMAP <- function(input, output, session, sitelist) {
# sitelist <- sites_db
  df_site <- sitelist %>%
    select(c(3:16))

# levels (Categories) of Colors and Legend
  map_levels <- c("Headwaters", "Mid-Reach", "Lower-Reach")

# Create a new column in df_site for coloring and legend purposes
  df_site <- df_site %>%
      mutate(MapFactor = factor(df_site$ZONE, levels = map_levels))
# Color

  color_pal <- colorFactor(palette = c("darkturquoise",
                                       "blue4",
                                       "purple3"),
                           domain = factor(df_site$MapFactor, levels = map_levels),
                           ordered = TRUE)

# Map

  output$map <- renderLeaflet({
    pal <- color_pal
    #Placeholder for subbasin boundaries - get HUC boundaries
    # QWW <- readOGR("gis/QuabbinWareWachusettWatersheds.shp") %>%
    #   spTransform(CRS("+proj=longlat +ellps=GRS80"))

 leaflet(data = df_site) %>%
      # Set Long/Lat (not completely neccesary)
      setView(lng = -71.6, lat = 42.10, zoom = 10) %>%
      #
      addTiles() %>%
       addProviderTiles(input$map_type,
                           options = providerTileOptions(noWrap = TRUE)) %>%
      # Basemap (World Imagery from ESRI)
      # addProviderTiles(providers$Esri.WorldImagery,
      #                  options = providerTileOptions(noWrap = TRUE)) %>%
      # Watershed Boundary
      # addPolygons(data = QWW,
      #             layerId = QWW,
      #             color = "white", # "#00008B",
      #             weight = 2, smoothFactor = 0.5,
      #             opacity = 0.7, fillOpacity = .1,
      #             fillColor = "#00008B") %>%  # ,
                  # Removed Highlighting due to BringToFront interferring with circle Markers
                  #highlightOptions = highlightOptions(color = "white",
                                                      #weight = 2,
                                                      #bringToFront = TRUE)) %>%
      # Site Location Markers
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        label=~SITE_NAME,
        popup = ~paste("SITE NAME: ", SITE_NAME, "<br/>",
                       "WATERBODY: ", WATERBODY_NAME, "<br/>",
                       "ZONE:  ", ZONE, "<br/>",
                       "DESCRIPTION:  ", SITE_DESCR, "<br/>",
                       "COLD WATER FISHERY:  ", CFR, "<br/>",
                       "HUC 12 #:  ", HUC12_NUM, "<br/>",
                       "HUC 12 NAME:  ", HUC_NAME, "<br/>",
                       "TOWN:  ", TOWN),
        color = ~pal(MapFactor),
        radius = 5,
        weight = 3,
        opacity = 0.8,
        fillOpacity = 0.4) %>%
      # Legend
      leaflet::addLegend(
                position = "topright",
                values = ~MapFactor,
                pal = pal,
                opacity = 1,
                na.label = "Not Available",
                title = "Blackstone River Coalition Zones:")

  })

} # end Server Function
