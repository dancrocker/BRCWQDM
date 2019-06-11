### Functions for shiny App

# data <- read.table(stagedDataCSV, stringsAsFactors = FALSE, header = T)
# str(data)
#
# ### These lists are static
# sites <- sites_db$BRC_CODE
# names(sites) <- paste0(sites_db$SITE_NAME, " - ", sites_db$BRC_CODE)
# sites <- sites[order(names(sites))]
# samplers <- sort(unique(assignments_db$NAME[assignments_db$YEAR == year(Sys.Date())]))
#
# wea_choices <- c("","Storm (heavy rain)", "Rain (steady rain)",
#                  "Showers (intermittent rain)", "Overcast","Clear/Sunny", "Other","Not Recorded")
# wat_appear_choices <- c("","Clear", "Milky", "Foamy", "Oily Sheen",
#                         "Dark Brown", "Greenish", "Orange", "Tea Color", "Other", "Not Recorded")
# wat_trash_choices <- c("","None", "Light", "Medium", "Heavy", "Not Recorded")
# wat_odor_choices <-c("","None", "Sewage", "Fishy", "Chlorine",
#                      "Rotten Eggs", "Other", "Not Recorded")
# wat_NAV_choices <- c("","None", "Light", "Medium", "Heavy", "Not Recorded")
# wat_clarity_choices <- c("","Clear","Slightly Hazy","Cloudy","Opaque", "Not Recorded")
# wat_erosion_choices <- c("","Undercut bank", "Slumping", "Erosional gullies in bank",
#                          "Bridge or building undermining", "No erosion", "Not Recorded")
# depth_choices <- c("","Gage (Staff Plate-feet)", "Ruler (inches)", "Not Recorded", "No Datum")


data_csv2df <- function(data, data_fields){
  ### Read the RDS File
  mydata <- data
  names(mydata) <- as.character(data_fields$dt_cols)
  # names(mydata)
  ### Convert to Dataframe
  mydata <- data.frame(mydata, stringsAsFactors = F, check.names = F)

  ### Get list of columns that use select input and need to be factors
  # col_fact <- data_fields$dt_cols[data_fields$col_type == "factor"]
  # col_fact
  ### Convert factor variables
  # mydata[,col_fact] <- lapply(mydata[,col_fact], factor)  ## as.factor() could also be used
  ### fix Factor Levels
  # levels(mydata$Site) <- unique(c(levels(mydata$Site), paste0(sites)))
  # levels(mydata$Samplers) <- unique(c(levels(mydata$Samplers), samplers))
  # levels(mydata$`Weather Last 48hrs`) <- unique(c(levels(mydata$`Weather Last 48hrs`),wea_choices))
  # levels(mydata$`Current Weather`) <- unique(c(levels(mydata$`Current Weather`), wea_choices))
  # levels(mydata$`Water Appearance`) <- unique(c(levels(mydata$`Water Appearance`), wat_appear_choices))
  # levels(mydata$Trash) <- unique(c(levels(mydata$Trash), wat_trash_choices))
  # levels(mydata$Erosion) <- unique(c(levels(mydata$Erosion), wat_erosion_choices))
  # levels(mydata$`Water Odor`) <- unique(c( levels(mydata$`Water Odor`), wat_odor_choices))
  # levels(mydata$`Water Veg`) <- unique(c(levels(mydata$`Water Veg`),wat_NAV_choices))
  # levels(mydata$`Water Clarity`) <- unique(c(levels(mydata$`Water Clarity`), wat_clarity_choices))
  # levels(mydata$`Depth Measurement Type`) <- unique(c( levels(mydata$`Depth Measurement Type`), depth_choices))

  return(mydata)
}
# data <- read.table(stagedCommentsCSV, stringsAsFactors = FALSE, header = T)
comm_csv2df <- function(data, comment_fields){
  ### Read the RDS File
  mydata <- data
  names(mydata) <- as.character(comment_fields$dt_cols)
  # names(mydata)
  ### Convert to Dataframe
  mydata <- data.frame(mydata, stringsAsFactors = F, check.names = F)

  ### Get list of columns that use select input and need to be factors
  # col_fact <- comment_fields$dt_cols[comment_fields$col_type == "factor"]
  # col_fact
  ### Convert factor variables
  # mydata[,col_fact] <- lapply(mydata[,col_fact], factor)  ## as.factor() could also be used
  ### fix Factor Levels
  # levels(mydata$`Comment Site`) <- unique(c(levels(mydata$`Comment Site`), paste0(sites)))
  # levels(mydata$Parameter) <- unique(c(levels(mydata$Parameter),"General Comment", data_fields$dt_cols[table_fields$take_comments == "yes"]))
  # levels(mydata$Commenter) <- unique(c(levels(mydata$Commenter), app_user))
  return(mydata)
}

