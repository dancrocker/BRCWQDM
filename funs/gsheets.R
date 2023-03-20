################################### HEADER ###################################
#  TITLE: gsheets.R
#  DESCRIPTION: Connects to google sheets with cached auth token
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: May 25, 2020
#  GIT REPO: BRCWQDM
#  R version 3.6.3 (2020-02-29)  x86_64
##############################################################################.

library("googlesheets4")

auth_json <-  paste0(dataDir, "brcwqdm-cb75549bb9d4.json")
# gs_api <- config[15]
gs_scope <- "https://www.googleapis.com/auth/spreadsheets"

########################################################################.
###                      SET OPTIONS                               ####
########################################################################.
options(gargle_quiet = FALSE)

if (!interactive()) {
  print(glue("Googlesheets API being used non-interactively at {Sys.time()}"))
  gs4_deauth()
}

### Set required options and get token
httr::set_config(httr::config(http_version = 2))

options(gargle_oauth_cache = dataDir,
        gargle_oauth_email = TRUE,
        # gargle_oob_default  = TRUE,
        gargle_verbosity = "debug")

#options(shiny.port = 3838)
#options(googleAuthR.redirect = "http://localhost:3838")

########################################################################.
###                          WITH AUTH                              ####
########################################################################.

# gs4_auth_configure(api_key = jsonlite::fromJSON(txt = auth_json, simplifyVector = TRUE)[["private_key"]])

gs4_auth(path = auth_json,
         # use_oob = TRUE,
         scopes = gs_scope)

GS_APPEND_PHOTO <- function(sheet, data, photos) {
  if (gs4_has_token()) {
    sheet_append(ss = sheet, data)
    GS_GET_PHOTOS(sheet, photos = photos)
    return(print("Photo record successfully added to list..."))
  } else {
    return("No token found for authentication...photo could not be added")
  }
}

# GS_APPEND_PHOTO(sheet = config[14], data = photo_rec)
# sheet <- config[14]

GS_GET_PHOTOS <- function(sheet, photos) {
    if (gs4_has_token()) {
      df_photos <- range_read(ss = sheet) %>% as.data.frame()
      # df_photos$DATE <- unlist(df_photos$DATE)

      print("Photo list retrieved...")
    } else {
      df_photos <- NULL
      return("No token found for authentication...photo list could not be retrieved")
    }
    if (nrow(df_photos) == 0) {
      all_photos <- photos %>%
        select(-ID) %>%
        distinct()
    } else {
      all_photos <- bind_rows(photos, df_photos) %>%
        select(-ID) %>%
        distinct()
    }

  rxdata$photos <<- all_photos
}

# GS_GET_PHOTOS(sheet = config[14])
# df_photos <- range_read(ss = config[14]) %>% as.data.frame()

GS_APPEND_SAMPLER <- function(sheet, data) {
  if (gs4_has_token()) {
    sheet_append(ss = sheet, data)
    try(GS_GET_SAMPLERS(sheet))
    print("Sampler successfully added to list...")
  } else {
    return("No token found for authentication...photo could not be added")
  }
}

GS_GET_SAMPLERS <- function(sheet) {
  if (gs4_has_token()) {
    df_samplers <- range_read(ss = sheet) %>% as.data.frame()
    print("List of samplers retrieved...")
    rxdata$samplers <<- c(samplers_db, df_samplers$FULL_NAME) %>% sort()
  } else {
    print("No token found for authentication...sampler list could not be retrieved from google sheets and sampler list may not be complete!")
    rxdata$samplers <<- samplers_db %>% sort()
  }
}
