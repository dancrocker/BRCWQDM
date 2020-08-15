################################### HEADER ###################################
#  TITLE: gsheets.R
#  DESCRIPTION: Connects to google sheets with cached auth token
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: May 25, 2020
#  GIT REPO: BRCWQDM
#  R version 3.6.3 (2020-02-29)  x86_64
##############################################################################.
# library("gargle")
library("googlesheets4")
# library("googleAuthR")
# library("googledrive")

# devtools::install_github("tidyverse/googlesheets4")

########################################################################.
###                       GET CREDENTIALS                            ####
########################################################################.

### Get a token to cache ####
# token <- gs4_token() ## Use this to re-authenticate
# gs_tokenpath <- paste0(dataDir, "gs4_token.RDS")
# saveRDS(token, paste0(dataDir, "gs4_token.RDS"))
### Fetch the saved token
# gs_token <- readRDS(gs_tokenpath)
### Check that token is valid
# gs4_has_token()




########################################################################.
###                     AUTH VARS                                   ####
########################################################################.

# gs_tokenpath <- paste0(dataDir, "gs4_token.RDS")
# gs_token <- readRDS(gs_tokenpath)

auth_json1 <-  paste0(dataDir, "api_creds_orig.json")
# auth_json2 <-  paste0(dataDir, "brcwqdm-0fad5d12f18f.json")
auth_json3 <-  paste0(dataDir, "brcwqdm-cb75549bb9d4.json")

gs_api <- config[15]
gs_scope <- "https://www.googleapis.com/auth/spreadsheets"

# token <- credentials_service_account(
#   scopes = "https://www.googleapis.com/auth/spreadsheets",
#   path = auth_json3
# )

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

options(gargle_oauth_email = TRUE,
        gargle_oob_default   = TRUE)

options(shiny.port = 3838)
options(googleAuthR.redirect = "http://localhost:3838")

# options("googlesheets.client_id" = jsonlite::fromJSON(txt = auth_json1, simplifyVector = TRUE)[["installed"]][["client_id"]])
# options("googlesheets.client_secret" = jsonlite::fromJSON(txt = auth_json1, simplifyVector = TRUE)[["installed"]][["client_secret"]])

########################################################################.
###                     AUTH METHODS                                ####
########################################################################.

#### Method https://www.thetopsites.net/projects/shiny/google-sheets.shtml ####
# options(googleAuthR.redirect = "http://localhost:3838")
#
# # JSON with you client data from GCP
# gar_set_client(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly",
#                web_json = "<YOUR_JSON>")
# spreadsheet_key <- "<YOUR SHEET>"
####

###********************************************************************************###

### Method: gs4 (jennybc)
### Code reference: https://github.com/jennybc/rsc-gsheets

# jsonlite::fromJSON(txt = auth_json1, simplifyVector = TRUE)[["installed"]][["client_id"]]


# gs4_auth_configure(api_key = config[15],)

# ### designate project-specific cache
# options(gargle_oauth_cache = ".secrets")

# ### check the value of the option, if you like
# gargle::gargle_oauth_cache()

### sheets reauth with specified token and email address
# googlesheets4::sheets_auth(
#   cache = dataDir,
#   email = config[4]
#   )

# gargle_oob_default = TRUE
# gargle_oauth_cache = ".secrets"

# drive_auth(path = info, scopes = gs_scope,use_oob = T, token = gs_token)

########################################################################.
###                          WITH AUTH                              ####
########################################################################.


# info <- jsonlite::fromJSON(txt = auth_json1, simplifyVector = F) #[["installed"]]

#
# httr::oauth_app(
#     appname = info$project_id,
#     key = info$client_id,
#     secret = info$client_secret
#   )

# google_app <- httr::oauth_app(
#   "brcwqdm",
#   key = config[16],
#   secret = config[17]
# )

gs4_auth_configure(api_key = jsonlite::fromJSON(txt = auth_json3, simplifyVector = TRUE)[["private_key"]])

# oauth_app_from_json(path = app_auth)
# confirm the changes
# gs4_oauth_app()
# gs4_api_key()

gs4_auth(use_oob = TRUE,
         path = auth_json3,
         scopes = gs_scope)
# Sys.setenv("GOOGLE_APPLICATION_CREDENTIALS" = app_auth)
# Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")

  # bring your own app via JSON downloaded from Google Developers Console
  # this file has the same structure as the JSON from Google
### Open authorized access to google sheets

### Some test data
# photo_rec <- tibble(SITE_CODE = "The Site",
#                       DATE = "2020-09-15",
#                       PARAMETER = "General",
#                       PHOTAGRAPHER = "DC",
#                       FILENAME = file,
#                       CAPTION = "My Caption",
#                       ADDED_BY = app_user)

GS_APPEND_PHOTO <- function(sheet, data) {
  if (gs4_has_token()) {
    sheet_append(ss = sheet, data)
    GS_GET_PHOTOS(sheet)
    return(print("Photo record successfully added to list..."))
  } else {
    return("No token found for authentication...photo could not be added")
  }
}

# GS_APPEND_PHOTO(sheet = config[14], data = photo_rec)
# sheet <- config[14]

GS_GET_PHOTOS <- function(sheet) {
    if (gs4_has_token()) {
      df_photos <- range_read(ss = sheet) %>% as.data.frame()
      df_photos$DATE <- unlist(df_photos$DATE)
      print("Photo list retrieved...")
    } else {
      return("No token found for authentication...photo list could not be retrieved")
    }
return(df_photos)
}

