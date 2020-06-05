################################### HEADER ###################################
#  TITLE: gsheets.R
#  DESCRIPTION: Connects to google sheets with cached auth token
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: May 25, 2020
#  GIT REPO: BRCWQDM
#  R version 3.6.3 (2020-02-29)  x86_64
##############################################################################.
library("googlesheets4")

########################################################################.
###                     AUTH SETUP                                  ####
########################################################################.

### Initial setup and options
### Code reference: https://github.com/jennybc/rsc-gsheets

# ### designate project-specific cache
# options(gargle_oauth_cache = ".secrets")

# ### check the value of the option, if you like
# gargle::gargle_oauth_cache()

### sheets reauth with specified token and email address
# googlesheets4::sheets_auth(
#   cache = dataDir,
#   email = config[4]
#   )

### Set required options and get token
httr::set_config(httr::config(http_version = 0))
options(gargle_oob_default = TRUE, gargle_oauth_email = TRUE)

# token <- gs4_token() ## Use this to re-authenticate
gs_tokenpath <- paste0(dataDir, "gs4_token.RDS")
# saveRDS(token, paste0(dataDir, "gs4_token.RDS"))
# Fetch the saved token
gs_token <- readRDS(gs_tokenpath)
### Check that token is valid
# gs4_has_token()

### Open authorized access to google sheets
# gs4_auth(email = config[4], token = gs_token)

########################################################################.
###                          WITH AUTH                              ####
########################################################################.

GS_APPEND_PHOTO <- function(sheet, data) {
  sheet_append(ss = sheet, data)
  GS_GET_PHOTOS(sheet)
  return(print("Photo record successfully added to list..."))
}

# sheet <- config[14]
GS_GET_PHOTOS <- function(sheet) {
  df_photos <- range_read(ss = sheet) %>% as.data.frame()
  df_photos$DATE <- unlist(df_photos$DATE)
  print("Photo list retrieved...")
return(df_photos)
}
