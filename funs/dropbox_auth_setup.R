### One-time Dropbox OAuth2 setup ####
### Run this script ONCE, interactively, from R or RStudio - not from the Shiny app.
### It authorizes the app against your Dropbox account and produces a refresh_token
### that never expires (unless revoked), replacing the old 4-hour access token.
###
### Prerequisites (see docs/dropbox_auth_setup.md for full instructions):
###   1. Register an app at https://www.dropbox.com/developers/apps
###      - "Scoped access", "Full Dropbox" access type
###      - Permissions tab: enable files.metadata.read, files.content.read,
###        files.content.write, then click Submit
###   2. Have the App key and App secret from the app's Settings tab handy

library(httr)
library(jsonlite)

client_id <- readline("Dropbox App key: ")
client_secret <- readline("Dropbox App secret: ")

authorize_url <- paste0(
  "https://www.dropbox.com/oauth2/authorize?",
  "client_id=", client_id,
  "&response_type=code",
  "&token_access_type=offline"
)

message("Opening a browser window to authorize the app...")
message("If it doesn't open automatically, visit this URL:")
message(authorize_url)
utils::browseURL(authorize_url)

auth_code <- readline("Paste the authorization code Dropbox shows you: ")

resp <- httr::POST(
  url = "https://api.dropboxapi.com/oauth2/token",
  body = list(
    code = trimws(auth_code),
    grant_type = "authorization_code",
    client_id = client_id,
    client_secret = client_secret
  ),
  encode = "form"
)

if (httr::http_error(resp)) {
  stop("Dropbox authorization failed: ", httr::content(resp, as = "text", encoding = "UTF-8"))
}

result <- httr::content(resp, as = "parsed", type = "application/json")

if (is.null(result$refresh_token)) {
  stop(
    "No refresh_token was returned. Make sure this app has never been authorized ",
    "before (repeat authorizations sometimes skip returning a new refresh_token). ",
    "If needed, revoke the app at https://www.dropbox.com/account/connected_apps ",
    "and run this script again."
  )
}

creds <- list(
  client_id = client_id,
  client_secret = client_secret,
  refresh_token = result$refresh_token
)

out_path <- file.path(getwd(), "dropb_token.RDS")
saveRDS(creds, out_path)

message("Saved credentials to: ", out_path)
message("Copy this file to Data/dropb_token.RDS in the config bundle distributed to each user.")
