### Functions to interface with Dropbox ####

library(rdrop2)
### Get Token - only need once, then saved locally -DONE
# drop_auth()
### Save the Token to an object - DONE
# token <- drop_auth()
### Save token
# tokendir <- paste0(config[1], "Data/")
# saveRDS(token, paste0(tokendir, "dropb_token.RDS"))
tokenpath <- paste0(config[1], "Data/dropb_token.RDS")
### Use Token like this:
drop_auth(rdstoken = tokenpath)

### Function to Load DB RDS Files ####
LOAD_DB_RDS <- function(){
### List Drop Box files ####
  # dropb_root_dir <- config[12]
  # safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = TRUE)
  # dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/AppFiles"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  # dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  # files <- dir_listing$result
  # files$path_display
# #
local_data_dir <- paste0(config[1],"Data/rdsFiles")

# ### Save all database RDS files to Local Data Cache ####
# lapply(files$path_display, drop_download, local_path = local_data_dir, overwrite = TRUE,
#          dtoken =  drop_auth(rdstoken = tokenpath))

### Load rds files ####
### Make a list of all the .rds files using full path
rds_files <- list.files(local_data_dir, full.names = TRUE , pattern = "\\.rds$")
### create an object that contains all of the rds files
data <- lapply(rds_files, readRDS)
### Make a list of the df names by eliminating extension from files
df_names <- gsub(".rds", "", list.files(local_data_dir, pattern = "\\.rds$"))
# name each df in the data object appropriately
names(data) <- df_names
### Extract each element of the data object into the global environment
list2env(data ,.GlobalEnv)
### Remove data
rm(data)
}

SUBMIT_CSV <- function(zone, drop_path = "/BRCWQDM/Submitted_Data_Staging"){
 ### Args for interactive testing
  ### Need to add some error handling here
  zone <- "Mid-Reach"
  # drop_path <-  "BRCWQDM/Submitted_Data_Staging"
  ### Upload the data first
  csv <- stagedDataCSV
  if(file.exists(csv)){
  fn <- paste0(zone,"_SubmittedData_",today(),".csv")
  file.copy(csv,fn, R = TRUE)
  ### First check and see if it was already submitted
  # drop_exists(path = drop_path, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = fn, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  ### Now move the submitted data to the submitted folder
  file.rename(fn, paste0(submittedDataDir,fn))
  file.remove(csv)
  } else {
    print("There were no data to upload!")
  }

  ### Upload the Comments
  csv <- stagedCommentsCSV
  if(file.exists(csv)){
  fn <- paste0(zone,"_SubmittedComments_",today(),".csv")
  file.copy(csv,fn,overwrite = TRUE)
  ### First check and see if it was already submitted
  # drop_exists(path = drop_path, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = fn, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  ### Now move the submitted data to the submitted folder
  file.rename(fn, paste0(submittedDataDir,fn))
  file.remove(csv)
  } else {
    print("There were no comments to upload!")
  }
  return(print("File uploaded to Dropbox successful"))
}


ARCHIVE_CSV <- function(){

  drop_move(from_path = , to_path = , dtoken = drop_auth(rdstoken = tokenpath) )

}


  # sitesRDS <- paste0(remote_data_dir,"sites_db.rds")
  # peopleRDS <- paste0(remote_data_dir,"people_db.rds")
  # parametersRDS <- paste0(remote_data_dir,"parameters_db.rds")
  # assignmentsRDS <- paste0(remote_data_dir,"assignments_db.rds")
  #
  # ### Dropbox Submitted Data Staging
  # "/BRCWQDM/Submitted_Data_Staging"
  # ### Dropbox Imported Data Archived (move/rename after import)
  # "/BRCWQDM/Imported_Data_Archived"





