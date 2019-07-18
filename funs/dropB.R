### Functions to interface with Dropbox ####

library(rdrop2)
httr::set_config(httr::config(http_version = 0))
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

SUBMIT_CSV <- function(zone, drop_path = "BRCWQDM/Submitted_Data_Staging"){
 ### Args for interactive testing
  ### Need to add some error handling here
  # zone <- "Mid-Reach"
  # drop_path <-  "BRCWQDM/Submitted_Data_Staging"
  ### Upload the data first
  csv <- stagedDataCSV
  if(file.exists(csv)){
  fn <- paste0(zone,"_SubmittedData_",today(),".csv")
  file.copy(csv,fn, overwrite = TRUE)
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
  csv <- stagedCommentsCSV

  ### Upload the Comments
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

GET_SUBMITTED_DATA <- function(){
  ### List Drop Box files ####
  dropb_root_dir <- config[12]
  safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = FALSE)
  dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  files <- drop_dir(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  # files <- dir_listing$result["name"] %>% unlist()
  files <- files$name
  local_data_dir <- paste0(config[1],"Data/SubmittedData")
  if(files %>%  length() %>% as.numeric() > 0){
    ### Check if we already have the submitted files locally
       if(all(files %in% list.files(submittedDataDir))){
         print("All submitted files found on Dropbox were previously downloaded")
        } else {
          paths <- unlist(dir_listing$result["path_display"])
          ### At least one submitted file is not already downloaded - download the missing ones
          for (i in seq_along(paths)) {
           fn <- str_replace(paths[[i]],"/BRCWQDM/Submitted_Data_Staging/","")
            if (!fn %in% list.files(local_data_dir,full.names = F)) {
              drop_download(path = paths[[i]], local_path = local_data_dir, overwrite = FALSE,
                 dtoken =  drop_auth(rdstoken = tokenpath))
            } # End If
          } # End Loop
        } # End else
    } else {
    print("There were no submitted files found on Dropbox")
  }
}

ARCHIVE_SUBMITTED_DATA <- function(data_file){
 ### List Drop Box files ####
# data_file <- input$selectFile # in shiny app
# data_file <- "Mid-Reach_SubmittedData_2019-07-11.csv"
comment_file <- str_replace(data_file,"_SubmittedData_","_SubmittedComments_")

dropb_root_dir <- config[12]
# From/To paths

data_from_path <-  paste0(dropb_root_dir, "/Submitted_Data_Staging/", data_file)
comment_from_path <- paste0(dropb_root_dir, "/Submitted_Data_Staging/", comment_file)
data_to_path <-  paste0(dropb_root_dir, "/Imported_Data_Archive/", data_file)
comment_to_path <-   paste0(dropb_root_dir, "/Imported_Data_Archive/", comment_file)

safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = FALSE)
dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
files <- drop_dir(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))

if (comment_from_path %in% files$path_display) {
drop_move(from_path = data_from_path, to_path = data_to_path, dtoken = drop_auth(rdstoken = tokenpath))
print(paste0("The data csv file '", data_file, "'  was moved to the Imported Data Archive folder"))
}

if (comment_from_path %in% files$path_display) {
drop_move(from_path = comment_from_path,  to_path = comment_to_path, dtoken = drop_auth(rdstoken = tokenpath))
print(paste0("The comment csv file '", comment_file, "'  was moved to the Imported Data Archive folder"))
}

} # end function


  # sitesRDS <- paste0(remote_data_dir,"sites_db.rds")
  # peopleRDS <- paste0(remote_data_dir,"people_db.rds")
  # parametersRDS <- paste0(remote_data_dir,"parameters_db.rds")
  # assignmentsRDS <- paste0(remote_data_dir,"assignments_db.rds")
  #
  # ### Dropbox Submitted Data Staging
  # "/BRCWQDM/Submitted_Data_Staging"
  # ### Dropbox Imported Data Archived (move/rename after import)
  # "/BRCWQDM/Imported_Data_Archived"





