### Functions to interface with Dropbox ####

library(rdrop2)
library(tibble)
library(lubridate)


### This setting is very important - if not set then random connection errors occur
httr::set_config(httr::config(http_version = 0))

### Get Token - only need once, then saved locally -DONE
# drop_auth()
### Save the Token to an object - DONE
# token <- drop_auth()
### Save token
# tokendir <- paste0(LocalDir, "Data/")
# saveRDS(token, paste0(tokendir, "dropb_token.RDS"))
tokenpath <- paste0(LocalDir, "Data/dropb_token.RDS")
### Use Token like this:
drop_auth(rdstoken = tokenpath)

### Function to Load DB RDS Files ####
LOAD_DB_RDS <- function(){
### List Drop Box files ####
dropb_root_dir <- config[12]
safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = TRUE)
dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/AppFiles"),
                              recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  if (FALSE %in% dir_listing$result) {
  print("Dropbox connection failed trying to get data RDS files. Check internet connection and verify database RDS files have been uploaded to the correct location on dropbox")
} else {
  files <- dir_listing$result
  local_data_dir <- paste0(LocalDir,"Data/rdsFiles")
  # # Get info from dropbox RDS files:
  rds_info_path <- paste0(local_data_dir,"/rds_info.rds")
  dbox_rds_info <- tibble(`rds_file` = files$name, `timestamp` = files$server_modified)
  paths <- files$path_display

  ### Save all updated database RDS files to Local Data Cache ####
  if(file.exists(rds_info_path)){ # File exists - compare each file - datestamp to the dropbox version, if drop
    # box is newer - download and overwrite, if not, then skip
    local_rds_info <- readRDS(rds_info_path)
    for (i in seq_along(paths)) {
      fn <- files$name[i]
      ts <- files$server_modified[i]
      if(local_rds_info$timestamp[local_rds_info$rds_file == fn] == ts){
        print(paste0(fn, " already up to date, skipping download and moving to next file."))
        next
      } else {
        print(paste0(fn, " out of date, downloading more recent version from Dropbox."))
        drop_download(path = paths[i], local_path = local_data_dir, overwrite = TRUE,
                      dtoken =  drop_auth(rdstoken = tokenpath))
      } # End Else
    } # End loop
    saveRDS(object = dbox_rds_info, file = rds_info_path)
  } else { # rds info file does not exist, download all rds files (overwrite) as well as the rds info tibble
    saveRDS(object = dbox_rds_info, file = rds_info_path)
    lapply(files$path_display, drop_download, local_path = local_data_dir, overwrite = TRUE,
           dtoken =  drop_auth(rdstoken = tokenpath))
  }

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
}

SUBMIT_CSV <- function(zone, drop_path = "BRCWQDM/Submitted_Data_Staging"){
 ### Args for interactive testing
  ### Need to add some error handling here
  # zone <- "Mid-Reach"
  ### Upload the data first
  csv <- stagedDataCSV # This is the full path
  if(file.exists(csv)){
  ### Get month of data
  data_date <- read.table(csv, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA") %>%
    slice(1) %>%
    pull(SampleDateTime)
 ### Calculate year and month from date of data
  mon <- month.abb[month(data_date)]
  yr <- year(data_date)

  fn <- paste0(zone,"_SubmittedData_", mon,"_", yr,".csv")
  file_path <- paste0(submittedDataDir, "/", fn)

  ### Make a copy of staged data csv - rename and put in submitted data folder
  file.copy(csv, paste0(submittedDataDir, "/", fn), overwrite = TRUE)
  ### Upload submitted data to dropbox
  # drop_exists(path = drop_path, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = file_path, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  ### Delete staged data csv
  # file.rename(fn, paste0(submittedDataDir,"/",fn))
  file.remove(csv)
  } else {
    print("There are no data to upload!")
  }
  csv <- stagedCommentsCSV

  ### Upload the Comments
  if(file.exists(csv) & read.table(csv, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA") %>% nrow() > 0) {
  ### Get month of data
  data_date <- read.table(csv, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA") %>%
    slice(1) %>%
    pull(DATE)

 ### Calculate year and month from date of data
  mon <- month.abb[month(data_date)]
  yr <- year(data_date)

  fn <- paste0(zone,"_SubmittedComments_", mon,"_", yr,".csv")
  file_path <- paste0(submittedDataDir, "/", fn)
  file.copy(csv, file_path, overwrite = TRUE)
  ### Upload submitted comments to dropbox
  # drop_exists(path = drop_path, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = fn, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  ### Delete the staged comments csv
  # file.rename(fn, paste0(submittedDataDir,"/",fn))
  file.remove(csv)
  } else {
    print("There were no comments to upload!")
  }
  return(print("File uploaded to Dropbox successful"))
}

# Submitted Data only fetched for Program Coordinator
GET_SUBMITTED_DATA <- function() {
if (user_role %in% c("Program Coordinator", "App Developer")){
  ### List Drop Box files ###
  dropb_root_dir <- config[12]
  safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = FALSE)
  dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"),
                              recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
 if (FALSE %in% dir_listing$result) {
    print("Dropbox connection failed trying to download submitted data csv files. Check internet connection and verify that csv files are on Dropbox in the correct location")
    } else {
  files <- drop_dir(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  # files <- dir_listing$result["name"] %>% unlist()
  if (length(files) > 0){
  files <- files$name
  }
  local_data_dir <- paste0(LocalDir,"Data/SubmittedData")
  if(files %>% length() %>% as.numeric() > 0){
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
} else {
  print(paste0(app_user, " is not the Program Coordinator or App Developer - submitted files not downloaded from Dropbox. User submitted files available locally"))
}
}
# GET_SUBMITTED_DATA()

GET_DATABASE_DATA <- function(){
  ### List Drop Box files ###
  dropb_root_dir <- config[12]
  safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = FALSE)
  dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/DB_Tables_RDS"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  if (FALSE %in% dir_listing$result) {
    print("Dropbox connection failed trying to download database RDS files. Check internet connection and verify database RDS files have been uploaded to the correct location on dropbox")
  } else {
    files <- dir_listing$result
      if(files$name %>% length() %>% as.numeric() > 0){ # There are db data rds files to get...
        local_data_dir <- paste0(LocalDir,"Data/rdsFiles")
        # # Get info from dropbox RDS files:
        db_info_path <- paste0(local_data_dir,"/db_info.rds")
        dbox_db_info <- tibble(`rds_file` = files$name, `timestamp` = files$server_modified)
        paths <- files$path_display
        ### Save all updated database RDS files to Local Data Cache ####
          if(file.exists(db_info_path)){ # File exists - compare each file - datestamp to the dropbox version, if drop
            # box is newer - download and overwrite, if not, then skip
            local_rds_info <- readRDS(db_info_path)
            for (i in seq_along(paths)) {
              fn <- files$name[i]
              ts <- files$server_modified[i]
              if(fn %in% local_rds_info$rds_file){
                  if(local_rds_info$timestamp[local_rds_info$rds_file == fn] == ts){
                    print(paste0(fn, " already up to date, skipping download and moving to next file."))
                    next
                  } else {
                    print(paste0(fn, " out of date, downloading more recent version from Dropbox."))
                    drop_download(path = paths[i], local_path = local_data_dir, overwrite = TRUE,
                                  dtoken =  drop_auth(rdstoken = tokenpath))
                  } # End Else
              } else { # Dropbox file has no local version
                print(paste0(fn, " does not exist locally, downloading from Dropbox."))
                drop_download(path = paths[i], local_path = local_data_dir, overwrite = TRUE,
                              dtoken =  drop_auth(rdstoken = tokenpath))
                local_rds_info <- local_rds_info %>%
                  add_case(rds_file = fn, timestamp = ts )
              }
            } # End loop
            saveRDS(object = dbox_db_info, file = db_info_path)
          } else { # rds info file does not exist, download all rds files (overwrite) as well as the rds info tibble
            saveRDS(object = dbox_db_info, file = db_info_path)
            lapply(paths, drop_download, local_path = local_data_dir, overwrite = TRUE,
                   dtoken =  drop_auth(rdstoken = tokenpath))
            print("No database file information available, downloading all files.")
          }
      } else {
        print("There were no database rds files found on Dropbox")
      }
  }
} # End Function
# GET_DATABASE_DATA()

ARCHIVE_SUBMITTED_DATA <- function(data_file){
 ### List Drop Box files ###
d_file <- str_replace(data_file, pattern = paste0(submittedDataDir,"/"),"")
c_file <- str_replace(d_file,"_SubmittedData_","_SubmittedComments_")

### List local csv files

d_csv <- str_replace(data_file, pattern = "SubmittedData","Imported_Data")
c_csv <- str_replace(d_csv,"_SubmittedData_","_SubmittedComments_")

dropb_root_dir <- config[12]
# From/To paths

data_from_path <-  paste0(dropb_root_dir, "/Submitted_Data_Staging/", d_file)
comment_from_path <- paste0(dropb_root_dir, "/Submitted_Data_Staging/", c_file)
data_to_path <-  paste0(dropb_root_dir, "/Imported_Data_Archive/", d_file)
comment_to_path <-   paste0(dropb_root_dir, "/Imported_Data_Archive/", c_file)
drop_path <- "BRCWQDM/Imported_Data_Archive"

# print(data_from_path)
# print(comment_from_path)

safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = FALSE)
dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
if (FALSE %in% dir_listing$result) {
  showModal(modalDialog(
    title = "Submitted file archive failure...",
    HTML("<h4>Submitted data files on Dropbox could not be archived!<br/>
         These files will have to be moved manually.</h4>")
  ))
  } else {
  files <- drop_dir(path = paste0(dropb_root_dir, "/Submitted_Data_Staging"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))

      if (data_from_path %in% files$path_display & file.exists(d_csv)) {
      drop_upload(file = d_csv, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))

      drop_delete(path = data_from_path, dtoken = drop_auth(rdstoken = tokenpath))
      print(paste0("The data csv file '", d_file, "'  was moved to the Imported Data Archive folder"))
    } else {
      print("The submitted data file on Dropbox was not found and could not be moved to the archive folder!")
    }

    if (comment_from_path %in% files$path_display & file.exists(c_csv)) {
      drop_upload(file = c_csv, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))

      drop_delete(path = comment_from_path, dtoken = drop_auth(rdstoken = tokenpath))
      print(paste0("The comment csv file '", c_file, "'  was moved to the Imported Data Archive folder"))
    } else {
      print("The submitted comment file on Dropbox was not found and could not be moved to the archive folder!")
    }
  }
} # end function

# Run function manually:
### input$selectFile in shiny app enter SubmittedData filename to use manually
# ARCHIVE_SUBMITTED_DATA(data_file = "Mid-Reach_SubmittedData_2019-09-12.csv")
# data_file <-  paste0(submittedDataDir,"/Mid-Reach_SubmittedData_2019-09-12.csv")
# data_file

UPLOAD_RDS <- function(){
### This function will upload the database RDS files to Dropbox
### This should be called after any updates to site, people, parameter, or assignment tables

dropb_root_dir <- config[12]
drop_path <- "BRCWQDM/AppFiles"

  sitesRDS <- paste0(LocalDir,"Data/rdsFiles/sites_db.rds")
  peopleRDS <- paste0(LocalDir,"Data/rdsFiles/people_db.rds")
  parametersRDS <- paste0(LocalDir,"Data/rdsFiles/parameters_db.rds")
  assignmentsRDS <- paste0(LocalDir,"Data/rdsFiles/assignments_db.rds")


  drop_upload(file = sitesRDS, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = peopleRDS, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = parametersRDS, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = assignmentsRDS, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
return("RDS files successfully copied to Dropbox")
}

UPLOAD_DB_DATA_RDS <- function(){
### This function will upload the database RDS files to Dropbox - this should

dropb_root_dir <- config[12]
drop_path <- "BRCWQDM/DB_Tables_RDS"

safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = FALSE)
dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/DB_Tables_RDS"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
  if (FALSE %in% dir_listing$result) {
    showModal(modalDialog(
      title = "Database data RDS file upload failed...",
      HTML("<h4> New database data will not be available in the app. <br/>
         Database data RDS file update will have to be done manually. Contact the app developer.</h4>")
    ))
  upload_msg <- "Database RDS files not uploaded to dropbox!"
} else {
  data_num_rds <-  paste0(LocalDir,"Data/rdsFiles/data_num_db.rds")
  data_text_rds <- paste0(LocalDir,"Data/rdsFiles/data_text_db.rds")
  data_comment_rds <-  paste0(LocalDir,"Data/rdsFiles/data_comment_db.rds")
  data_trans_log_rds <-  paste0(LocalDir,"Data/rdsFiles/trans_log_db.rds")

  drop_upload(file = data_num_rds, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = data_text_rds, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = data_comment_rds, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  drop_upload(file = data_trans_log_rds, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  upload_msg <- "Data RDS files successfully copied to Dropbox"
}
return(upload_msg)
}
# UPLOAD_DB_DATA_RDS()

BACKUP_DATABASE <- function(){
  db_path <- paste0(dataDir,"BRC_Database/BRCWQDB.sqlite")
  ### Dropbox backup directory:
  dropb_root_dir <- config[12]
  drop_path <- "BRCWQDM/DB_Backups"
  fn <- paste0(dataDir,"BRC_Database/BRCWQDB_",today(),".sqlite")
  ### Overwrite filename so that it can be uploaded to dropbox
  file.copy(from = db_path, to = fn, overwrite = T, copy.mode = F)

  drop_upload(file = fn, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  ### Now delete the temp copy of the database?
  file.remove(fn)
  return(print("Database backed up to Dropbox 'DB_Backups' folder"))
}
# BACKUP_DATABASE()

UPLOAD_LOG <- function(){
  log_path <- paste0(LocalDir,"BRCWQDM.log")
  ### Dropbox backup directory:
  dropb_root_dir <- config[12]
  drop_path <- "BRCWQDM/User_logs"
  fn <- glue("{LocalDir}{app_user}_AppLog_{Sys.Date()}.log")
  ### Overwrite filename so that it can be uploaded to dropbox
  file.copy(from = log_path, to = fn, overwrite = T, copy.mode = F)

  drop_upload(file = fn, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  ### Now delete the temp copy of the database?
  file.remove(fn)
  return(print("App log uploaded to Dropbox"))
}

UPLOAD_PHOTO <- function(file, name) {
  ### Dropbox backup directory:
  dropb_root_dir <- config[12]
  drop_path <- "BRCWQDM/Photos"
  fn <- name
  ### Overwrite filename so that it can be uploaded to dropbox
  file.copy(from = file, to = fn, overwrite = T, copy.mode = F)

  drop_upload(file = fn, path = drop_path, mode = "overwrite",
              verbose = TRUE, dtoken = drop_auth(rdstoken = tokenpath))
  ### Now delete the temp copy of the database?
  file.remove(file)
  return(print(glue("Photo {fn} uploaded to Dropbox")))
}

GET_PHOTO <- function(photo){
  # photo <- "C-02-03-040_2020-07-13_GEN_1594688860.jpg" ### Test image
  local_data_dir <- paste0(LocalDir,"Data/photos")
  if(dir.exists(local_data_dir)) {
    print("Checking local photos...")
  } else {
    print("There are no local photos...")
    dir_create(local_data_dir)
  }
  file <- paste0(local_data_dir, "/", photo)
  if (file_exists(file)) {
    return(paste0("Photo '", photo, "' already exists locally, skipping dropbox download..."))
  } else { # Get photo from dropbox
    ### List Drop Box files ###
    dropb_root_dir <- config[12]
    drop_path <- "BRCWQDM/Photos"
    db_photo <- paste0(dropb_root_dir, "/Photos/", photo)
    safe_dir_check <- purrr::safely(drop_dir, otherwise = FALSE, quiet = FALSE)
    dir_listing <- safe_dir_check(path = paste0(dropb_root_dir, "/Photos"), recursive = FALSE, dtoken = drop_auth(rdstoken = tokenpath))
    if (FALSE %in% dir_listing$result) {
      return("Dropbox connection failed trying to download photo file. Check internet connection and verify requested photo is on dropbox")
    } else {
      files <- dir_listing$result
      if(db_photo %in% files$path_display) {
        # Download the photo
        drop_download(path = db_photo, local_path = local_data_dir, overwrite = TRUE,
                      dtoken =  drop_auth(rdstoken = tokenpath))
        return("Photo file downloaded successfully!...")
      } else {
        return("The requested file is not in the dropbox photo archive - please contact database administrator...")
      }
    } # End if
  } # End if
} # End Function

# GET_PHOTO(photo)

