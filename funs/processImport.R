################################### HEADER ###################################
#  TITLE: import_brc.R
# DESCRIPTION: Processes brc data from staging file, error/qc checks, Imports
#              data to the SQLITE Database. Uses 2 separate functions,
#             must have up-to-date rds files and connection to SQLITE Database
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: June 2019
#  GIT REPO: dancrocker/BRCWQDM
#  R version 3.4.4 (2018-03-15)  x86_64
##############################################################################.

### LOAD PACKAGES ####
library("RSQLite")
library("DBI")
library("dplyr")
library("pool")
library("dbplyr")
library("tidyr")
library("magrittr")
library("stringr")

### ISSUES TO RESOLVE ####
  # Convert inches to ft for water depths or include both?
  # Average temperatures? from form?
  # Keep or toss qc records - now they are filtered out of final data
  # Data grades -opted not to include since it is a calculated value
  # How to handle non-detects - what do they come in like? Flag?
  # How to handle raw data archival - all in 1 folder? subfolders?

### CONFIG ####
# config # This is defined in the launch script

### Path to the synced database
db <- paste0(config[1],"Data/BRC_Database/",config[6])
### DB TABLES ####

### Make dfs from db tables ####
### Eventually these should be reading cached RDS Files from dropBox
DBdataNumTbl <- "data_num"
DBdataTextTbl <- "data_text"
DBdataCommentTbl <- "data_comments"
DBtransLog <- "trans_log"
DBparTbl <- "parameters"

### Make the db connection
pool <- dbPool(drv = RSQLite::SQLite(), dbname = db)

onStop(function() {
  poolClose(pool)
})

# dbListTables(pool)

# sites_db <- dbReadTable(pool, "sites")
# people_db <- dbReadTable(pool, "people")
parameters_db <- dbReadTable(pool, "parameters")
# assignments_db <- dbReadTable(pool,"site_assignments")
trans_log_db <- dbReadTable(pool,"trans_log")

### Get sample data - this will need to fetch 2 tables - numeric and text
data_num_db <- dbReadTable(pool, "data_num")
data_text_db <- dbReadTable(pool, "data_text")
data_comments_db <- dbReadTable(pool, "data_comments")

poolClose(pool)
### LOAD STAGED DATA ####
# eventually data should be passed in from shiny app.r

PROCESS2 <- function(file){
data_csv <- paste0(submittedDataDir,"/Mid-Reach_SubmittedData_2019-06-20.csv")
# data_csv <- input$selectFile
comment_csv <- str_replace(data_csv,"_SubmittedData_","_SubmittedComments_")

df_data <- read.table(data_csv, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
df_comments <- read.table(comment_csv, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
### Make the db connection
pool <- dbPool(drv = RSQLite::SQLite(), dbname = db)
onStop(function() {
  poolClose(pool)
})
### DB TBL COL ORDERS ####
col_data_num <- names(data_num_db)
col_data_text <- names(data_text_db)
col_data_comments <- names(data_comments_db)
col_trans_log <- names(trans_log_db)

### Get last transaction ID from trans_log
lastTransLogID <- trans_log_db$ID %>% max() %>% as.numeric()
if(is.na(last_trans_log_ID)) {
  last_trans_log_ID <- 0
}
### Get the last nueric data ID
lastNumID <- data_num_db$ID %>% max() %>% as.numeric()
if(!is.finite(lastNumID)){
  lastNumID <-  0
}
### Get the last text data ID
lastTextID <- data_text_db$ID %>% max() %>% as.numeric()
if(!is.finite(lastTextID)){
  lastTextID <-  0
}
### Get the last comment data ID
lastCommentID <- data_comments_db$ID %>% max() %>% as.numeric()
if(!is.finite(lastCommentID)){
  lastCommentID <-  0
}

### Get the last sample event ID - the new SEID starts at lastSEID + 1
lastSEID <- data_num_db$SEID %>% max() %>% as.numeric()
if(!is.finite(lastSEID)){
  lastSEID <-  0
}

### Transform staged data into tidy format
data <- df_data %>%
  arrange(SampleDateTime) %>%
  mutate("SEID" = lastSEID + row_number()) %>%
  gather(key = "PARAMETER", value = "RESULT", 4:ncol(.)-1) %>%
  arrange(SampleDateTime, SEID, PARAMETER) %>%
  mutate("UNIQUE_ID" = paste0(.$site,"_", .$SampleDateTime,"_",
                            parameters_db$ABBRV[match(.$PARAMETER, parameters_db$SHINY_OBJ)]),
         "IMPORT_DATE" = paste0(Sys.Date()),
         "IMPORTED_BY"= config[2]) %>%
  dplyr::rename("DATE_TIME" = SampleDateTime, "SITE_BRC_CODE" = site)

# Eliminate NA and NULL?
### Convert the logical values to integers: 1=TRUE 0=False
data$RESULT <- recode(data$RESULT, "TRUE" = "1", "FALSE" = "0")
data$PARAMETER <- parameters_db$PARAMETER_NAME[match(data$PARAMETER,parameters_db$SHINY_OBJ)]

### NA Parameters are those that don't match parameters table
# QC check boxes mostly
# Also water depth inches - convert to ft or leave separate?

### This filter removes those NA Parameter entries and RESULTS that are NA, blank, or NULL, add a DATE column to match comments
data <- data %>%
  filter(!is.na(PARAMETER), !is.na(RESULT), RESULT != "NULL", RESULT != "") %>%
  mutate("DATE" = str_trunc(.$DATE_TIME, width = 10, side = "right", ellipsis = ""))

### Split data by type - numerical vs text ####
num_pars <- parameters_db$PARAMETER_NAME[parameters_db$UNITS != "text"]
text_pars <- parameters_db$PARAMETER_NAME[parameters_db$UNITS == "text"]

########################################################################.
###                           DATA_N                                ####
########################################################################.

### Data types/col order ####
data_n <- data[data$PARAMETER %in% num_pars,]
data_n$RESULT <- as.numeric(data_n$RESULT)
data_n <- data_n %>% mutate("ID" = lastNumID + row_number())

### REMOVE/SORT COLS FROM DATA_N ####
data_n <- data_n %>%
  mutate("UNITS" = parameters_db$UNITS[match(.$PARAMETER, parameters_db$PARAMETER_NAME)]) %>%
  select(-c("IMPORTED_BY","IMPORT_DATE")) %>%
  select(col_data_num)

########################################################################.
###                           DATA_T                                ####
########################################################################.

data_t <- data[data$PARAMETER %in% text_pars, c("SEID","PARAMETER","RESULT")]
data_t <- data_t %>%
  arrange(SEID, PARAMETER) %>%
  mutate("ID" = lastTextID + row_number()) %>%
  select(col_data_text)

########################################################################.
###                           DATA_C                              ####
########################################################################.

# First make sure there are comments
if(!is.null(df_comments)){
### Match the comments to data_t or data_n records
# Change parameter names back to shiny inputs then convert to database parameter names
data_c <- df_comments
data_c$PARAMETER[data_c$PARAMETER != "General Comment"] <- table_fields$shiny_input[match(data_c$PARAMETER[data_c$PARAMETER != "General Comment"], table_fields$dt_cols)]
data_c$PARAMETER[data_c$PARAMETER != "General Comment"] <- parameters_db$PARAMETER_NAME[match(data_c$PARAMETER[data_c$PARAMETER != "General Comment"], parameters_db$SHINY_OBJ)]
# at this point there should only be NAs for same # as GenComm
if(data_c$PARAMETER[is.na(data_c$PARAMETER)] %>% length() %>%  as.numeric() > 0){
  stop("There are non-matching shiny-objects or parameter names that need to be resolved before proceeding!")
}
# Bring in SEID from data using a join, add ID col, reduce cols to DB cols and rename
data_c <- data_c %>%
  left_join(data, by = c("SITE" = "SITE_BRC_CODE", "DATE", "PARAMETER")) %>%
  dplyr::rename("COMMENT_TEXT" = "COMMENT") %>%
  arrange(SEID, PARAMETER) %>%
  mutate("ID" = lastCommentID + row_number()) %>%
  select(col_data_comments)
} else {
  df_comments <- NULL
}

########################################################################.
###                    TRANSACTION LOG                              ####
########################################################################.
trans_log <- data %>%
  select("SEID","IMPORTED_BY","IMPORT_DATE") %>%
  distinct() %>%
  arrange(SEID) %>%
  mutate("ID" = last_trans_log_ID + row_number()) %>%
  select(col_trans_log)


# Add all dfs to import to a list

dfs <- list()
  dfs$data_n <- data_n
  dfs$data_t <- data_t
  dfs$data_c <- data_c
  dfs$trans_log <- trans_log

return(dfs)
}

dfs_to_import <- PROCESS2(file = file)

IMPORT_DATA <- function(dfs_to_import){

### Make the db connection
pool <- dbPool(drv = RSQLite::SQLite(), dbname = db)

dbWithTransaction(con, {
    dbWriteTable(pool, DBdataNumTbl, value = dfs$data_n, append = TRUE)
    dbWriteTable(pool, DBdataTextTbl, value = dfs$data_t, append = TRUE)
    dbWriteTable(pool, DBdataCommentTbl, value = dfs$data_c, append = TRUE)
    dbWriteTable(pool, DBtransLog, value = dfs$trans_log, append = TRUE)
  })



# Write db files to Dropbox

newDataNum <- dbReadTable(pool, DBdataNumTbl)
newDataText <- dbReadTable(pool, DBdataTextTbl)
newDataComment <- dbReadTable(pool, DBdataCommentTbl)

# saveRDS(newDataNum, file = config[?])
# saveRDS(newDataText, file = config[?])




# Send email notice

poolClose(pool)



# Move the processed raw data file to the processed folder

  # processed_subdir <- paste0("/", max(year(df.wq$SampleDateTime))) # Raw data archived by year, subfolders = Year
  # processed_dir <- paste0(processedfolder, processed_subdir)
  # file.rename(path, paste0(processed_dir,"/", file))

# Generate new RDS files from database and write to DropBox shared folder or email?

  return(dfs)
}
