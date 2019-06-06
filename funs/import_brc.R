#####################################  HEADER  ################################
# SCRIPTNAME: import_brc.R
# DESCRIPTION: Processes brc data from staging file, error/qc checks, Imports
#              data to the SQLITE Database. Uses 2 separate functions,
#             must have up-to-date rds files and connection to SQLITE Database
# WRITTEN BY: Dan Crocker
# DATE OF LAST UPDATE:
##############################################################################.

### LOAD PACKAGES ####
library("RSQLite")
library("DBI")
library("dplyr")
library("dbplyr")
library("tidyr")
library("magrittr")

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
db <- paste0(config[1],"/data/",config[5])

### DB TABLES ####

### Make dfs from db tables ####
### Eventually these should be reading cached RDS Files from dropBox
DBdataNumTbl <- "data_num"
DBdataTextTbl <- "data_text"
DBtransLog <- "trans_log"
DBparTbl <- "parameters"

### DB CON ####
con <- dbConnect(RSQLite::SQLite(), db)

### Get parameter table
pars <- dbReadTable(con, DBparTbl)

### Get sample data - this will need to fetch 2 tables - numeric and text
DBdata_num <- dbReadTable(con, DBdataNumTbl)
DBdata_text <- dbReadTable(con, DBdataTextTbl)
dbDisconnect(con)
### LOAD STAGED DATA ####
# eventually data should be passed in from shiny app.r
recs <- loadData()

PROCESS_DATA <- function(recs){

### DB CON ####
con <- dbConnect(RSQLite::SQLite(), db)
### Get last transaction ID from trans_log
qry_trans_log <- dbGetQuery(con, paste0("SELECT max(ID) FROM ", DBtransLog, ";"))
if(is.na(qry_trans_log)) {
  last_trans_log_ID <- 0
}
last_trans_log_ID <- as.numeric(unlist(qry_trans_log))
rm(qry_trans_log)
### DB TBL COL ORDERS ####
col_data_num <- dbListFields(con, DBdataNumTbl)
col_data_text <- dbListFields(con,DBdataTextTbl)

dbDisconnect(con)
### Get the last nueric data ID
lastNumID <- as.numeric(max(DBdata_num$ID, na.rm = TRUE))
if(!is.finite(lastNumID)){
  lastNumID <-  0
}
### Get the last text data ID
lastTextID <- as.numeric(max(DBdata_text$ID, na.rm = TRUE))
if(!is.finite(lastTextID)){
  lastTextID <-  0
}

### Get the last sample event ID - the new SEID starts at lastSEID + 1
lastSEID <- as.numeric(max(DBdata_num$SEID, na.rm = TRUE))
if(!is.finite(lastSEID)){
  lastSEID <-  0
}

### Transform staged data into tidy format
data <- recs %>%
  arrange(SampleDateTime) %>%
  mutate("SEID" = lastSEID + row_number()) %>%
  gather(key = "PARAMETER", value = "RESULT", 4:ncol(.)-1) %>%
  arrange(SampleDateTime, SEID, PARAMETER) %>%
  mutate("UNIQUE_ID" = paste0(.$site,"_", .$SampleDateTime,"_",
                            pars$ABBRV[match(.$PARAMETER, pars$SHINY_OBJ)]),
         "IMPORT_DATE" = paste0(Sys.Date()),
         "IMPORTED_BY"= config[2]) %>%
  dplyr::rename("DATE_TIME" = SampleDateTime, "SITE_BRC_CODE" = site)

# Eliminate NA and NULL?
### Convert the logical values to integers: 1=TRUE 0=False
data$RESULT <- recode(data$RESULT, "TRUE" = "1", "FALSE" = "0")
data$PARAMETER <- pars$PARAMETER_NAME[match(data$PARAMETER,pars$SHINY_OBJ)]

### NA Parameters are those that don't match parameters table
# QC check boxes mostly
# Also water depth inches - convert to ft or leave separate?

### This filter removes those NA Parameter entries and RESULTS that are NA, blank, or NULL
data <- data %>%
  filter(!is.na(PARAMETER), !is.na(RESULT), RESULT != "NULL", RESULT != "")

### Split data by type - numerical vs text ####
num_pars <- pars$PARAMETER_NAME[pars$UNITS != "text"]
text_pars <- pars$PARAMETER_NAME[pars$UNITS == "text"]

### Data types/col order ####
data_n <- data[data$PARAMETER %in% num_pars,]
data_n$RESULT <- as.numeric(data_n$RESULT)
data_n <- data_n %>% mutate("ID" = lastNumID + row_number())

data_t <- data[data$PARAMETER %in% text_pars, c("SEID","PARAMETER","RESULT")]
data_t <- data_t %>%
  arrange(SEID, PARAMETER) %>%
  mutate("ID" = lastTextID + row_number()) %>%
  select(col_data_text)

### TRANSACTION LOG ####
trans_log <- data_n %>%
  select("SEID","IMPORTED_BY","IMPORT_DATE") %>%
  distinct() %>%
  arrange(SEID) %>%
  mutate("ID" = last_trans_log_ID + row_number()) %>%
  select(c(4,1,2,3))

### REMOVE/SORT COLS FROM DATA_N ####
data_n <- data_n %>%
  mutate("UNITS" = pars$UNITS[match(.$PARAMETER, pars$PARAMETER_NAME)]) %>%
  select(-c("IMPORTED_BY","IMPORT_DATE")) %>%
  select(col_data_num)

dfs <- list()
  dfs$data_n <- data_n
  dfs$data_t <- data_t
  dfs$trans_log <- trans_log

return(dfs)
}

dfs <- PROCESS_DATA(recs)

IMPORT_DATA <- function(dfs){

### DB CON ####
con <- dbConnect(RSQLite::SQLite(), db)
dbWithTransaction(con, {
    dbWriteTable(con, DBdataNumTbl, value = dfs$data_n, append = TRUE)
    dbWriteTable(con, DBdataTextTbl, value = dfs$data_t, append = TRUE)
    dbWriteTable(con, DBtransLog, value = dfs$trans_log, append = TRUE)
  })

newDataNum <- dbReadTable(con, DBdataNumTbl)
newDataText <- dbReadTable(con, DBdataTextTbl)
dbDisconnect(con)

### Save new data to rds files for sharing via dropbox/email for analysis module
# saveRDS(newDataNum, file = config[?])
# saveRDS(newDataText, file = config[?])

# Move the processed raw data file to the processed folder
  # processed_subdir <- paste0("/", max(year(df.wq$SampleDateTime))) # Raw data archived by year, subfolders = Year
  # processed_dir <- paste0(processedfolder, processed_subdir)
  # file.rename(path, paste0(processed_dir,"/", file))

# Generate new RDS files from database and write to DropBox shared folder or email?

  return("Import Successful")
}

IMPORT_DATA(dfs)
