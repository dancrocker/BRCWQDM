################################### HEADER ###################################
#  TITLE: data_update.R
#  DESCRIPTION: Script to update data rds files from BRCWQDM database
#   -called after new data imported
#  AUTHOR(S):
#  DATE LAST UPDATED:
#  GIT REPO:
#  R version 3.4.4 (2018-03-15)  x86_64
##############################################################################.

### LOAD PACKAGES ####
library("RSQLite")
library("DBI")
library("dplyr")
library("pool")

DATA_UPDATE <- function(){
### Path to the synced database
db <- paste0(config[1],"Data/BRC_Database/",config[6])
### Make the db connection
pool <- dbPool(drv = RSQLite::SQLite(), dbname = db)

### Get sample data - this will need to fetch 2 tables - numeric and text
data_num_db <- dbReadTable(pool, "data_num")
data_text_db <- dbReadTable(pool, "data_text")
data_comments_db <- dbReadTable(pool, "data_comments")
trans_log_db <- dbReadTable(pool,"trans_log")

poolClose(pool)
rm(db)

# Save them in RDS folder in project directory (This is only so they can be uploaded to Dropbox)
saveRDS(data_num_db, file = paste0(config[1],"Data/rdsFiles/data_num_db.rds"))
saveRDS(data_text_db, file = paste0(config[1],"Data/rdsFiles/data_text_db.rds"))
saveRDS(data_comments_db, file = paste0(config[1],"Data/rdsFiles/data_comment_db.rds"))
saveRDS(trans_log_db, file = paste0(config[1],"Data/rdsFiles/trans_log_db.rds"))

### UPLOAD RDS FILES TO DROPBOX ####
source(paste0(wdir, "/funs/dropB.R"))
UPLOAD_DB_DATA_RDS()

return(print("Database rds files updated for numerical data, text data, comments, and transaction log"))
}

# DATA_UPDATE()
