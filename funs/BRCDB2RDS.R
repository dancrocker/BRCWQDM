################################### HEADER ###################################
#  TITLE: BRCDB2RDS.R
#  DESCRIPTION: Generates RDS files from BRCWQDM database to local project dir
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: July 22, 2019
#  GIT REPO: BRCWQDM
#  R version 3.4.4 (2018-03-15)  x86_64
##############################################################################.
### GENERATE RDS FILES FROM BRC DATABASE ####

library("RSQLite")
library("DBI")
library("dplyr")
library("magrittr")
library("pool")
library("dbplyr")

MAKE_DB_RDS <- function(){
#Read config file
config <- read.csv(paste0(app_dir,"BRC_config.csv"), header = TRUE, stringsAsFactors = FALSE)
config <- as.character(config$config_value)
wdir <- getwd()
### Path to the synced database
db <- paste0(LocalDir,"Data/BRC_Database/", config[6])
### Make the db connection
pool <- dbPool(drv = RSQLite::SQLite(), dbname = db)
dbListTables(pool)

sites_db<- dbReadTable(pool, "sites")
people_db <- dbReadTable(pool, "people")
parameters_db <- dbReadTable(pool, "parameters")
assignments_db <- dbReadTable(pool,"assignments")

poolClose(pool)
rm(db)
saveRDS(sites_db, paste0(LocalDir,"Data/rdsFiles/sites_db.rds"))
saveRDS(people_db, paste0(LocalDir,"Data/rdsFiles/people_db.rds"))
saveRDS(parameters_db, paste0(LocalDir,"Data/rdsFiles/parameters_db.rds"))
saveRDS(assignments_db, paste0(LocalDir,"Data/rdsFiles/assignments_db.rds"))
source("funs/dropB.R")
# Upload the rds files to dropbox
UPLOAD_RDS()

return("Database RDS files successfully generated and uploaded to Dropbox")
} # End function

# MAKE_DB_RDS()
