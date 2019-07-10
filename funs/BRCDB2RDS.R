### GENERATE RDS FILES FROM BRC DATABASE ####

library("RSQLite")
library("DBI")
library("dplyr")
library("magrittr")
library("pool")
library("dbplyr")

#Read config file
config <- read.csv(paset0(wdir,"/BRC_config.csv", header = TRUE, stringsAsFactors = FALSE))
config <- as.character(config$config_value)
wdir <- config[1]
### Path to the synced database (change to config[i])
paste0(config[1],"data/BRC_Database/",config[6])
### Make the db connection
pool <- dbPool(drv = RSQLite::SQLite(), dbname = db)

dbListTables(pool)

sites_db<- dbReadTable(pool, "sites")
people_db <- dbReadTable(pool, "people")
parameters_db <- dbReadTable(pool, "parameters")
assignments_db <- dbReadTable(pool,"site_assignments")

poolClose(pool)
rm(db)

saveRDS(sites_db, paste0(wdir,"/data/rdsFiles/sites_db.rds"))
saveRDS(people_db, paste0(wdir,"/data/rdsFiles/people_db.rds"))
saveRDS(parameters_db, paste0(wdir,"/data/rdsFiles/parameters_db.rds"))
saveRDS(assignments_db, paste0(wdir,"/data/rdsFiles/assignments_db.rds"))
