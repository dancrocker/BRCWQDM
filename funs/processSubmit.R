#####################################  HEADER  ################################
# SCRIPTNAME: process.R
# DESCRIPTION: Processes brc data and comments from staging files, error/qc checks
# WRITTEN BY: Dan Crocker
# DATE OF LAST UPDATE:
##############################################################################.


### This function will process staged data and comments prior to submittal
### Processing includes:
# Checking for valid entries
# Adding "Not-Recorded" for blank/NULL/NA entries

# Processed data frames will display below the action buttons
# Any data errors will display and prevent data submit button

PROCESS1 <- function(){
### Read the staged data and comments from the csv
df_data <- read.table(stagedDataCSV, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")

t_fields <- table_fields %>%
 filter(row_number() <= 35)

# Which columns have numeric data?
num_cols <- t_fields$shiny_input[t_fields$col_type == "numeric"]
text_cols <- t_fields$shiny_input[t_fields$col_type %in% c("text","factor")]
text_cols <- text_cols[text_cols != "photos"]

### Convert empty numeric records to  -999999
df_data <- df_data %>% mutate_at(num_cols, ~replace(., is.na(.), -999999))
df_data <- df_data %>% mutate_at(text_cols, ~str_replace(., "FALSE",  "Not Recorded"))

### Convert all blanks, NA, and NULLs to "Not Recorded"

df_data[is.na(df_data)] <- "Not Recorded"
df_data[df_data == ""] <- "Not Recorded"
df_data[df_data == "NULL"] <- "Not Recorded"

### Perform any other checks on data here:

### Overwrite the csv with the updates:
write.table(x = df_data, file = stagedDataCSV,
            row.names = FALSE, na = "", quote = TRUE,
            qmethod = "d", append = FALSE)

### PROCESS COMMENTS ####
if (file.exists(stagedCommentsCSV)) {
  df_comments <- read.table(stagedCommentsCSV, stringsAsFactors = FALSE, header = T,  sep = " " , na.strings = "NA")
  if (nrow(df_comments) > 0) {
  ### Do any manipulations needed here...

  ### Overwrite the csv with the updates:
  write.table(x = df_comments, file = stagedCommentsCSV,
              row.names = FALSE, na = "", quote = TRUE,
              qmethod = "d", append = FALSE)

  } else {
    df_comments <<- NULL
  }

} else {
  df_comments <<- NULL
}

dfs <- list()
dfs$data <- df_data
dfs$comments <- df_comments

return(dfs)
}