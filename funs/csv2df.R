###############################################################################.
#  TITLE: csv2df.R
#  DESCRIPTION: File formatting function for BRC Shiny App
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2023-10-20
#  GIT REPO:
#  R version 3.6.3 (2020-02-29)  x86_64
##############################################################################.

### This fetches staged data and comments and formats
### if the staged data does not have all necessary columns then
### mutate them on and re-save csv


data_csv2df <- function(data, data_fields, file) {

  ### Read the RDS File
  mydata <- data
  # mydata <- stagedData
  # file <- stagedDataCSV

#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*

# NOTE: This section is only needed until all 2023 data is imported and new
#  data conforms to updated database - 35 columns

   ### Rename E. coli Replicate to E. coli Field Replicate
  if("e_coli_rep" %in% names(mydata)) {
    mydata <- mydata %>%
      dplyr::rename("e_coli_field_rep" = "e_coli_rep")
  }

  ### Add the 4 new columns to the staged data file, if they are not there already and resave file
  cols <- c("aql_e_coli_field_rep", "aql_e_coli_lab_rep", "aql_e_coli", "e_coli_lab_rep")

  # Function to add missing columns
  add_cols <- function(df, cols) {
    add <- cols[!cols %in% names(df)]
    if(length(add) !=0 ) df[add] <- NA
    return(df)
  }
  ### Add the columns
  mydata <- add_cols(df = mydata, cols = cols)
  ### Reorder columns
  mydata <- mydata[ ,data_fields$shiny_input]

  write.table(x = mydata, file = file,
                row.names = FALSE, col.names = data_fields$shiny_input, na = "", quote = TRUE,
                qmethod = "d", append = FALSE)
  #.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*#.*

  display_names <- which(table_fields$shiny_input %in% names(mydata))

  names(mydata) <- table_fields$dt_cols[display_names]

  new_cols <- c("E. coli Lab Replicate" = NA_real_,
                "E. coli AQL" = NA,
                "E. coli Field Rep AQL" = NA,
                "E. coli Lab Rep AQL" = NA
  )
  ### Add the new columns to the data if they are not there
  ### note: the !!! (bang bang bang) unpacks the new_cols list so the function doesn't fail
  for(i in c(1:4)) {
    if(!names(new_cols[i]) %in% names(mydata)) {
      mydata <- mydata %>%
        as_tibble() %>%
        add_column(!!!new_cols[i], .name_repair = "minimal")
    }
  }

  # mydata$`E. coli AQL` <- as.logical(mydata$`E. coli AQL`)
  # mydata$`E. coli Field Rep AQL` <- as.logical(mydata$`E. coli Field Rep AQL`)
  # mydata$`E. coli AQL` <- as.logical(imydata$`E. coli AQL`)

  ### Order columns correctly for display
  mydata <- mydata[ ,data_fields$dt_cols]
  ### Force back into dataframe
  mydata <- data.frame(mydata, stringsAsFactors = F, check.names = F)

  return(mydata)
}
# data <- read.table(stagedCommentsCSV, stringsAsFactors = FALSE, header = T)

comm_csv2df <- function(data, comment_fields){
  ### Read the RDS File
  mydata <- data
  names(mydata) <- as.character(comment_fields$dt_cols)
  ### Convert to Dataframe
  mydata <- data.frame(mydata, stringsAsFactors = F, check.names = F)

  return(mydata)
}

