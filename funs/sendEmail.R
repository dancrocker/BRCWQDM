################################### HEADER ###################################
#  TITLE: sendEmail.R
#  DESCRIPTION: Script to send emails using mailR package
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: July, 2019
#  GIT REPO:
#  R version 3.4.4 (2018-03-15)  x86_64
##############################################################################.

library(mailR)
library(lubridate)

#set working directory
# setwd(config[1])

########################################################################.
###                       SUBMIT EMAIL                              ####
########################################################################.

### Function to send email when data is submitted to program coordinator
### distro needs to be program coordinator and field coordinator submitting file (user)

submitEmail <- function(){

  yr <- year(Sys.Date())

  # Get program coordinator name
  pc <- filter(assignments_db, YEAR == yr,
               ROLE == "Program Coordinator") %>% .$NAME
  # Get app developer name
  # ad <- filter(assignments_db, YEAR == yr,
  #              ROLE == "App Developer") %>% .$NAME
  # Get program coordinator email
  pc_email <- people_db$EMAIL[people_db$FULL_NAME == pc]
  # ad_email <- people_db$EMAIL[people_db$FULL_NAME == ad]
  # Person submitting data name
  from_name <- app_user
  # Person submitting data email address
  from_email <- people_db$EMAIL[people_db$FULL_NAME == from_name]
  # Sending email acct
  sender <- config[4]
  # Recipients
  if(isTRUE(rxdata$t_mode)){
    distro <- c(from_email) %>% unique() # Need to add ST here once app is live
  } else {
    distro <- c(from_email, pc_email) %>% unique() # Need to add ST here once app is live
  }
  # Msg subject
  subj <- "New BRC water quality data has been submitted"
  # Msg body
  bod <- paste0(from_name, " has just submitted data for review and entry into the BRCWQDM Database.")

  # Send the message
  send.mail(from = paste0("<", sender, ">"),
            to =paste0(distro),
            subject = subj,
            body = bod,
            smtp = list(host.name = "smtp.gmail.com", port = 465,
                        user.name = config[4],
                        passwd = config[5], ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)

} # End function

########################################################################.
###                          IMPORT EMAIL                           ####
########################################################################.

### Function to send email when data is imported to database
### distro needs to be db admin, program coordinator, and 3 field coordinator

importEmail <- function(){
  yr <- year(Sys.Date())

  # Get program coordinator name
  pc <- filter(assignments_db, YEAR == yr,
               ROLE == "Program Coordinator") %>% .$NAME
  # Get program coordinator email
  pc_email <- people_db$EMAIL[people_db$FULL_NAME == pc]
    # Get field coordinator names
  fc <- filter(assignments_db, YEAR == yr,
               ROLE == "Field Coordinator") %>% .$NAME
  fc <- fc[1] # Delete when app is in full use
  # Get field coordinator names
  fc_email <- people_db$EMAIL[people_db$FULL_NAME %in% fc]

  # Person importing data name
  from_name <- app_user
  # Person submitting data email address
  from_email <- people_db$EMAIL[people_db$FULL_NAME == from_name]
  # Sending email acct
  sender <- config[4]
  # Recipients
  if(isTRUE(rxdata$t_mode)){
    distro <- from_email
  } else {
    distro <- c(from_email, pc_email, fc_email) %>% unique() # Need to add  fc_email here once app is live
  }

  # Msg subject
  subj <- "New water quality data added to the BRCWQDM Database"
  # Msg body
  bod <- paste0(from_name, " has just imported new data to the BRCWQDM Database.")

  # Send the message
  send.mail(from = paste0("<", sender, ">"),
            to =paste0(distro),
            subject = subj,
            body = bod,
            smtp = list(host.name = "smtp.gmail.com", port = 465,
                        user.name = config[4],
                        passwd = config[5], ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)

} # End function
