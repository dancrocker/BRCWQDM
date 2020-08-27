################################### HEADER ###################################
#  TITLE: sendEmail.R
#  DESCRIPTION: Script to send emails using mailR package
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: July, 2019
#  GIT REPO:
#  R version 3.4.4 (2018-03-15)  x86_64
##############################################################################.

library(curl)
library(lubridate)
library(glue)
library(magrittr)
library(dplyr)
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
  ad <- filter(assignments_db, YEAR == yr,
               ROLE == "App Developer") %>% .$NAME
  # Get program coordinator email
  pc_email <- people_db$EMAIL[people_db$FULL_NAME == pc]
  ad_email <- people_db$EMAIL[people_db$FULL_NAME == ad]
  # Person submitting data name
  from_name <- app_user
  # Person submitting data email address
  from_email <- people_db$EMAIL[people_db$FULL_NAME == from_name]

  # Recipients
  if(isTRUE(rxdata$t_mode)){
    distro <- c(from_email) %>% unique() # Need to add ST here once app is live
  } else {
    distro <- c(from_email, pc_email, ad_email) %>% unique() # Need to add ST here once app is live
  }

message <- glue('From: "BRCWQDM App" <BlackstoneWQMdata@gmail.com>
Subject: New BRC water quality data has been submitted

{from_name} has just submitted data for review and entry into the BRCWQDM Database.')

recipients <- distro
sender <- config[4]
username <- config[4]
password <- config[5]

curl::send_mail(mail_from = sender, mail_rcpt = recipients, smtp_server = 'smtp.gmail.com',
  message = message, username = username, password = password, use_ssl = T, verbose = F)

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
 # Get app developer name
  ad <- filter(assignments_db, YEAR == yr,
               ROLE == "App Developer") %>% .$NAME
  # Get program coordinator email
  pc_email <- people_db$EMAIL[people_db$FULL_NAME == pc]
  ad_email <- people_db$EMAIL[people_db$FULL_NAME == ad]
    # Get field coordinator names
  fc <- filter(assignments_db, YEAR == yr,
               ROLE == "Field Coordinator") %>% .$NAME
  # Get field coordinator names
  fc_email <- people_db$EMAIL[people_db$FULL_NAME %in% fc]

  # Person importing data name
  from_name <- app_user
  # Person submitting data email address
  from_email <- people_db$EMAIL[people_db$FULL_NAME == from_name]

  # Recipients
  if(isTRUE(rxdata$t_mode)){
    distro <- from_email
  } else {
    distro <- c(from_email, pc_email, fc_email, ad_email) %>% unique() # Need to add  fc_email here once app is live
  }

message <- glue('From: "BRCWQDM App" <BlackstoneWQMdata@gmail.com>
Subject: New water quality data added to the BRCWQDM Database

{from_name} has just imported new data to the BRCWQDM Database.')

recipients <- distro
sender <- config[4]
username <- config[4]
password <- config[5]

curl::send_mail(mail_from = sender, mail_rcpt = recipients, smtp_server = 'smtp.gmail.com',
  message = message, username = username, password = password, use_ssl = T, verbose = F)


} # End function


########################################################################.
###                          TEST EMAIL                           ####
########################################################################.

### Function to send email when data is imported to database
### distro needs to be db admin, program coordinator, and 3 field coordinator

testEmail <- function(){
  yr <- year(Sys.Date())

  # Get program coordinator name
  pc <- filter(assignments_db, YEAR == yr,
               ROLE == "Program Coordinator") %>% .$NAME
 # Get app developer name
  ad <- filter(assignments_db, YEAR == yr,
               ROLE == "App Developer") %>% .$NAME

  ad_email <- people_db$EMAIL[people_db$FULL_NAME == ad]
    # Get field coordinator names

  # Person importing data name
  from_name <- app_user
  # Person submitting data email address
  from_email <- people_db$EMAIL[people_db$FULL_NAME == from_name]


  distro <- c(from_email, ad_email) %>% unique() # Need to add  fc_email here once app is live


message <- glue('From: "BRCWQDM App" <BlackstoneWQMdata@gmail.com>
Subject: This is a testing email

{from_name} was able to generate email through the BRCWQDM App!')

recipients <- distro
sender <- config[4]
username <- config[4]
password <- config[5]

curl::send_mail(mail_from = sender, mail_rcpt = recipients, smtp_server = 'smtp.gmail.com',
  message = message, username = username, password = password, use_ssl = T, verbose = F)

} # End function


