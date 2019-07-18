library(sendmailR)

#set working directory
setwd(config[1])

# Senders email:
from_name <- app_user
from_name <- "Mike Sperry" ### Testing line - delete when in actual use

people_db$EMAIL[people_db$FULL_NAME == from_name]


#####send plain email

from <- config[4]
to <- ""
subject <- "Testing scripted email"
body <- "Just testing this method out"
mailControl=list(smtpServer=" smtp.gmail.com")

sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)

#####send same email with attachment

#needs full path if not in working directory
attachmentPath <- ""

#same as attachmentPath if using working directory
attachmentName <- "BRC_Data_2019-04-04.csv"

#key part for attachments, put the body and the mime_part in a list for msg
attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
bodyWithAttachment <- list(body,attachmentObject)

sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)

### mailR Method ####

#install.packages("mailR")
library(mailR)
sender <- "your_email@gmail.com"
recipients <- c("recipient_email@gmail.com")
send.mail(from = sender,
          to = recipients,
          subject = "Test mail from Rstudio",
          body = "Test email body",
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "your_email@gmail.com",
                      passwd = "your_email_password", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
