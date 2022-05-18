################################################################################
#
# Sending emails to list of recipients from R  
# Author: Johannes Abele
# Date: 05/04/2022
#
# Objective: 
#            
#            
# 
# input files: - 
#              - 
#              - 
#              - 
#              
#
# output file: - 
#
#
################################################################################


# ------------------------------- Preparation ----------------------------------

# Clear environment
remove(list = ls())

# Set paths
data_path <- "./Data/"
files_raw <- list.files(data_path, pattern = ".csv$")

# Load libraries
library(tidyverse)
library(beepr)


#-------------------------- RDCOMClient try ------------------------------------
# Supposed to work for Outlook, but on newer versions of R (version 4.xxx) 
# CreateItem(0) crashes R session, SOLUTION: install from BSchamberger/RDCOMClient
# if devtools doesn't work try remotes

# Necessary package for devtools on OECD machines
# install.packages("brio")
library(remotes) 
library(devtools)

# Set the right path for Rtools, since for the manual installation from Github,
# the newest version is necessary
Sys.setenv(PATH = paste("//OECDMAIN/em_apps/R/Rtools-4.0", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "//OECDMAIN/em_apps/R/Rtools-4.0/mingw64/bin/")
devtools::install_github("BSchamberger/RDCOMClient", ref = "master", force = TRUE)
#remotes::install_github("BSchamberger/RDCOMClient", ref = "master", force = TRUE)

library(RDCOMClient)

Outlook <- COMCreate("Outlook.Application")

list_recipients <- read_lines("./Emails/recipients.txt", skip_empty_rows = TRUE)
email_body <- readLines("./Emails/email_body.txt")
email_body <- paste(email_body, collapse = "\n")
excel_files <- list.files(path = "./Tmp/XGBoost/Manual verification/")

for (i in 1:length(list_recipients)) {
  path_to_attachment <- paste0(getwd(), "/Tmp/XGBoost/Manual verification/", excel_files[i])
  Email = Outlook$CreateItem(0)
  Email[["to"]] = list_recipients[i]
  Email[["subject"]] = "[We need your help!] Making the PRESS methodology even smarter"
  Email[["body"]] = email_body
  Email[["attachments"]]$Add(path_to_attachment)
  Email$send()
}

#Email[["cc"]] = "giorgi.sharvadze@oecd.org"


#--------------------------- Via SMTP server -----------------------------------
# Does not work for Outlook so far 

install.packages("mailR")
library(mailR)

send.mail(from = "johannes.abele@oecd.org",
          to = "johannes.abele@ensae.fr",
          subject = "Hurensohn bist du",
          body = "Test emails body",
          smtp = list(host.name = "smtp.office365.com", port = 587,
                      user.name = "johannes.abele@ensae.fr",
                      passwd = "Azerty123456", tls = TRUE),
          authenticate = TRUE,
          send = TRUE)
