# library(reshape2) #??? is it necessary?
# library(splitstackshape)
# library(plyr)
# only one of the packages are needed
# library(cldr)
# library(cld2)
# library(cld3)
# library(slam)


# Load required libraries
packages <-
  c(
    "tidyverse",
    "readxl",
    "openxlsx",
    "reader",
    "zoo", 
    "countrycode", 
    "beepr", 
    "cld2", 
    "koRpus", 
    "SnowballC", 
    "tidytext"
  )
# Install uninstalled packages
lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)
rm(packages)

# Set wd
setwd(getwd())



# source("data/functions.R")

