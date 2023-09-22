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
    "tidytext", 
    "stringdist",
    "tm", 
    "data.table", 
    "feather" # for quick save and read of files
    , "digest" # hash value generator
  )
# Install uninstalled packages
lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)
rm(packages)

# Set wd
setwd(getwd())
# extremely important for mac os:
# if(Sys.info()[1]=="Darwin") Sys.setlocale("LC_ALL", "C")

# source("data/functions.R")
print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

source("code/00.1a functions_stem_and_concat.R")
source("code/00.1b function_ts_summary.R")
