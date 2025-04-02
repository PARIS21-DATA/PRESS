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
    "kableExtra",
    "rmarkdown",
    "feather" # for quick save and read of files
    , "digest" # hash value generator
    , "stringi" # Used for case-insensitive string matching
    , "scales" # for percentage format conversion
    , "gridExtra" # charts side by side
    , "grDevices" # create colours
  )

# Install uninstalled packages
lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)
rm(packages)

# Set wd
setwd(getwd())

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

