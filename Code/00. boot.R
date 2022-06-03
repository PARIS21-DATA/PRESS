################################################################################
#
# Boot file for PRESS 
# Author: Yu Tian, Johannes Abele
# Date: 05/10/2022
#
# Objective: Load all packages necessary for PRESS methodology
#            
# 
# input files: -
#              
#
# output file: - 
#
#
################################################################################


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
    "text2vec",
    "tm",
    "textstem",
    "textcat",
    "textclean",
    "lexicon",
    "quanteda",
    "deeplr",
    "stringdist"
  )

# Install uninstalled packages
#lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)
rm(packages)

# Set wd
setwd(getwd())

# Clean up memory
gc()

# source("data/functions.R")

