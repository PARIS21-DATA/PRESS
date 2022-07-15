################################################################################
#
# Data cleaning and adding text markers for CRS data 
# Author: Yu Tian, Johannes Abele
# Date: 05/10/2022
#
# Objective: 
#            
#            
# 
# input files: - /Data/intermediate/crs01_1_full.rds 
#              
#
# output file: - /Data/intermediate/crs02_full.rds
#
#
################################################################################


# ------------------------------- Preparation ----------------------------------

rm(list = ls())

# Load packages
source("./Code/00. boot.R")

source <- "crs"

# Set paths (change to sample to work with sample of CRS data)
crs_path <- "./Data/Intermediate/crs01_1_full.rds"
crs_path_new <- "./Data/Intermediate/crs02_full.rds"

# Load data
df_crs_raw <- readRDS(crs_path)
rm(crs_path)

#df_crs <- sample_n(df_crs_raw, size = nrow(df_crs_raw)/50) # take sample to speed up testing
#saveRDS(df_crs, "./Data/intermediate/crs01_1_sample.rds")


# Define all columns needed later on
cols_needed <- c("process_id", 
                 "crsid",
                 "year",
                 "projecttitle", 
                 "shortdescription", 
                 "longdescription", 
                 "donorname", 
                 "recipientname",
                 "purposecode",
                 "sectorname",
                 "sectorcode",
                 "channelcode",
                 "sdgfocus",
                 "gender",
                 "rmnch",
                 "usd_disbursement_defl")

# every step, we try to use a subset of the data to make the process quicker


#--------------------------- Data cleaning -------------------------------------

# Drop french part of the Canadian project titles/descriptions since they are in 
# the format "Englisch / French", 
df_crs_CAN <- df_crs_raw %>%
  select(all_of(cols_needed)) %>%
  filter(donorname == "Canada") %>%
  separate(projecttitle, into = c("projecttitle", "projecttitle_fr"), sep = "/", fill = "right", extra = "drop") %>%
  separate(shortdescription, into = c("shortdescription", "shortdescription_fr"), sep = "/", fill = "right", extra = "drop") %>%
  separate(longdescription, into = c("longdescription", "longdescription_fr"), sep = "/", fill = "right", extra = "drop") %>%
  mutate(longdescription_fr = NULL, shortdescription_fr = NULL, projecttitle_fr = NULL)

# Check same number of Canadian projects
nrow(df_crs_CAN)
nrow(df_crs_raw %>% filter(donorname == "Canada"))

# Replace all Canadian projects with the ones with English project titles/descriptions
df_crs_raw <- df_crs_raw %>%
  select(cols_needed) %>%
  filter(!(donorname == "Canada")) %>%
  rbind(df_crs_CAN)
rm(df_crs_CAN)


#----------------- Adding text id + description combination --------------------

# Define function to clean titles
clean_titles <- function(title){
  title <- title %>% 
    removeNumbers %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    tolower
  return(title)
}

# Create a unique text_id that is made from the combination of the project title, 
# short description and the long description 
# REMARK: uncomment below to use project title or short description if long description not available (using stringdist)
max_string_dist <- 10 # max string distance underneath which strings can be considered the same/differing just by a word
df_crs <- df_crs_raw %>%
  mutate(projecttitle = clean_titles(projecttitle),
         shortdescription = clean_titles(shortdescription),
         longdescription = clean_titles(longdescription)) %>%
  #mutate(description_comb = ifelse(stringdist(projecttitle, shortdescription) < max_string_dist, 
  #                                 projecttitle, 
  #                                 paste(projecttitle, shortdescription, sep = ". "))) %>%
  #mutate(description_comb = ifelse(stringdist(description_comb, longdescription) < max_string_dist, 
  #                                 description_comb, 
  #                                 paste(description_comb, longdescription, sep = ". "))) %>%
  #mutate(string_dist_title_long = stringdist(tolower(projecttitle), tolower(longdescription))) %>%
  mutate(text_id = as.numeric(as.factor(longdescription))) %>%
  mutate(description_comb = ifelse(stringdist(projecttitle, longdescription) < max_string_dist | str_count(longdescription) < 3, 
                                   NA, 
                                   longdescription)) %>% # try only long description for DTM process that are distinct from title
  mutate(scb = ifelse(purposecode==16062,1,0), # add SCB identifier
         pop = ifelse(purposecode==13010,1,0), 
         gen_ppcode = ifelse(purposecode %in% c(15170:15180), 1, 0), # add gender purpose code identifier
         gen_marker = ifelse(gender == 1 & !is.na(gender), 1, 0), # add gender marker (0 - no gender, 1 - gender primary purpose, 2 - gender secondary)
         gen_donor = ifelse(channelcode == 41146, 1, 0), # all projects from UN Women
         gen_sdg = str_detect(sdgfocus, "^5|,5")
         ) %>%
  select(-cols_needed[which(!cols_needed %in% c("process_id", "longdescription"))])

# Add text_id and description combination to the crs data
df_crs <- df_crs %>% 
  select(-longdescription) %>%
  right_join(df_crs_raw)

rm(df_crs_raw)

#Output::
saveRDS(df_crs, file = crs_path_new)


