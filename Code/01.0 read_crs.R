################################################################################
#
# Reading in data from CRS data base
# Author: Yu Tian, Johannes Abele
# Date: 05/10/2022
#
# Objective: Load all available CRS data from OECD repository https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1 
#            and save as .rds file
# 
# input files: - Data/Raw/CRS/zip/*.zip
#              - Data/Raw/CRS/txt/*.txt
#              - 
#              
#
# output file: - /Data/Raw/CRS/crs_sample.rds
#
#
################################################################################


# ------------------------------- Preparation ----------------------------------

rm(list = ls())

# Load packages
source("./Code/00. boot.R")

# Paths to raw data
crs_zip_folder <-  "./Data/Raw/CRS/zip"
crs_txt_folder <-  "./Data/Raw/CRS/txt"


#---------------------------- txt file processing ------------------------------

# Extract .zip files that were prviously downloaded into txt folder
crs_zip_files <- paste(crs_zip_folder, list.files(crs_zip_folder), sep = "/")
lapply(crs_zip_files, unzip, overwrite = T, exdir = crs_txt_folder)
rm(crs_zip_folder, crs_zip_files)

# Add directory names to .txt files
crs_txt_files <- paste(crs_txt_folder, list.files(crs_txt_folder), sep = "/")

# Read crs data from .txt files and store each as an entry of list_crs
list_crs <- lapply(crs_txt_files, read.csv , sep = "|", header = T, stringsAsFactors = F, encoding = "utf-8")
beep()

# Merge all crs from different years into one data frame
start <- Sys.time()
df_crs <-  rbind(list_crs[[1]], 
               list_crs[[2]], 
               list_crs[[3]], 
               list_crs[[4]], 
               list_crs[[5]], 
               list_crs[[6]], 
               list_crs[[7]], 
               list_crs[[8]], 
               list_crs[[9]], 
               list_crs[[10]], 
               list_crs[[11]], 
               list_crs[[12]], 
               list_crs[[13]], 
               list_crs[[14]] 
)

# Time difference of 86.15631 secs
difftime(Sys.time(),start, units = "sec")
beep(2)

# making basic changes
df_crs <- df_crs %>%
  mutate(source = "crs")
names(df_crs)<- tolower(names(df_crs))
row.names(df_crs) <- c()
df_crs$process_id <- row.names(df_crs)

saveRDS(df_crs, file  = "./Data/Raw/CRS/crs_full.rds")
beep(2)

# If full data available in Data/Raw/, uncomment to load 
#df_crs <- readRDS("./Data/Raw/CRS/crs_full.rds")

# Take a sample of the entire data frame for further testing 
df_crs_sample <- df_crs[sample(nrow(df_crs),nrow(df_crs)/10), ]
rm(df_crs)

df_crs <- df_crs_sample
saveRDS(df_crs, file  = "./Data/Raw/CRS/crs_sample.rds") 
gc()
