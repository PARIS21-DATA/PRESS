rm(crs)

########
# read IATI data
########
iati_folder_path = "data/raw/IATI/"
iati_file_path = paste0(iati_folder_path,"activity.csv")
iati_rds_path = paste0(iati_folder_path,"iati_2021.rds")
#??? to be documented: the IATI projects
iati = read_csv(iati_file_path)  
save(iati, file = iati_rds_path) 
