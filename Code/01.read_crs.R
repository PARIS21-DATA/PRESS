## ----------
## this script should be only run once every update
## ----------

rm(list = ls())
gc()

crs_zip_folder <-  "./Data/Raw/CRS/zip"
crs_txt_folder <-  "./Data/Raw/CRS/txt"

crs_zip_files <- paste(crs_zip_folder, list.files(crs_zip_folder), sep = "/")
lapply(crs_zip_files, unzip,overwrite = T, exdir = crs_txt_folder)
rm(crs_zip_folder, crs_zip_files)

crs_txt_files <- paste(crs_txt_folder, list.files(crs_txt_folder), sep = "/")

list_crs <- lapply( crs_txt_files, read.csv , sep = "|", header = T, stringsAsFactors = F, encoding = "utf-8" )
beep()

# df_crs = bind_rows(list_crs) # not successful
# crs_vars = lapply(list_crs, names)

# gc()
# start = Sys.time()
# if(exists("df_crs")) rm(df_crs)
# for (df in list_crs) {
#   if(!exists("df_crs")) df_crs = df else df_crs = rbind(df_crs, df)
# }
# difftime( Sys.time(),start, units = "sec")
# # Time difference of 269.2933 secs
# rm(df_crs, df)
# gc()

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
difftime(Sys.time(),start, units = "sec")
# Time difference of 86.15631 secs
beep(2)
save(df_crs, file  = "./Data/Raw/CRS/crs_full.rds") 
beep(2)
df_crs_sample <- df_crs[sample(nrow(df_crs),nrow(df_crs)/40 ), ]
rm(df_crs)
df_crs <- df_crs_sample
save(df_crs, file  = "./Data/Raw/CRS/crs_sample.rds") 

rm(list = ls())
gc()
