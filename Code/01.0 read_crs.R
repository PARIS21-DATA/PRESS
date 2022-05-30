## ----------
## this script should be only run once every update
## ----------

rm(list = ls())
gc()

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

crs_zip_folder <-  "./Data/Raw/CRS/zip"
crs_txt_folder <-  "./Data/Raw/CRS/txt"

### ---------
# Extract txt from zips
start <- Sys.time()
crs_zip_files <- paste(crs_zip_folder, list.files(crs_zip_folder), sep = "/")

if(length(list.files("./data/Raw/CRS/txt/"))==0)  lapply(crs_zip_files, unzip,overwrite = T, exdir = crs_txt_folder)
rm(crs_zip_folder, crs_zip_files)
print_time_diff(start) # Time difference of 52.25433 secs


crs_txt_files <- paste(crs_txt_folder, list.files(crs_txt_folder), sep = "/")


### ---------
# read txt
start <- Sys.time()
list_crs <- lapply( crs_txt_files, read.csv , sep = "|", header = T, stringsAsFactors = F, encoding = "utf-8" )
beep()
print_time_diff(start)
# Time difference of 187.5492 secs


lapply(list_crs, function(x) print(ncol(x)))
lapply(list_crs, function(x) print(nrow(x)))
start <- Sys.time()
df_crs <-  rbindlist(list_crs, fill = T)
print_time_diff(start)

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




# making basic changes
df_crs <- df_crs %>%
  mutate(source = "crs")
names(df_crs)<- tolower(names(df_crs))
df_crs$process_id <- 1:nrow(df_crs)

start <- Sys.time()
readr::write_rds(df_crs, file ="./Data/Raw/CRS/crs_full1.rds" )
print_time_diff(start)
# Time difference of 64.25225 secs
start <- Sys.time()

saveRDS(df_crs, file  = "./Data/Raw/CRS/crs_full.rds")
beep(2)
print_time_diff(start)
# Time difference of 161.5576 secs

df_crs_sample <- df_crs[sample(nrow(df_crs),nrow(df_crs)/40 ), ]
rm(df_crs)
df_crs <- df_crs_sample
saveRDS(df_crs, file  = "./Data/Raw/CRS/crs_sample.rds") 
beep()
rm(list = ls())
gc()
