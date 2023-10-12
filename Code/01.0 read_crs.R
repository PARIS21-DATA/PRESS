## ----------
## this script should be only run once every update
## ----------
source("code/00. boot.R")
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
crs_zip_files <- crs_zip_files[grepl(".zip", crs_zip_files, fixed = T)]

if(length(list.files("./data/Raw/CRS/txt/"))==0)  lapply(crs_zip_files, unzip,overwrite = T, exdir = crs_txt_folder)
rm(crs_zip_folder, crs_zip_files)
print_time_diff(start) # Time difference of 52.25433 secs

crs_txt_files <- paste(crs_txt_folder, list.files(crs_txt_folder), sep = "/") 
crs_txt_files <- crs_txt_files[grepl(".txt", crs_txt_files, fixed = T)]

### ---------
# read txt
start <- Sys.time()
fun_read_csv <- function(var, enc ) {
  start_mini <- Sys.time()
  x <- read.csv(var,  sep = "|", header = T,
                stringsAsFactors = F, 
                # fileEncoding = "LATIN1" 
                fileEncoding = enc
                ) 
  print(var)
  print(paste0(nrow(x), " rows"))
  print_time_diff(start_mini)
  return(x)
}

# always test it beofre 
# 
# x <- read.csv(crs_txt_files[1],  
#               sep = "|", header = T,
#               stringsAsFactors = F, 
#               # fileEncoding = "LATIN1" 
#               fileEncoding = "UTF-16" 
# ) 
list_crs <- lapply( crs_txt_files, FUN = fun_read_csv , enc = "UTF-8")
print_time_diff(start)
#Time difference of 148.4077 secs

rm(fun_read_csv)
beep()
# Time difference of 187.5492 secs

lapply(list_crs, function(x) print(ncol(x)))
lapply(list_crs, function(x) print(nrow(x)))
start <- Sys.time()
df_crs <-  rbindlist(list_crs, fill = T)
print_time_diff(start)

# making basic changes
df_crs <- df_crs %>%
  mutate(source = "crs")
names(df_crs)<- tolower(names(df_crs))
df_crs$process_id <- 1:nrow(df_crs)

start <- Sys.time()
write_feather(df_crs, file =paste0("./Data/Raw/CRS/crs_full_", year(Sys.Date()),".feather") )
write_feather(df_crs, file =paste0("./Data/Raw/CRS/crs_full_", Sys.Date(),".feather") )
print_time_diff(start)
# Time difference of 33.25225 secs


# df_crs_sample <- df_crs[sample(nrow(df_crs),nrow(df_crs)/40 ), ]
# rm(df_crs)
# df_crs <- df_crs_sample
# saveRDS(df_crs, file  = "./Data/Raw/CRS/crs_sample.rds") 
beep()
# rm(list = ls())
gc()
