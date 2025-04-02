setwd("~/dropbox/PARIS21/R/PRESS/")
rm(list = ls())
gc()

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

crs_zip_folder <-  "./Data/Raw/CRS/zip"
crs_txt_folder <-  "./Data/Raw/CRS/txt"

crs_zip_files <- paste(crs_zip_folder, list.files(crs_zip_folder), sep = "/")
# lapply(crs_zip_files, unzip,overwrite = T, exdir = crs_txt_folder)
rm(crs_zip_folder, crs_zip_files)

crs_txt_files <- paste(crs_txt_folder, list.files(crs_txt_folder), sep = "/")


list_crs <- lapply( crs_txt_files[1:2], read.csv , 
                    sep = "|", header = T, stringsAsFactors = F, encoding = "utf-8"
)
beep()


a =rbind(list_crs[[1]], list_crs[[2]])
a %>% filter(DonorName == "Germany") %>% .$AgencyName %>% unique

codepages <- setNames(iconvlist(), iconvlist())

start <- Sys.time()
encoding_test_read <- function(enc) {print(enc) 
  try(read.csv(crs_txt_files[1],
               sep = "|", 
               header = T, 
               fileEncoding = enc,
               nrows=100))
}
print(Sys.time()-start)


x <- lapply(codepages, encoding_test_read)
beep()
unique(do.call(rbind, sapply(x, dim)))

maybe_ok <- sapply(x, function(x) isTRUE(all.equal(dim(x), c(100,91))))
which(maybe_ok)
codepages[maybe_ok] 


rm(list_crs)
list_crs <- lapply( crs_txt_files[1:2], read.csv, 
                    sep = "|", header = T, stringsAsFactors = F, fileEncoding = "utf-8"
)
beep()


a =rbind(list_crs[[1]], list_crs[[2]])
a %>% filter(DonorName == "Germany") %>% .$AgencyName %>% unique
names(a)
