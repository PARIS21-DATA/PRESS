## ----------
## Run to test the encoding of files
## ----------
source("code/00. boot.R")
rm(list = ls())
gc()

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

source("code/00.5a function reading csv.R")
source("code/00.5b function summarise dimensions.R")


crs_txt_folder <-  "./Data/Raw/CRS/txt"

crs_txt_files <- paste(crs_txt_folder, list.files(crs_txt_folder), sep = "/")
crs_txt_files <- crs_txt_files[grepl(".txt", crs_txt_files, fixed = T)]

codepages <- setNames(iconvlist(), iconvlist())

ls_possible <- list()
ls_possible[[1]] <- codepages
# codepages[grep("16", codepages, T)]
# codepages[grep("utf", codepages, T)]
# codepages[grep("uni", codepages, T)]
# ls_possible[[1]] <- codepages[c(1:50,52:419)]

start <- Sys.time()
input_file <- crs_text_files
# input_file <- crs_text_files[11]
# input_file <- "~/downloads/CRS 2021 data.txt"
n_rows <- 10

x <- lapply(ls_possible[[length(ls_possible)]],
            FUN = fun_testing_read,
            n_rows = n_rows,
            input_file = input_file) # you get lots of errors/warning here

dim_summary <- fun_summarise_dimension(x)


# change here:
n_cols <- 93

ls_possible[[1+length(ls_possible)]] <- dim_summary %>%
  filter(rows == n_rows,
         cols == 93) %>%
  select(code) %>%
  .$code

vec_possible <- ls_possible[[length(ls_possible)]]

vec_possible[grepl("1251", vec_possible, ignore.case = T)]
#iso-8859-1
#iso-8859-15
#latin1
#Windows-1251
#CP-1251


df_strings_test <- read.csv(crs_txt_files[11], fileEncoding = "LATIN1", sep = "|",
              stringsAsFactors = F)


df_strings_test %>%
  filter(DonorName %in% c("Austria",
                          "Germany",
                          "France",
                          "Spain")) %>%
  select(ProjectTitle) %>%
  unique %>%
  head
