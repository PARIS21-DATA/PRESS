rm(list = ls())
gc()
path_data <- paste0("Data/Raw/CRS/crs_full_", year(Sys.Date()),".rds")
df_crs_raw <- readRDS(path_data)
df_crs <- df_crs_raw
beep()

# df_crs %>% select(projecttitle) %>% head(10)
infoRDS(path_data)
# iconv(df_crs$agencyname,from = "CP1252", to = "UTF-8") %>% unique
enc2utf8(df_crs$projecttitle) %>% unique %>% head(10)
df_crs$projecttitle %>% head(10)

convert_char_win2utf <- function(x) {
  # if(is.character(x)) {x <- iconv( x, from = "CP1252" , to = "utf-8")  }
  if(is.character(x)) {x <- enc2utf8(x) }
  return(x)
}

# is.character(df_crs_de$agencyname)

df_crs_ls <- df_crs %>% 
  lapply(convert_char_win2utf)
beep(2)

df_crs <- df_crs_ls %>% 
  data.frame(stringsAsFactors = F)

# checking if the characters are converted 
df_crs %>% filter(donorname %in% c("France","Spain", "Switzerland", "Germany")) %>%
  filter(longdescription != "") %>% 
  pull(longdescription) %>%
  sample() %>%
  head(100)

# saveRDS(df_crs, file = "Data/Raw/CRS/crs_utf8_full.rds", ascii = F)
write_rds(df_crs, file = "Data/Raw/CRS/crs_utf8_full.rds")
# note from web: 
# If you've used saveRDS() in the past, you will have no trouble using write_rds() . The only major difference between the two is that write_rds() does not compress the file by default. The sister function of write_rds() is read_rds() .
infoRDS("Data/Raw/CRS/crs_utf8_full.rds")
rm(df_crs_raw)
rm(df_crs_ls)

beep()
