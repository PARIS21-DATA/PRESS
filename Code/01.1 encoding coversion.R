rm(list = ls())
gc()
df_crs_raw <- readRDS("Data/Raw/CRS/crs_full.rds")
df_crs <- df_crs_raw # %>%
#   # filter(donorname == "UN Women")
#   filter(channelcode == 41146)
# a = df_crs_raw$agencyname %>% unique
# grepl("women", a, ignore.case = T) %>% which

beep()
# df_crs %>% select(year) %>% table()
# df_crs_de %>% select(agencyname) %>% unique
df_crs %>% select(projecttitle) %>% head(10)
infoRDS("Data/Raw/CRS/crs_full.rds")
# iconv(df_crs$agencyname,from = "CP1252", to = "UTF-8") %>% unique
enc2utf8(df_crs$projecttitle) %>% head(10)



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

# df_crs$agencyname %>% unique
saveRDS(df_crs, file = "Data/Raw/CRS/crs_utf8_full.rds", ascii = F)
write_rds(df_crs, file = "Data/Raw/CRS/crs_utf8_full.rds")
rm(df_crs_raw)
rm(df_crs_ls)
# infoRDS("Data/Raw/CRS/crs_de.rds")
beep()
# df_crs_de <- readRDS("Data/Raw/CRS/crs_de.rds")
