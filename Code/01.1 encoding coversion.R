rm(list = ls())
gc()
df_crs <- readRDS("Data/Raw/CRS/crs_full.rds")

df_crs_de <- df_crs %>% 
  filter(donorname == "Germany")
beep()
# df_crs %>% select(year) %>% table()
df_crs_de %>% select(agencyname) %>% unique
infoRDS("Data/Raw/CRS/crs_full.rds")
iconv(df_crs_de$agencyname,from = "CP1252", to = "UTF-8") %>% unique

convert_char_win2utf <- function(x) {
  if(is.character(x)) {x <- iconv( x, from = "CP1252" , to = "utf-8")  }
  return(x)
}

is.character(df_crs_de$agencyname)

df_crs_de_ls <- df_crs_de %>% 
  lapply(convert_char_win2utf)


df_crs_de <- df_crs_de_ls %>% 
  data.frame(stringsAsFactors = F)

df_crs_de$agencyname %>% unique

saveRDS(df_crs_de, file = "Data/Raw/CRS/crs_de.rds", ascii = F)
infoRDS("Data/Raw/CRS/crs_de.rds")

df_crs_de <- readRDS("Data/Raw/CRS/crs_de.rds")
