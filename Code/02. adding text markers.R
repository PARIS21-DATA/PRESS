### ---------------
# start data cleaning 
### 
rm(list = ls())
source <- "crs"
crs_path <- "./Data/intermediate/crs01_1.rds"
crs_path_new <- "./Data/intermediate/crs02.rds"
df_crs_raw <- readRDS(crs_path)
rm(crs_path)
names(df_crs_raw)
cols_needed <- c("process_id", 
                 "projecttitle", 
                 "shortdescription", 
                 "longdescription", 
                 "purposecode", 
                 "donorname", 
                 "gender")


# every step, we try to use a subset of the data to make the process quicker
df_crs_raw <- df_crs_raw %>%
  select(all_of(cols_needed)) 

# only needed when working on a Mac. Also not working well becausee it will convert utf8 to NA
df_crs_raw = df_crs_raw %>%
  mutate(projecttitle = iconv(projecttitle, "WINDOWS-1252", "UTF-8"),
         shortdescription = iconv(shortdescription, "WINDOWS-1252", "UTF-8"),
         longdescription= iconv(longdescription, "WINDOWS-1252", "UTF-8"))

df_crs <- df_crs_raw %>%
  mutate(description_comb = paste(projecttitle, 
                                  shortdescription, 
                                  longdescription, 
                                  sep=". "), 
         description_comb = tolower(description_comb)
         ) 

# df_crs$description_comb[140:150]
# df_crs_raw$longdescription[145]
# a$longdescription[145]
# df_crs_raw$longdescription[145]


df_crs <- df_crs %>%
  mutate(text_id = as.numeric(as.factor(description_comb))) %>%
  ## add SCB identifier
  mutate(scb = ifelse(purposecode==16062,1,0), 
         pop = ifelse(purposecode==13010,1,0),
         gen_ppcode = (purposecode %in% c(15170:15180)), 
         gen_donor = (donorname == "UN Women"), 
         gen_marker = (gender %in% c(1, 2) & (!is.na(gender)))
         ) %>%
  select(-cols_needed[which(!cols_needed %in% c("process_id", "longdescription"))])




df_crs_lang <- df_crs %>% 
  select(text_id, longdescription) %>%
  ## 1.b. drop duplicate project descriptions
  filter(!duplicated(text_id)) %>%
  mutate(language = cld2::detect_language(longdescription)) %>%
  select(-longdescription) 


df_crs <- df_crs %>% 
  select(-longdescription) %>%
  left_join(df_crs_lang) 

df_crs <- df_crs %>%
  right_join(df_crs_raw)

rm(df_crs_lang)
rm(df_crs_raw)


which(is.na(df_crs$description_comb)) %>% print 
which(df_crs$description_comb == "") %>% print
table(df_crs$language) %>% print 
names(df_crs)
#Output::
saveRDS(df_crs, file=crs_path_new)


