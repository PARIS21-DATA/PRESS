### ---------------
# start data cleaning 
### 
rm(list = ls())
source <- "crs"
crs_path <- "./Data/intermediate/crs01_1.rds"
crs_path_new <- "./Data/intermediate/crs02.rds"
df_crs_raw <- readRDS(crs_path)
rm(crs_path)

cols_needed <- c("process_id", 
                 "projecttitle", 
                 "shortdescription", 
                 "longdescription", 
                 "purposecode")


# every step, we try to use a subset of the data to make the process quicker
df_crs_raw <- df_crs_raw %>%
  select(cols_needed) 

df_crs <- df_crs_raw %>%
  mutate(description_comb = paste(projecttitle, 
                                  shortdescription, 
                                  longdescription, 
                                  sep=". "), 
         description_comb = tolower(description_comb)
         ) %>%
  mutate(text_id = as.numeric(as.factor(description_comb))) %>%
  ## add SCB identifier
  mutate(scb = ifelse(purposecode==16062,1,0), 
         pop = ifelse(purposecode==13010,1,0),
         gen_ppcode = ifelse(purposecode %in% c(15170:15180), 1, 0)
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

#Output::
saveRDS(df_crs, file=crs_path_new)

