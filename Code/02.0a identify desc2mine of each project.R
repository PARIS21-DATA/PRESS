

## 2.1 setting up a function to clean text
# adopting JA's function
clean_titles <- function(title){
  title <- title %>%
    removeNumbers %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    tolower
  return(title)
}

## 2.2 if there is no long description, use short description instead. 
df_crs_raw <- df_crs_raw %>% 
  mutate(longdescription = ifelse(is.na(longdescription), shortdescription, 
                                  ifelse(longdescription == "", shortdescription, 
                                         longdescription)))

## 2.3 clean text columns
df_crs <- df_crs_raw %>%
  mutate(projecttitle = clean_titles(projecttitle),
         shortdescription = clean_titles(shortdescription),
         longdescription = clean_titles(longdescription))
df_crs %>% 
  write_feather(crs_path_intermediate_after_cleaning )
print("Cleaning 3 text columns")
print_time_diff(start)
gc()
beep()
# for the whole crs data:
# Time difference of 148.8021 secs


## 2.4 reduce longdesc by removing the ones too similar to titles
## TO REVIVE afterwards
## if the long description is too similar to project title, remove it. 
max_string_dist <- 10
# first no need to detect if the character length difference is already large
# we only detect the string distance of a subset of the data
df_crs_2detect4diff <- df_crs %>%
  select(projecttitle, longdescription, process_id) %>% 
  filter((nchar(projecttitle) - nchar(longdescription) )^2 < 900) 

df_crs_2detect4diff <- df_crs_2detect4diff %>% 
  mutate(ldesc_id_tmp = as.numeric(as.factor(paste(projecttitle, longdescription))))
# df_crs_2detect4diff <- df_crs_2detect4diff %>% 
# filter(!duplicated(projecttitle, longdescription))
# mutate(ldesc_id_tmp = as.numeric(as.factor(paste(projecttitle, longdescription))))

# df_crs_2detect4diff_shorten <- df_crs_2detect4diff %>% 
# filter(!duplicated(ldesc_id_tmp))

# replace the longdesc with NA if it is too similar to project title. 
df_crs_2detect4diff_shorten <- df_crs_2detect4diff %>%
  select(ldesc_id_tmp, projecttitle, longdescription) %>% 
  distinct %>% 
  mutate(desc_2mine = ifelse(stringdist(projecttitle, longdescription)< max_string_dist, NA, longdescription))  %>% 
  select(ldesc_id_tmp, desc_2mine) %>% 
  distinct 

df_crs_2detect4diff <- df_crs_2detect4diff %>% 
  left_join(df_crs_2detect4diff_shorten) %>% 
  select(process_id, desc_2mine) 

df_crs_desc2mine <- df_crs %>% 
  mutate(desc_2mine = longdescription) %>% 
  select(process_id, desc_2mine) %>% 
  filter(!(process_id %in% df_crs_2detect4diff$process_id)) %>% 
  rbind(df_crs_2detect4diff) %>% 
  mutate(desc_2mine_id = as.numeric(as.factor(desc_2mine)))

names(df_crs_2detect4diff)

names(df_crs_2detect4diff_shorten)
rm(df_crs_2detect4diff_shorten)
rm(df_crs_2detect4diff)

df_crs <- df_crs %>%
  left_join(df_crs_desc2mine) %>% 
  # filter(!process_id %in% df_crs_2detect4diff$process_id) %>%
  # mutate(desc_2mine = longdescription) %>%
  # rbind(df_crs_2detect4diff) %>% 
  mutate(text_id = desc_2mine_id)

gc()
print("find the best text for desc_2mine")
print_time_diff(start)
