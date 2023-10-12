### ---------------
# 0. start loading data an set parameters
###
rm(list = ls())
source("Code/00. boot.R")
gc()
Sys.sleep(10)

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}
source <- "crs"
skip_icov <- T
# job_specific_suffix <- ""
job_specific_suffix <- "_full_"
path_input_crs <- paste0("./Data/intermediate/crs01_1", job_specific_suffix,year(Sys.Date()),  ".feather")
path_intermediate_crs_after_cleaning <- paste0("./Data/intermediate/crs02_int_clean_titles", job_specific_suffix,year(Sys.Date()),  ".feather")
path_output_crs <- paste0("./Data/intermediate/crs02", job_specific_suffix,year(Sys.Date()), ".feather")
start <- Sys.time()


## load the data file
print("Loading document:")
# df_crs_raw <- df_crs
# rm(df_crs)
df_crs_raw <- read_feather(path_input_crs)
print_time_diff(start)
# Time difference of 44.57935 secs


### ---------------
# 1. simplify the working dataset
###

## select columns
# every step, we try to use a subset of the data to make the process quicker
rm(crs_path)
# checking all the column names
names(df_crs_raw)
cols_needed <- c("process_id",
                 "projecttitle",
                 "shortdescription",
                 "longdescription",
                 "purposecode",
                 "donorname",
                 "gender", 
                 "channelcode",
                 "rmnch", 
                 "year")

df_crs_raw <- df_crs_raw %>%
  select(all_of(cols_needed))
beep()
# rm(cols_needed)


### ---------------
# 2. processing text data
###

source("code/02.0a identify desc2mine of each project.R")

### ---------------
# 3. Add markers from pp code and policy markers
###
start <- Sys.time()
df_crs <- df_crs %>%
  # mutate(text_id = as.numeric(as.factor(desc_2mine))) %>%
  ## add SCB identifier
  mutate(scb = ifelse(purposecode==16062,1,0),
         pop = ifelse(purposecode==13010,1,0),
         gen_ppcode = (purposecode %in% c(15170:15180)),
         gen_donor = (channelcode == 41146),
         gen_marker = (gender %in% c(1, 2) & (!is.na(gender))), 
         gen_marker1 = (gender == 1), 
         gen_marker2 = ( gender ==2)
         # , 
         # gen_rmnch = (rmnch ==1)|(rmnch==2), 
         # gen_rmnch1 = (rmnch == 1), 
         # gen_rmnch2 = (rmnch == 2)
  ) 

df_crs <- df_crs %>% 
  mutate(mining_ppcode = purposecode == 15250)

### ---------------
# 3.1 RMNCH analysis
###

source("code/02.0b fix issues in RMNCH mark.R")


### ---------------
# 3.2 language detection
###
df_crs %>%
  select(-cols_needed[which(!cols_needed %in% c("process_id", "longdescription"))])


df_crs <- df_crs  %>% 
  mutate(title_id = as.numeric(as.factor(projecttitle))) 
  

df_crs_lang <- df_crs %>%
  select(text_id, desc_2mine) %>%
  ## drop duplicate project descriptions
  filter(!duplicated(text_id)) %>%
  mutate(language = cld2::detect_language(desc_2mine)) %>%
  select(-desc_2mine)

df_crs_lang_title <- df_crs %>% 
  select(title_id, projecttitle) %>% 
  distinct %>% 
  mutate(language_title = cld2::detect_language(projecttitle)) %>% 
  select(-projecttitle)

# 
# df_crs_lang <- df_crs_lang %>% 
#   mutate(language_title = cld2::detect_language(projecttiitle)) 
# df_crs1 <- df_crs %>% 
#   mutate(language_title = cld2::detect_language(projecttiitle)) 

print("Detecting Lanugage")
print_time_diff(start)


names(df_crs_lang)
df_crs <- df_crs %>%
  select(-longdescription) %>%
  left_join(df_crs_lang) %>%
  left_join(df_crs_lang_title) %>%
  mutate(language_desc = language)


### ---------------
# 4 join with Raw data
###


vec_cols_2keep <- names(df_crs)[!names(df_crs) %in% names(df_crs_raw)]

df_crs <- df_crs %>%
  select(process_id, all_of(vec_cols_2keep)) %>%
  right_join(df_crs_raw)


rm(df_crs_lang)
rm(df_crs_lang_title)
rm(df_crs_raw)
print("rest of the analysis")
print_time_diff(start)
gc()
Sys.sleep(10)

print("NAs in desc_2mine")
which(is.na(df_crs$desc_2mine)|df_crs$desc_2mine == "") %>% length %>% print()
# which(df_crs$desc_2mine == "") %>% print
table(df_crs$language) %>% print
names(df_crs)
print_time_diff(start)
#Time difference of 283.4721 secs



#Output::
start <- Sys.time()
# saveRDS(df_crs, file=crs_path_new)
write_feather(df_crs, path_output_crs)
print("Save file:")
print_time_diff(start)
beep()
# Time difference of 14.25521 secs

