### ---------------
# start data cleaning
###
# source("Code/00. boot.R")
rm(list = ls())
gc()
print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}
source <- "crs"
skip_icov <- T
job_specific_suffix <- ""
job_specific_suffix <- "_utf8_full"
crs_path <- paste0("./Data/intermediate/crs01_1", job_specific_suffix, ".rds")
crs_path_new <- paste0("./Data/intermediate/crs02", job_specific_suffix, ".rds")
start <- Sys.time()


## load the data file
df_crs_raw <- readRDS(crs_path)
print("Loading document:")
print_time_diff(start)

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
                 "channelcode")

df_crs_raw <- df_crs_raw %>%
  select(all_of(cols_needed))
beep()

## setting up a function to clean text
# adopting JA's function
clean_titles <- function(title){
  title <- title %>%
    removeNumbers %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    tolower
  return(title)
}

## if there is no long description, usee short description instead. 
df_crs_raw <- df_crs_raw %>% 
  mutate(longdescription = ifelse(is.na(longdescription), shortdescription, 
                                  ifelse(longdescription == "", shortdescription, 
                                         longdescription)))

## clean text columns
df_crs <- df_crs_raw %>%
  mutate(projecttitle = clean_titles(projecttitle),
         shortdescription = clean_titles(shortdescription),
         longdescription = clean_titles(longdescription))
print("Cleaning 3 text columns")
print_time_diff(start)
gc()
beep()
# for the whole crs data:
# Time difference of 148.8021 secs


## TO REVIVE afterwards
## if the long description is too similar to project title, remove it. 
max_string_dist <- 10

# first no need to detect if the character length difference is already large
# we only detect a subset of the data
df_crs_2detect4diff <- df_crs %>%
  filter((nchar(projecttitle) - nchar(longdescription) )^2 < 100) 

df_crs_2detect4diff <- df_crs_2detect4diff %>% 
  mutate(ldesc_id_tmp = as.numeric(as.factor(paste(projecttitle, longdescription))))

df_crs_2detect4diff_shorten <- df_crs_2detect4diff %>% 
  filter(!duplicated(ldesc_id_tmp))

# replace the longdesc with NA if it is too similar to project title. 
df_crs_2detect4diff_shorten <- df_crs_2detect4diff_shorten %>%
  mutate(desc_2mine = ifelse(stringdist(projecttitle, longdescription)< max_string_dist, NA, longdescription))

df_crs_2detect4diff_shorten <- df_crs_2detect4diff_shorten %>% 
  select(ldesc_id_tmp, desc_2mine)

df_crs_2detect4diff <- df_crs_2detect4diff %>% 
  left_join(df_crs_2detect4diff_shorten) %>% 
  select(-ldesc_id_tmp)

rm(df_crs_2detect4diff_shorten)

df_crs <- df_crs %>% 
  filter(!process_id %in% df_crs_2detect4diff$process_id) %>%
  mutate(desc_2mine = longdescription) %>%
  rbind(df_crs_2detect4diff) %>%
  mutate(text_id = as.numeric(as.factor(desc_2mine)))

gc()
print("find the best text for desc_2mine")
print_time_diff(start)


df_crs <- df_crs %>%
  # mutate(text_id = as.numeric(as.factor(desc_2mine))) %>%
  ## add SCB identifier
  mutate(scb = ifelse(purposecode==16062,1,0),
         pop = ifelse(purposecode==13010,1,0),
         gen_ppcode = (purposecode %in% c(15170:15180)),
         gen_donor = (channelcode == 41146),
         gen_marker = (gender %in% c(1, 2) & (!is.na(gender))), 
         gen_marker1 = (gender == 1), 
         gen_marker2 =( gender ==2)
  ) %>%
  select(-cols_needed[which(!cols_needed %in% c("process_id", "longdescription"))])

df_crs_lang <- df_crs %>%
  select(text_id, desc_2mine) %>%
  ## 1.b. drop duplicate project descriptions
  filter(!duplicated(text_id)) %>%
  mutate(language = cld2::detect_language(desc_2mine)) %>%
  select(-desc_2mine)

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
  left_join(df_crs_lang)

df_crs <- df_crs %>%
  right_join(df_crs_raw)

df_crs <- df_crs %>%
  mutate(language_title = cld2::detect_language(projecttitle)) 

rm(df_crs_lang)
rm(df_crs_raw)
print("rest of the analysis")
print_time_diff(start)


print("NAs in desc_2mine")
which(is.na(df_crs$desc_2mine)|df_crs$desc_2mine == "") %>% length %>% print()
# which(df_crs$desc_2mine == "") %>% print
table(df_crs$language) %>% print
names(df_crs)


#Output::
# saveRDS(df_crs, file=crs_path_new)
write_rds(df_crs, file=crs_path_new)

print("Save file:")
print_time_diff(start)

beep(3)
