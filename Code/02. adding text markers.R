### ---------------
# start data cleaning
###
source("Code/00. boot.R")
rm(list = ls())
gc()
print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}
source <- "crs"
skip_icov <- T
job_specific_suffix <- ""
job_specific_suffix <- "_de"
crs_path <- paste0("./Data/intermediate/crs01_1", job_specific_suffix, ".rds")
crs_path_new <- paste0("./Data/intermediate/crs02", job_specific_suffix, ".rds")
start <- Sys.time()

df_crs_raw <- readRDS(crs_path)
print("Loading document:")
print_time_diff(start)

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
beep()
# only needed when working on a Mac. Also not working well becausee it will convert utf8 to NA

if(!skip_icov) {df_crs_raw = df_crs_raw %>%
  mutate(projecttitle = iconv(projecttitle, "WINDOWS-1252", "UTF-8"),
         shortdescription = iconv(shortdescription, "WINDOWS-1252", "UTF-8"),
         longdescription= iconv(longdescription, "WINDOWS-1252", "UTF-8"))}


# adopting JA's function

clean_titles <- function(title){
  title <- title %>%
    removeNumbers %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    tolower
  return(title)
}



df_crs_raw <- df_crs_raw %>% 
  mutate(longdescription = ifelse(is.na(longdescription), shortdescription, 
                                  ifelse(longdescription == "", shortdescription, 
                                         longdescription)))

max_string_dist <- 10
df_crs <- df_crs_raw %>%
  mutate(projecttitle = clean_titles(projecttitle),
         shortdescription = clean_titles(shortdescription),
         longdescription = clean_titles(longdescription))
print("Cleaning 3 text columns")

saveRDS(df_crs, "./Data/intermediate/crs02_de_cleaned.rds")

print_time_diff(start)
gc()

df_crs <- df_crs %>%
  mutate(desc_2mine = ifelse(stringdist(projecttitle, longdescription)< max_string_dist, NA, longdescription)) %>%
  mutate(text_id = as.numeric(as.factor(desc_2mine)))


saveRDS(df_crs, "./Data/intermediate/crs02_de_desc.rds")
gc()
print("find the best text for desc_2mine")
print_time_diff(start)


# mutate(desc_2mine = ifelse(stringdist(projecttitle, shortdescription) < max_string_dist,
#                                  projecttitle,
#                                  paste(projecttitle, shortdescription, sep = ". "))) %>%
# mutate(desc_2mine = ifelse(stringdist(desc_2mine, longdescription) < max_string_dist,
#                                  desc_2mine,
#                                  paste(desc_2mine, longdescription, sep = ". "))) %>%
# mutate(string_dist_title_short = stringdist(tolower(projecttitle), tolower(shortdescription))) %>%
# mutate(text_id = as.numeric(as.factor(desc_2mine)))


# df_crs$desc_2mine[140:150]
# df_crs_raw$longdescription[145]
# a$longdescription[145]
# df_crs_raw$longdescription[145]


df_crs <- df_crs %>%
  # mutate(text_id = as.numeric(as.factor(desc_2mine))) %>%
  ## add SCB identifier
  mutate(scb = ifelse(purposecode==16062,1,0),
         pop = ifelse(purposecode==13010,1,0),
         gen_ppcode = (purposecode %in% c(15170:15180)),
         gen_donor = (donorname == "UN Women"),
         gen_marker = (gender %in% c(1, 2) & (!is.na(gender))), 
         gen_marker1 = gender == 1, 
         gen_marker2 = gender ==2
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
saveRDS(df_crs, file=crs_path_new)
print("Save file:")
print_time_diff(start)
beep()
