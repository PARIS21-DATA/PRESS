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

# adopting JA's function
clean_titles <- function(title){
  title <- title %>% 
    removeNumbers %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    tolower
  return(title)
}

df_crs <- df_crs_raw %>%
  mutate(desc_2mine = paste(projecttitle, 
                                  shortdescription, 
                                  longdescription, 
                                  sep=". "), 
         desc_2mine = tolower(desc_2mine)
         ) 

max_string_dist <- 10
df_crs <- df_crs_raw %>%
  mutate(projecttitle = clean_titles(projecttitle),
         shortdescription = clean_titles(shortdescription),
         longdescription = clean_titles(longdescription)) %>%
  mutate(desc_2mine = ifelse(stringdist(projecttitle, longdescription)< max_string_dist, NA, longdescription)) %>%
  mutate(text_id = as.numeric(as.factor(desc_2mine)))
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


which(is.na(df_crs$desc_2mine)) %>% print 
which(df_crs$desc_2mine == "") %>% print
table(df_crs$language) %>% print 
names(df_crs)
#Output::
saveRDS(df_crs, file=crs_path_new)


